{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
module CodeGen where

import           Allocate
import           Compiler.Hoopl      hiding ((<*>))
import           Control.Lens
import           Control.Monad.State
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           Node
import           Numeric.Natural
import           Source
import           SymTab
import           Target
import           Token

data CG = CG {
  _sym     :: SymTab,
  _descr   :: M.Map Reg (S.Set Id),
  _resolve :: Label -> Imm,
  _code    :: [Target]
}

makeLenses ''CG

type CGM = State CG

codeGen :: (Intermediate, SymTab) -> [Target]
codeGen (inter, symTab) = result
  where
    (result, reslv) = resolveLabels (evalState (cgGraph inter) initialState)
    initialState = CG {
      _sym = symTab,
      _descr = M.fromList [(reg, S.empty) | reg <- [R1 .. R9]],
      _resolve = reslv,
      _code = []
      }

cgGraph :: Intermediate -> CGM [Either Label Target]
cgGraph GNil = return []
cgGraph (GUnit block) = cgBlock block
cgGraph (GMany (JustO entry) body (JustO exit)) = do
  entryCode <- cgBlock entry
  let linearBody = postorder_dfs_from body (successors entry)
  bodyCode <- concat <$> mapM cgBlock linearBody
  exitCode <- cgBlock exit
  return (entryCode ++ bodyCode ++ exitCode)

cgBlock :: Block Node e x -> CGM [Either Label Target]
cgBlock block = do
  alive <- uses sym (M.keysSet . M.filter (\x -> x^.kind /= Temp))
  let exitUses = getExitUses exit
      nodes' = getUses (reverse nodes) exitUses alive []
  cg nodes' alive
  cgExit exit alive
  code <- flush
  return $ case entry of
    NothingC -> map Right code
    JustC e -> Left (entryLabel e) : map Right code
  where
    (entry, middle, exit) = blockSplitAny block
    nodes = blockToList middle

flush :: CGM [Target]
flush = do
  cg <- get
  zoom (sym.traverse) $ do
    regs .= S.empty
    memo <- use memory
    addrs .= maybe S.empty S.singleton memo
  code .= []
  return (reverse (cg^.code))

regBind :: Reg -> Id -> CGM ()
regBind reg var = do
  sym.ix var.regs.icontains reg .= True
  descr.ix reg.icontains var .= True

regShare :: Reg -> Id -> S.Set Id -> S.Set Id -> CGM ()
regShare reg var used alive = do
  zoom (sym.ix var) $ do
    regs .= S.singleton reg
    addrs .= S.empty
  zoom descr $ do
    traverse.icontains var .= False
    ix reg.icontains var .= True
  saveOnLastUse reg (VarRV var) used alive

regRebind :: Reg -> Id -> S.Set Id -> S.Set Id -> CGM ()
regRebind reg var used alive = do
  sym.traverse.regs.icontains reg .= False
  regShare reg var used alive

regUnbind :: Reg -> Id -> CGM ()
regUnbind reg var =
  sym.ix var.regs.icontains reg .= False

getLoc :: RValue -> [Reg] -> CGM Reg
getLoc (LitRV lit) locked = do
  reg <- getEmptyRegister locked
  cgConst reg lit
  return reg
getLoc (VarRV var) locked = do
  entry <- use $ sym.ix var
  let registers = entry^.regs
      addresses = entry^.addrs
  if S.null registers && S.null addresses then do
    reg <- getEmptyRegister locked
    regBind reg var
    return reg
  else if S.null registers && not (S.null addresses) then do
    let bestAddress = minimumBy (compare `on` costOf) addresses
    reg <- getEmptyRegister locked
    cgConst reg bestAddress
    emit (LOAD reg (Ind reg))
    regBind reg var
    return reg
  else do
    dscr <- gets _descr
    return (minimumBy (compare `on` (S.size . (dscr M.!))) registers)

getMem :: Id -> CGM (Maybe Natural)
getMem var = _memory <$> use (sym.ix var)

saveWith :: Reg -> Id -> Natural -> CGM ()
saveWith reg var mem = do
  sym.ix var.addrs.icontains mem .= True
  cgConst R0 mem
  emit (STORE reg (Ind R0))

saveOnLastUse :: Reg -> RValue -> S.Set Id -> S.Set Id -> CGM ()
saveOnLastUse reg (LitRV _) used alive = return ()
saveOnLastUse reg (VarRV var) used alive =
  unless (var `S.member` used) $ do
    descr.traverse.icontains var .= False
    zoom (sym.ix var) $ do
      regs .= S.empty
      addrs .= S.empty
    when (var `S.member` alive) $ do
      Just mem <- getMem var
      addresses <- _addrs <$> use (sym.ix var)
      unless (mem `S.member` addresses) $
        saveWith reg var mem

save :: Id -> CGM ()
save var = do
  memo <- getMem var
  case memo of
    Nothing ->
      error "save"
    Just mem -> do
      registers <- _regs <$> use (sym.ix var)
      unless (S.null registers) $ do
        let reg = S.findMin registers
        saveWith reg var mem

getEmptyRegister :: [Reg] -> CGM Reg
getEmptyRegister locked = do
  (nulls, fulls) <- uses descr $
    M.partition S.null . M.filterWithKey (\k _ -> k `notElem` locked)
  if M.null nulls then do
    let comp = compare `on` S.size . snd
        (reg, vars) = minimumBy comp $ M.toList fulls
    vars `forM_` \var -> do
      save var
      regUnbind reg var
    return reg
  else
    return (fst $ M.findMin nulls)

getExitUses :: MaybeC x (Node O C) -> S.Set Id
getExitUses NothingC = S.empty
getExitUses (JustC (Branch _)) = S.empty
getExitUses (JustC (Czero x _ _)) = S.singleton x
getExitUses (JustC (Codd x _ _)) = S.singleton x
getExitUses (JustC _) = error "getExitUses"

getUses :: [Node O O] -> S.Set Id -> S.Set Id
  -> [(Node O O, S.Set Id)] -> [(Node O O, S.Set Id)]
getUses (node : nodes) used alive acc = case node of
  Move x y | VarRV x == y -> getUses nodes used alive acc
  Move x y -> go x (tryInsert y (S.delete x used))
  Binop x o y z -> go x (tryInsert y (tryInsert z (S.delete x used)))
  Reset x -> go x (S.delete x used)
  Inc x -> go x used
  Dec x -> go x used
  Shl x -> go x used
  Shr x -> go x used
  Load x y z -> go x (tryInsert z (S.delete x used))
  Store x y z -> go' (tryInsert y (tryInsert z used))
  Read x -> go x (S.delete x used)
  Write x -> go' (tryInsert x used)
  where
    go' u = getUses nodes u alive ((node, used) : acc)
    go x u | x `S.member` alive = go' u
           | x `S.member` used = go' u
           | otherwise = getUses nodes used alive acc
getUses [] _ _ acc = acc

tryInsert :: RValue -> S.Set Id -> S.Set Id
tryInsert (LitRV _) = id
tryInsert (VarRV x) = S.insert x

cgExit :: MaybeC x (Node O C) -> S.Set Id -> CGM ()
cgExit NothingC alive = return ()
cgExit (JustC node) alive = do
  res <- use resolve
  case node of
    Branch l -> emit (JUMP (res l))
    Czero x l1 l2 -> do
      x' <- getLoc (VarRV x) []
      saveOnLastUse x' (VarRV x) S.empty alive
      emit (JZERO x' (res l1))
      emit (JUMP (res l2))
    Codd x l1 l2 -> do
      x' <- getLoc (VarRV x) []
      saveOnLastUse x' (VarRV x) S.empty alive
      emit (JODD x' (res l1))
      emit (JUMP (res l2))

cg :: [(Node O O, S.Set Id)] -> S.Set Id -> CGM ()
cg ((node, used) : nodes) alive = case node of
  Move x y -> do
    y' <- getLoc y []
    saveOnLastUse y' y used alive
    regShare y' x used alive
    cg nodes alive
  Binop x o y z -> do
    y' <- getLoc y []
    z' <- getLoc z [y']
    unless (VarRV x == y) $ saveOnLastUse y' y used alive
    saveOnLastUse z' z used alive
    emit (op y' z')
    regRebind y' x used alive
    cg nodes alive
    where
      op = case o of
        Add -> ADD
        Sub -> SUB
        _ -> error "cg: mul/div/mod"
  Reset x -> do
    x' <- getEmptyRegister []
    emit (RESET x')
    regRebind x' x used alive
    cg nodes alive
  Inc x -> do
    x' <- getLoc (VarRV x) []
    emit (INC x')
    regRebind x' x used alive
    cg nodes alive
  Dec x -> do
    x' <- getLoc (VarRV x) []
    emit (DEC x')
    regRebind x' x used alive
    cg nodes alive
  Shl x -> do
    x' <- getLoc (VarRV x) []
    emit (SHL x')
    regRebind x' x used alive
    cg nodes alive
  Shr x -> do
    x' <- getLoc (VarRV x) []
    emit (SHR x')
    regRebind x' x used alive
    cg nodes alive
  Load x y zz -> do
    (mem, z') <- case zz of
      VarRV z -> do
        Just mem <- getMem y
        z' <- getLoc zz []
        saveOnLastUse z' zz used alive
        cgConst R0 mem
        emit (ADD z' R0)
        return (mem, z')
      LitRV z -> do
        Just mem <- getMem y
        z' <- getLoc (LitRV (mem + z)) []
        return (mem, z')
    emit (LOAD z' (Ind z'))
    regRebind z' x used alive
    cg nodes alive
  Store x y z -> do
    Just mem <- getMem x
    z' <- getLoc z []
    saveOnLastUse z' z used alive
    case y of
      LitRV y ->
        cgConst R0 (mem + y)
      VarRV y -> do
        y' <- getLoc (VarRV y) [z']
        saveOnLastUse y' (VarRV y) used alive
        cgConst R0 mem
        emit (ADD R0 y')
    emit (STORE z' (Ind R0))
    cg nodes alive
  Read x -> do
    x' <- getEmptyRegister []
    emit (READ x')
    regRebind x' x used alive
    cg nodes alive
  Write x -> do
    x' <- getLoc x []
    emit (WRITE x')
    saveOnLastUse x' x used alive
    cg nodes alive
cg [] _ = return ()

emit :: Target -> CGM ()
emit tg = code %= (tg :)

cgConst :: Reg -> Natural -> CGM ()
cgConst r 0 = emit (RESET r)
cgConst r x = emit (RESET r) >> emit (INC r) >> go x []
  where
    go 1 acc = mapM_ emit acc
    go x acc = case divMod x 2 of
      (d, 0) -> go d (SHL r : acc)
      (d, _) -> go d (SHL r : INC r : acc)

resolveLabels :: [Either Label Target] -> ([Target], Label -> Imm)
resolveLabels = go 0 [] (mapEmpty :: LabelMap Imm)
  where
    go line code labels [] = (foldl (flip (:)) [HALT] code, fromJust . flip mapLookup labels)
    go line code labels (Left lbl : xs) = go line code (mapInsert lbl (Imm line) labels) xs
    go line code labels (Right x : xs) = go (line + 1) (x : code) labels xs
