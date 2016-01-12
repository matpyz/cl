{-# LANGUAGE GADTs #-}
module CodeGen where

import           Allocate
import           Compiler.Hoopl      hiding ((<*>))
import           Control.Arrow
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
  sym     :: SymTab,
  regs    :: M.Map Reg (S.Set Id),
  resolve :: Label -> Imm,
  code    :: [Target]
}

type CGM = State CG

codeGen :: (Intermediate, SymTab) -> [Target]
codeGen (inter, symTab) = result
  where
    (result, reslv) = resolveLabels (evalState (cgGraph inter) initialState)
    initialState = CG {
      sym = symTab,
      regs = M.fromList [(reg, S.empty) | reg <- [R1 .. R9]],
      resolve = reslv,
      code = []
      }

cgGraph :: Intermediate -> CGM [Either Label Target]
cgGraph GNil = return []
cgGraph (GUnit block) = cgBlock block
cgGraph (GMany (JustO entry) body (JustO exit)) = do
  entryCode <- cgBlock entry
  let linearBody = postorder_dfs_from body (successors entry)
  bodyCode <- concat <$> mapM cgBlock linearBody
  exitCode <- cgBlock exit
  return (entryCode ++ bodyCode ++ exitCode ++ [Right HALT])

cgBlock :: Block Node e x -> CGM [Either Label Target]
cgBlock block = do
  alive <- gets (M.keysSet . M.filter (\x -> kind x /= Temp) . sym)
  let exitUses = getExitUses exit alive
      nodes' = getUses (reverse nodes) exitUses []
  cg nodes'
  cgExit exit alive
  code <- flush
  return $ case entry of
    NothingC -> map Right code
    JustC e -> Left (entryLabel e) : map Right code
  where
    (entry, middle, exit) = blockSplitAny block
    nodes = blockToList middle

flush :: CGM [Target]
flush = state (\cg -> (reverse (code cg), cg {
  sym = M.map (\s -> s { addr = (S.empty, maybe S.empty S.singleton (memo s)) }) (sym cg),
  code = []
}))

bindRegister :: Reg -> Id -> CGM ()
bindRegister reg var = modify (\cg -> cg {
  sym = adjust (setAddr $ first (S.insert reg)) var (sym cg),
  regs = M.insertWith S.union reg (S.singleton var) (regs cg)
})

rebindRegister :: Reg -> Id -> CGM ()
rebindRegister reg var = modify (\cg -> cg {
  sym = adjust (setAddr $ \(_, _) ->
    (S.singleton reg, S.empty)) var (sym cg),
  regs = M.insert reg (S.singleton var) $ M.map (S.delete var) (regs cg)
})

unbindRegister :: Reg -> Id -> CGM ()
unbindRegister reg var = modify (\cg -> cg {
  sym = adjust (setAddr $ first (S.delete reg)) var (sym cg)
})

getLoc :: RValue -> CGM Reg
getLoc (LitRV lit) = do
  reg <- getEmptyRegister
  cgConst reg lit
  return reg
getLoc (VarRV var) = do
  locations <- gets (addr . (M.! var) . sym)
  case locations of
    (registers, addresses)
      | S.null registers && S.null addresses -> do
        reg <- getEmptyRegister
        bindRegister reg var
        return reg
      | S.null registers && not (S.null addresses) -> do
        let bestAddress = minimumBy (compare `on` costOf) addresses
        reg <- getEmptyRegister
        cgConst reg bestAddress
        emit (LOAD reg (Ind reg))
        bindRegister reg var
        return reg
      | otherwise ->
        return (S.findMin registers)

save :: S.Set Id -> CGM ()
save vars = do
  symTab <- gets sym
  forM_ vars $ \var -> do
    let s = symTab M.! var
        (regs, _) = addr s
    unless (S.null regs) $ let reg = S.findMin regs in case memo s of
      Just mem -> do
        modify (\cg -> cg {
          sym = adjust (setAddr $ second (S.insert mem)) var (sym cg)
          })
        cgConst R0 mem
        emit (STORE reg (Ind R0))
      Nothing ->
        return ()

getEmptyRegister :: CGM Reg
getEmptyRegister = do
  registers <- gets regs
  let (emptys, nonemptys) = M.partition S.null registers
  if M.null emptys then do
    let comp = compare `on` S.size . snd
    let (reg, vars) = minimumBy comp $ M.toList nonemptys
    save vars
    mapM_ (unbindRegister reg) vars
    return reg
  else
    return (fst $ M.findMin emptys)

getExitUses :: MaybeC x (Node O C) -> S.Set Id -> S.Set Id
getExitUses (JustC (Branch _)) alive = alive
getExitUses (JustC (Czero x _ _)) alive = S.insert x alive
getExitUses (JustC (Codd x _ _)) alive = S.insert x alive
getExitUses (JustC _) _ = error "getExitUses"

getUses :: [Node O O] -> S.Set Id
  -> [(Node O O, S.Set Id)] -> [(Node O O, S.Set Id)]
getUses (node : nodes) uses acc = case node of
  Move x y -> go x (tryInsert y (S.delete x uses))
  Binop x o y z -> go x (tryInsert y (tryInsert z (S.delete x uses)))
  Reset x -> go x (S.delete x uses)
  Inc x -> go x uses
  Dec x -> go x uses
  Shl x -> go x uses
  Shr x -> go x uses
  Load x y z -> go x (tryInsert z (S.delete x uses))
  Store x y z -> go' (tryInsert y (tryInsert z uses))
  Read x -> go x (S.delete x uses)
  Write x -> go' (tryInsert x uses)
  where
    go' u = getUses nodes u ((node, uses) : acc)
    go x u | x `S.member` uses = go' u
           | otherwise = getUses nodes uses acc
getUses [] _ acc = acc

tryInsert :: RValue -> S.Set Id -> S.Set Id
tryInsert (LitRV _) = id
tryInsert (VarRV x) = S.insert x

cgExit :: MaybeC x (Node O C) -> S.Set Id -> CGM ()
cgExit NothingC alive = save alive
cgExit (JustC node) alive = do
  save alive
  res <- gets resolve
  case node of
    Branch l -> emit (JUMP (res l))
    Czero x l1 l2 -> do
      x' <- getLoc (VarRV x)
      emit (JZERO x' (res l1))
      emit (JUMP (res l2))
    Codd x l1 l2 -> do
      x' <- getLoc (VarRV x)
      emit (JODD x' (res l1))
      emit (JUMP (res l2))

cg :: [(Node O O, S.Set Id)] -> CGM ()
cg ((node, uses) : nodes) = case node of
  Move x y | VarRV x == y -> return ()
           | otherwise -> do
    y' <- getLoc y
    rebindRegister y' x
    cg nodes
  Binop x o y z -> do
    y' <- getLoc y
    z' <- getLoc z
    emit (op y' z')
    rebindRegister y' x
    cg nodes
    where
      op = case o of
        Add -> ADD
        Sub -> SUB
        _ -> error "cg"
  Reset x -> do
    x' <- getEmptyRegister
    emit (RESET x')
    rebindRegister x' x
    cg nodes
  Inc x -> do
    x' <- getLoc (VarRV x)
    emit (INC x')
    rebindRegister x' x
    cg nodes
  Dec x -> do
    x' <- getLoc (VarRV x)
    emit (DEC x')
    rebindRegister x' x
    cg nodes
  Shl x -> do
    x' <- getLoc (VarRV x)
    emit (SHL x')
    rebindRegister x' x
    cg nodes
  Shr x -> do
    x' <- getLoc (VarRV x)
    emit (SHR x')
    rebindRegister x' x
    cg nodes
  Load x y zz -> do
    (mem, z') <- case zz of
      VarRV z -> do
        Just mem <- gets (memo . (M.! y) . sym)
        z' <- getLoc (VarRV z)
        cgConst R0 mem
        emit (ADD z' R0)
        return (mem, z')
      LitRV z -> do
        Just mem <- gets (memo . (M.! y) . sym)
        z' <- getLoc (LitRV (mem + z))
        return (mem, z')
    emit (LOAD z' (Ind z'))
    rebindRegister z' x
    cg nodes
  Store x (LitRV y) z -> do
    Just mem <- gets (memo . (M.! x) . sym)
    z' <- getLoc z
    cgConst R0 (mem + y)
    emit (STORE z' (Ind R0))
    cg nodes
  Store x (VarRV y) z -> do
    Just mem <- gets (memo . (M.! x) . sym)
    z' <- getLoc z
    y' <- getLoc (VarRV y)
    cgConst R0 mem
    emit (ADD R0 y')
    emit (STORE z' (Ind R0))
    cg nodes
  Read x -> do
    x' <- getEmptyRegister
    emit (READ x')
    rebindRegister x' x
    cg nodes
  Write x -> do
    x' <- getLoc x
    emit (WRITE x')
    cg nodes
cg [] = return ()

emit :: Target -> CGM ()
emit tg = modify (\s -> s { code = tg : code s })

cgConst :: Reg -> Natural -> CGM ()
cgConst r 0 = emit (RESET r)
cgConst r x = emit (RESET r) >> emit (INC r) >> go x
  where
    go 1 = return ()
    go x = case divMod x 2 of
      (d, 0) -> emit (SHL r) >> go d
      (d, _) -> emit (SHL r) >> emit (INC r) >> go d

resolveLabels :: [Either Label Target] -> ([Target], Label -> Imm)
resolveLabels = go 0 [] (mapEmpty :: LabelMap Imm)
  where
    go line code labels [] = (reverse code, fromJust . flip mapLookup labels)
    go line code labels (Left lbl : xs) = go line code (mapInsert lbl (Imm line) labels) xs
    go line code labels (Right x : xs) = go (line + 1) (x : code) labels xs
