module Intermediate where

import           Compiler.Hoopl      hiding ((<*>))
import qualified Compiler.Hoopl      as H ((<*>))
import           Control.Monad.State
import           Info
import           Node
import           Source
import qualified Data.Map as M

type InterM = State Info

intermediate :: ([Command Id], Info) -> (Intermediate, Info)
intermediate = uncurry (runState . mkCmds)

mkCmds :: [Command Id] -> InterM Intermediate
mkCmds cmds = catGraphs <$> mapM mkCmd cmds

mkCmd :: Command Id -> InterM Intermediate
mkCmd (x := y) = mkAssign x y
mkCmd (If c t f) = mkIfThenElse <$> mkCond c <*> mkCmds t <*> mkCmds f
mkCmd (While cond cmds) = mkWhileDo <$> mkCond cond <*> mkCmds cmds
mkCmd (For i down a b c) = mkFor i down a b =<< mkCmds c
mkCmd (Get v) = v `withLValue` \lv -> return (mkMiddle (Read lv))
mkCmd (Put v) = (\(cd, rv) -> cd H.<*> mkMiddle (Write rv)) <$> toRValue v

mkAssign :: Identifier Id -> Expression Id -> InterM Intermediate
mkAssign x y = x `withLValue` \lv ->
  (\(cd, rv) -> cd H.<*> mkMiddle (Move lv rv)) <$> toRValue y

mkCond :: Condition Id -> InterM (Label -> Label -> InterExit)
mkCond (Con a o b) = do
  (cda, rva) <- toRValue a
  (cdb, rvb) <- toRValue b
  return (\t f -> cda H.<*> cdb H.<*> mkLast (Cond rva o rvb t f))

mkFor :: Id -> Bool -> Value Id -> Value Id -> Intermediate -> InterM Intermediate
mkFor i down a b body = do
  let i' = Id i NoIx
  let (a', b') = if down then (b, Var i') else (Var i', b)
  c <- new Iter
  let c' = Id c NoIx
  before <- mkCmds [
    i' := Val a,
    c' := Val b',
    c' := Expr (Var c') Add (Lit 1),
    c' := Expr (Var c') Sub a']
  let cond t f = mkLast (Cond (VarRV c) (False, EQ) (LitRV 0) t f)
  let op = if down then Sub else Add
  let inside = mkMiddles [Binop c (VarRV c) Sub (LitRV 1),
                          Binop i (VarRV i) op (LitRV 1)] H.<*> body
  return (before H.<*> mkWhileDo cond inside)

indexToRValue :: Index Id -> Maybe RValue
indexToRValue NoIx = Nothing
indexToRValue (LitIx i) = Just (LitRV i)
indexToRValue (VarIx i) = Just (VarRV i)

withLValue :: Identifier Id -> (Id -> InterM Intermediate) -> InterM Intermediate
withLValue (Id a ix) k = case indexToRValue ix of
  Nothing -> k a
  Just rv -> do
    t <- new Temp
    code <- k t
    return (code H.<*> mkMiddle (Store a rv (VarRV t)))

class ToRValue f where
  toRValue :: f Id -> InterM (Intermediate, RValue)

instance ToRValue Identifier where
  toRValue (Id a ix) = case indexToRValue ix of
    Nothing -> return (emptyGraph, VarRV a)
    Just rv -> do
      t <- new Temp
      return (mkMiddle (Load t a rv), VarRV t)

instance ToRValue Value where
  toRValue (Lit v) = return (emptyGraph, LitRV v)
  toRValue (Var v) = toRValue v

instance ToRValue Expression where
  toRValue (Val v) = toRValue v
  toRValue (Expr a o b) = do
    (cda, rva) <- toRValue a
    (cdb, rvb) <- toRValue b
    t <- new Temp
    return (cda H.<*> cdb H.<*> mkMiddle (Binop t rva o rvb), VarRV t)

new :: Kind -> InterM Id
new knd = state $ \s -> let
  t = varcnt s
  str = '_' : show t
  dcl = Decl {
      name = (str, (0, 0)),
      size = Nothing,
      kind = knd,
      addr = [],
      regs = []
    }
  in
  (str, s {
      symtab = M.insert str dcl (symtab s),
      varcnt = t + 1
    })
