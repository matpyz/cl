module Intermediate where

import           Compiler.Hoopl      hiding ((<*>))
import qualified Compiler.Hoopl      as H ((<*>))
import           Control.Monad.State
import           Data.Bool
import qualified Data.Map            as M
import           Node
import           Source
import           SymTab

intermediate :: ([Command Id], SymTab) -> SimpleUniqueMonad (Intermediate, SymTab)
intermediate (commands, symTab) = do
  (aGraph, symTab') <- runSymTabM (mkCmds commands) symTab
  graph <- graphOfAGraph aGraph
  return (graph, symTab')

mkCmds :: [Command Id] -> SymTabM AIntermediate
mkCmds cmds = catGraphs <$> mapM mkCmd cmds

mkCmd :: Command Id -> SymTabM AIntermediate
mkCmd (x := y) = mkAssign x y
mkCmd (If c t f) = mkIfThenElse <$> mkCond c <*> mkCmds t <*> mkCmds f
mkCmd (While cond cmds) = mkWhileDo <$> mkCond cond <*> mkCmds cmds
mkCmd (For i down a b c) = mkFor i down a b =<< mkCmds c
mkCmd (Get v) = v `withLValue` \lv -> return (mkMiddle (Read lv))
mkCmd (Put v) = (\(cd, rv) -> cd H.<*> mkMiddle (Write rv)) <$> toRValue v

mkAssign :: Identifier Id -> Expression Id -> SymTabM AIntermediate
mkAssign x y = x `withLValue` \lv ->
  case y of
    Val (Lit v) -> return (mkMiddle (Move lv (LitRV v)))
    Val (Var (Id a ix)) -> case indexToRValue ix of
      Nothing -> return (mkMiddle (Move lv (VarRV a)))
      Just rv -> return (mkMiddle (Load lv a rv))
    Expr a o b -> do
      (cda, rva) <- toRValue a
      (cdb, rvb) <- toRValue b
      return (cda H.<*> cdb H.<*> mkMiddle (Binop lv o rva rvb))

mkCond :: Condition Id -> SymTabM (Label -> Label -> AInterExit)
mkCond (Con a (c, o) b) = do
  (cda, rva) <- toRValue a
  (cdb, rvb) <- toRValue b
  return . bool flip id c $ \t f ->
    cda H.<*> cdb H.<*> mkLast (Cond rva o rvb t f)

mkFor :: Id -> Bool -> Value Id -> Value Id -> AIntermediate -> SymTabM AIntermediate
mkFor i down a b body = do
  let i' = Id i NoIx
  let (a', b') = if down then (b, Var i') else (Var i', b)
  c <- new Iter
  let c' = Id c NoIx
  before0 <- mkCmds [i' := Val a, c' := Val b']
  before1 <- mkAssign c' (Expr (Var c') Sub a')
  let before = before0 H.<*> mkMiddle (Inc c) H.<*> before1
  let cond t f = mkLast (Czero c f t)
  let op = if down then Dec else Inc
  let inside = mkMiddle (Dec c) H.<*> body H.<*> mkMiddle (op i)
  return (before H.<*> mkWhileDo cond inside)

indexToRValue :: Index Id -> Maybe RValue
indexToRValue NoIx = Nothing
indexToRValue (LitIx i) = Just (LitRV i)
indexToRValue (VarIx i) = Just (VarRV i)

withLValue :: Identifier Id -> (Id -> SymTabM AIntermediate) -> SymTabM AIntermediate
withLValue (Id a ix) k = case indexToRValue ix of
  Nothing -> k a
  Just rv -> do
    t <- new Temp
    code <- k t
    return (code H.<*> mkMiddle (Store a rv (VarRV t)))

class ToRValue f where
  toRValue :: f Id -> SymTabM (AIntermediate, RValue)

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
    return (cda H.<*> cdb H.<*> mkMiddle (Binop t o rva rvb), VarRV t)

toId :: RValue -> SymTabM (AIntermediate, Id)
toId (LitRV x) = do
  t <- new Temp
  return (mkMiddle (Move t (LitRV x)), t)
toId (VarRV x) = return (emptyGraph, x)
