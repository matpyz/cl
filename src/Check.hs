module Check where

import           Control.Monad.RWS
import qualified Data.DList        as D
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe
import           Numeric.Natural   (Natural)
import           Source
import           SymTab

type CheckM = RWS [Id] (D.DList String) SymTab

msg :: [String] -> CheckM ()
msg = tell . D.singleton . concat

check :: Source -> ([Command Id], SymTab)
check (decls, cmds) = let
  (cmds', info, errors) = runRWS go [] newSymTab
  in case D.toList errors of
    [] -> (cmds', info)
    es -> error (intercalate "\n" es)
  where
  go = foldr (with Glob) (chkCommands cmds) decls

with :: Kind -> (Name, Maybe Natural) -> CheckM a -> CheckM a
with k (n@(str, pos), s) cont = do
  env <- ask
  when (str `elem` env) $
    msg ["Redeklaracja zmiennej ", str, " w ", show pos]
  modify $ add Sym {
    name = n,
    size = s,
    kind = k,
    defn = k == Iter || isJust s,
    memo = Nothing,
    addr = (mempty, mempty)
    }
  local (str :) cont

class Chk f where
  chk :: f Name -> CheckM (f Id)

instance Chk Command where
  chk (a := e) = flip (:=) <$> chk e <*> chkLValue a
  chk (If c s1 s2) = If <$> chkCondition c <*> chkCommands s1 <*> chkCommands s2
  chk (For i down a b s) = do
    a' <- chk a
    b' <- chk b
    with Iter (i, Nothing) $ do
      i' <- asks head
      s' <- chkCommands s
      return (For i' down a' b' s')
  chk (While c s) = While <$> chkCondition c <*> chkCommands s
  chk (Get a) = Get <$> chkLValue a
  chk (Put a) = Put <$> chk a

chkCommands :: [Command Name] -> CheckM [Command Id]
chkCommands = traverse chk

instance Chk Identifier where
  chk (Id x ix) = Id <$> chkName False (hasIndex ix) x <*> chk ix

hasIndex :: Index a -> Bool
hasIndex NoIx = False
hasIndex _ = True

chkCondition :: Condition Name -> CheckM (Condition Id)
chkCondition (Con a o b) = Con <$> chk a <*> pure o <*> chk b

instance Chk Expression where
  chk (Val v) = Val <$> chk v
  chk (Expr a o b) = Expr <$> chk a <*> pure o <*> chk b

instance Chk Index where
  chk NoIx = pure NoIx
  chk (LitIx l) = pure (LitIx l)
  chk (VarIx v) = VarIx <$> chkName False False v

instance Chk Value where
  chk (Lit l) = pure (Lit l)
  chk (Var v) = Var <$> chk v

chkName :: Bool -> Bool -> Name -> CheckM Id
chkName lValue indexed n@(str, pos) = do
  env <- ask
  if str `elem` env then do
    Just sym <- gets (M.lookup str)
    when (isNothing (size sym) == indexed) $
      msg [
        "Uzycie niezgodne z typem zmiennej ",
        if indexed then "skalarnej " else "tablicowej ",
        str, " w ", show pos]
    unless (lValue || defn sym) $
      msg ["Uzycie niezainicjalizowanej zmienej ", str, " w ", show pos]
  else
    msg ["Niezadeklarowana zmienna ", str, " w ", show pos]
  return str

chkLValue :: Identifier Name -> CheckM (Identifier Id)
chkLValue (Id v ix) = do
  v' <- chkName True (hasIndex ix) v
  case ix of
    NoIx -> do
      modify $ adjust (\s -> s { defn = True }) (fst v)
      return (Id v' NoIx)
    i -> Id v' <$> chk i
