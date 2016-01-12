module Check where

import           Control.Monad.RWS
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe
import           Info
import           Numeric.Natural   (Natural)
import           Source

type Env = [String]

type CheckM = RWS Env (Endo [String]) Info

check :: Source -> ([Command Id], Info)
check (decls, cmds) = let
  (cmds', info, errors) = runRWS go [] mkInfo
  in case intercalate "\n" $ appEndo errors [] of
    [] -> (cmds', info)
    es -> error es
  where
  go = foldr (with Glob) (chkCommands cmds) decls

with :: Kind -> (Name, Maybe Natural) -> CheckM a -> CheckM a
with k (n@(str, pos), s) cont = do
  env <- ask
  when (str `elem` env) $
    tell . Endo . (:) $
      "Redeklaracja zmiennej " ++ str ++ " w " ++ show pos
  modify (\(Info t m v) -> Info (M.insert str Decl {
    name = n,
    size = s,
    kind = k,
    addr = [m],
    regs = []
    } t) (m + fromMaybe 1 s) (v + 1))
  local (str :) cont

class Chk f where
  chk :: f Name -> CheckM (f Id)

instance Chk Command where
  chk (a := e) = (:=) <$> chk a <*> chk e
  chk (If c s1 s2) = If <$> chkCondition c <*> chkCommands s1 <*> chkCommands s2
  chk (For i down a b s) = do
    a' <- chk a
    b' <- chk b
    with Iter (i, Nothing) $ do
      i' <- asks head
      s' <- chkCommands s
      return (For i' down a' b' s')
  chk (While c s) = While <$> chkCondition c <*> chkCommands s
  chk (Get a) = Get <$> chk a
  chk (Put a) = Put <$> chk a

chkCommands :: [Command Name] -> CheckM [Command Id]
chkCommands = traverse chk

instance Chk Identifier where
  chk (Id x ix) = Id <$> chkName indexed x <*> chk ix
    where
      indexed = case ix of
        NoIx -> False
        _ -> True

chkCondition :: Condition Name -> CheckM (Condition Id)
chkCondition (Con a o b) = Con <$> chk a <*> pure o <*> chk b

instance Chk Expression where
  chk (Val v) = Val <$> chk v
  chk (Expr a o b) = Expr <$> chk a <*> pure o <*> chk b

instance Chk Index where
  chk NoIx = pure NoIx
  chk (LitIx l) = pure (LitIx l)
  chk (VarIx v) = VarIx <$> chkName False v

instance Chk Value where
  chk (Lit l) = pure (Lit l)
  chk (Var v) = Var <$> chk v

chkName :: Bool -> Name -> CheckM Id
chkName indexed n@(str, pos) = do
  env <- ask
  if str `elem` env then do
    Just info <- gets (M.lookup str . symtab)
    when (isNothing (size info) == indexed) $
      tell . Endo . (:) $
        "Nieprawidlowe uzycie zmiennej " ++
        (if indexed then "skalarnej " else "tablicowej ") ++
        str ++ " w " ++ show pos
  else
    tell . Endo . (:) $
      "Niezadeklarowana zmienna " ++ str ++ " w " ++ show pos
  return str
