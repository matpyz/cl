module Check where

import           Compiler.Hoopl    hiding ((<*>))
import           Control.Monad.RWS
import           Data.List
import           Data.Maybe
import           Source
import           Token             (Name)

type Id = Int
type Env = [(String, Id)]
type SymbolTable = UniqueMap Decl
type CheckT = RWST Env (Endo [String]) SymbolTable

check :: UniqueMonad m => Source -> m (SymbolTable, [Command Id])
check (decls, cmds) = do
  (cmds', symtab, errors) <- runRWST go [] mapEmpty
  case intercalate "\n" $ appEndo errors [] of
    [] -> return (symtab, cmds')
    es -> error es
  where
  go = foldr with (chkCommands cmds) decls

with :: UniqueMonad m => Decl -> CheckT m a -> CheckT m a
with (Decl n@(str, pos) s) cont = do
  env <- ask
  case lookup str env of
    Nothing -> return ()
    Just _ -> tell . Endo . (:) $
      "Redeklaracja zmiennej " ++ str ++ " w " ++ show pos
  uniq <- lift freshUnique
  modify (mapInsert uniq (Decl n s))
  local ((str, uniq) :) cont

class Chk f where
  chk :: UniqueMonad m => f Name -> CheckT m (f Id)

instance Chk Command where
  chk (a := e) = (:=) <$> chkIdentifier a <*> chk e
  chk (If c s1 s2) = If <$> chkCondition c <*> chkCommands s1 <*> chkCommands s2
  chk (For i down a b s) = do
    a' <- chk a
    b' <- chk b
    with (Decl i Nothing) $ do
      i' <- asks (snd . head)
      s' <- chkCommands s
      return (For i' down a' b' s')
  chk (While c s) = While <$> chkCondition c <*> chkCommands s
  chk (Get a) = Get <$> chkIdentifier a
  chk (Put a) = Put <$> chk a

chkCommands :: UniqueMonad m => [Command Name] -> CheckT m [Command Id]
chkCommands = traverse chk

chkIdentifier :: UniqueMonad m => Identifier Name -> CheckT m (Identifier Id)
chkIdentifier (x, ix) = (,) <$> nameToId indexed x <*> chk ix
  where
    indexed = case ix of
      NoIx -> False
      _ -> True

chkCondition :: UniqueMonad m => Condition Name -> CheckT m (Condition Id)
chkCondition (a, o, b) = (,,) <$> chk a <*> pure o <*> chk b

instance Chk Expression where
  chk (Val v) = Val <$> chk v
  chk (Expr a o b) = Expr <$> chk a <*> pure o <*> chk b

instance Chk Index where
  chk NoIx = pure NoIx
  chk (LitIx l) = pure (LitIx l)
  chk (VarIx v) = VarIx <$> nameToId False v

instance Chk Value where
  chk (Lit l) = pure (Lit l)
  chk (Var v) = Var <$> chkIdentifier v

nameToId :: UniqueMonad m => Bool -> Name -> CheckT m Id
nameToId indexed n@(str, pos) = do
  env <- ask
  case lookup str env of
    Nothing -> do
      tell . Endo . (:) $
        "Niezadeklarowana zmienna " ++ str ++ " w " ++ show pos
      lift freshUnique
    Just uniq -> do
      Just info <- gets (mapLookup uniq)
      when (isNothing (size info) == indexed) $
        tell . Endo . (:) $
          "Nieprawidlowe uzycie zmiennej " ++
          (if indexed then "skalarnej " else "tablicowej ") ++
          str ++ " w " ++ show pos
      return uniq
