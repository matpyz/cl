{-# LANGUAGE TypeFamilies #-}
module SymTab where

import           Compiler.Hoopl
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Numeric.Natural     (Natural)
import           Source
import           Target
import           Token

type SymTabM = InfiniteFuelMonad (StateT SymTab SimpleUniqueMonad)

instance UniqueMonad m => UniqueMonad (StateT s m) where
  freshUnique = lift freshUnique

instance CheckpointMonad m => CheckpointMonad (StateT s m) where
  type Checkpoint (StateT s m) = (s, Checkpoint m)
  checkpoint = (,) <$> get Prelude.<*> lift checkpoint
  restart (x, y) = put x >> lift (restart y)

data Kind = Glob | Iter | Temp
  deriving (Eq, Show)

data Sym = Sym {
  name :: Name,
  size :: Maybe Natural,
  kind :: Kind,
  defn :: Bool,
  memo :: Maybe Natural,
  addr :: (S.Set Reg, S.Set Natural)
} deriving (Show)

type SymTab = M.Map Id Sym

newSymTab :: SymTab
newSymTab = M.empty

runSymTabM :: SymTabM a -> SymTab -> SimpleUniqueMonad (a, SymTab)
runSymTabM = runStateT . runWithFuel infiniteFuel

add :: Sym -> SymTab -> SymTab
add s@Sym { name = (k, _) } = M.insert k s

setAddr :: ((S.Set Reg, S.Set Natural) -> (S.Set Reg, S.Set Natural))
  -> Sym -> Sym
setAddr f s@Sym { addr = a } = s { addr = f a }

adjust :: (Sym -> Sym) -> Id -> SymTab -> SymTab
adjust = M.adjust

new :: Kind -> SymTabM Id
new knd = do
  uniq <- freshUnique
  let str = '_' : show uniq
  liftFuel $ modify $ add Sym {
    name = (str, (0, 0)),
    size = Nothing,
    kind = knd,
    defn = True,
    memo = Nothing,
    addr = (S.empty, S.empty)
    }
  return str
