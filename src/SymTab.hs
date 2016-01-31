{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module SymTab where

import           Compiler.Hoopl
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Numeric.Natural     (Natural)
import           Source
import           Target
import           Token

data Kind = Glob | Iter | Temp
  deriving (Eq, Show)

data Sym = Sym {
  _name :: Name,
  _size :: Maybe Natural,
  _kind :: Kind,
  _defined :: Bool,
  _memory :: Maybe Natural,
  _regs :: S.Set Reg,
  _addrs :: S.Set Natural
} deriving (Show)

instance Monoid Sym where
  mempty = error "mempty :: Sym"
  mappend = const

makeLenses ''Sym

type SymTab = M.Map Id Sym

newSymTab :: SymTab
newSymTab = M.empty

add :: Sym -> SymTab -> SymTab
add s@Sym { _name = (k, _) } = M.insert k s

type SymTabM = InfiniteFuelMonad (StateT SymTab SimpleUniqueMonad)

instance UniqueMonad m => UniqueMonad (StateT s m) where
  freshUnique = lift freshUnique

instance CheckpointMonad m => CheckpointMonad (StateT s m) where
  type Checkpoint (StateT s m) = (s, Checkpoint m)
  checkpoint = (,) <$> get Prelude.<*> lift checkpoint
  restart (x, y) = put x >> lift (restart y)

runSymTabM :: SymTabM a -> SymTab -> SimpleUniqueMonad (a, SymTab)
runSymTabM = runStateT . runWithFuel infiniteFuel

new :: Kind -> SymTabM Id
new knd = liftFuel $ do
  uniq <- freshUnique
  let str = '~' : show uniq
  modify $ add Sym {
    _name = (str, (0, 0)),
    _size = Nothing,
    _kind = knd,
    _defined = True,
    _memory = Nothing,
    _regs = S.empty,
    _addrs = S.empty
    }
  return str
