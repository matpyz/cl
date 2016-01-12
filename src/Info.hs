module Info where

import qualified Data.Map        as M
import           Numeric.Natural (Natural)
import           Target
import           Token

data Decl = Decl {
  name :: Name,
  size :: Maybe Natural,
  kind :: Kind,
  addr :: [Natural],
  regs :: [Reg]
} deriving (Show)

data Kind = Glob | Iter | Temp
  deriving (Show)

data Info = Info {
  symtab :: SymTab,
  memsiz :: Natural,
  varcnt :: Natural
} deriving (Show)

mkInfo = Info { symtab = mempty, memsiz = 0, varcnt = 0 }

modSymTab :: (SymTab -> SymTab) -> Info -> Info
modSymTab mutate info = info { symtab = mutate (symtab info) }

type SymTab = M.Map String Decl
