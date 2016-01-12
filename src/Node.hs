{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Node where

import           Compiler.Hoopl
import           Numeric.Natural
import           Source

data RValue = LitRV Natural | VarRV Id
  deriving (Eq, Ord, Show)

data Node e x where
  Label :: Label -> Node C O
  Branch :: Label -> Node O C
  Cond :: RValue -> RelOp -> RValue -> Label -> Label -> Node O C
  Move :: Id -> RValue -> Node O O
  Binop :: Id -> RValue -> ArithOp -> RValue -> Node O O
  Load :: Id -> Id -> RValue -> Node O O
  Store :: Id -> RValue -> RValue -> Node O O
  Read :: Id -> Node O O
  Write :: RValue -> Node O O

deriving instance Show (Node e x)

instance NonLocal Node where
  entryLabel (Label l) = l
  successors (Branch l) = [l]
  successors (Cond _ _ _ t f) = [t, f]

instance HooplNode Node where
  mkLabelNode = Label
  mkBranchNode = Branch

type Inter = AGraph Node O
type Intermediate = Inter O
type InterExit = Inter C
