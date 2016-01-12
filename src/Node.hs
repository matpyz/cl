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
  Czero :: Id -> Label -> Label -> Node O C
  Codd :: Id -> Label -> Label -> Node O C
  Cond :: RValue -> Ordering -> RValue -> Label -> Label -> Node O C
  Move :: Id -> RValue -> Node O O
  Binop :: Id -> ArithOp -> RValue -> RValue -> Node O O
  Reset :: Id -> Node O O
  Inc :: Id -> Node O O
  Dec :: Id -> Node O O
  Shl :: Id -> Node O O
  Shr :: Id -> Node O O
  Load :: Id -> Id -> RValue -> Node O O
  Store :: Id -> RValue -> RValue -> Node O O
  Read :: Id -> Node O O
  Write :: RValue -> Node O O

deriving instance Show (Node e x)

instance NonLocal Node where
  entryLabel (Label l) = l
  successors (Branch l) = [l]
  successors (Czero _ t f) = [t, f]
  successors (Codd _ t f) = [t, f]
  successors (Cond _ _ _ t f) = [t, f]

instance HooplNode Node where
  mkLabelNode = Label
  mkBranchNode = Branch

type AInter = AGraph Node
type AIntermediate = AInter O O
type AInterExit = AInter O C

type Inter = Graph Node
type Intermediate = Inter O O
type InterExit = Inter O C
