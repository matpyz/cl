module Syntax where

import Token
import Numeric.Natural

type Program = ([VDeclaration], [Command])

type VDeclaration = (PIdentifier, Maybe Natural)

data Command
  = Identifier := Expression
  | If Condition [Command] [Command]
  | While Condition [Command]
  | For PIdentifier Bool Value Value [Command]
  | Get Identifier
  | Put Value
  deriving (Show)

data Expression = Val Value | Expr Value ArithOp Value
  deriving (Show)

type Condition = (Value, RelOp, Value)

data Value = Lit Natural | Var Identifier
  deriving (Show)

type Identifier = (PIdentifier, Index)

data Index = NoIx | LitIx Natural | VarIx PIdentifier
  deriving (Show)
