{-# LANGUAGE DeriveTraversable #-}
module Source (
  Name,
  Id,
  Source,
  Command(..),
  Expression(..),
  Condition(..),
  ArithOp(..),
  RelOp(..),
  Value(..),
  Identifier(..),
  Index(..)
) where

import           Numeric.Natural (Natural)
import           Token           (ArithOp(..), Name, RelOp)

type Id = String

type Source = ([(Name, Maybe Natural)], [Command Name])

data Command a
  = Identifier a := Expression a
  | If (Condition a) [Command a] [Command a]
  | While (Condition a) [Command a]
  | For a Bool (Value a) (Value a) [Command a]
  | Get (Identifier a)
  | Put (Value a)
  deriving (Show, Functor, Foldable, Traversable)

data Expression a = Val (Value a) | Expr (Value a) ArithOp (Value a)
  deriving (Show, Functor, Foldable, Traversable)

data Condition a = Con (Value a) RelOp (Value a)
  deriving (Show, Functor, Foldable, Traversable)

data Value a = Lit Natural | Var (Identifier a)
  deriving (Show, Functor, Foldable, Traversable)

data Identifier a = Id a (Index a)
  deriving (Show, Functor, Foldable, Traversable)

data Index a = NoIx | LitIx Natural | VarIx a
  deriving (Show, Functor, Foldable, Traversable)
