{-# LANGUAGE DeriveTraversable #-}
module Source (
  Source,
  Decl(..),
  Command(..),
  Expression(..),
  Condition,
  Value(..),
  Identifier,
  Index(..)
) where

import           Numeric.Natural (Natural)
import           Token           (ArithOp, Name, RelOp)

type Source = ([Decl], [Command Name])

data Decl = Decl {
  name :: Name,
  size :: Maybe Natural
} deriving (Show)

decl :: Decl
decl = Decl {
  name = ("", (0, 0)),
  size = Nothing
}

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

type Condition a = (Value a, RelOp, Value a)

data Value a = Lit Natural | Var (Identifier a)
  deriving (Show, Functor, Foldable, Traversable)

type Identifier a = (a, Index a)

data Index a = NoIx | LitIx Natural | VarIx a
  deriving (Show, Functor, Foldable, Traversable)
