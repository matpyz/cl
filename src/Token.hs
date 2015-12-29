module Token (
  Name,
  ArithOp(..),
  RelOp(..),
  Token(..)
) where

import           Numeric.Natural (Natural)

type Name = (String, (Int, Int))

data ArithOp
  = Add | Sub | Mul | Div | Mod
  deriving (Read, Show)
type RelOp = (Bool, Ordering)

data Token
  = DECLARE | IN | END
  | IF | THEN | ELSE | ENDIF
  | WHILE | DO | ENDWHILE
  | FOR | DOWN | FROM | TO | ENDFOR
  | GET | PUT
  | EQUAL | SEMICOLON | LPAREN | RPAREN
  | ARITHOP !ArithOp
  | RELOP !RelOp
  | NUM !Natural
  | ID !Name
  deriving (Read, Show)
