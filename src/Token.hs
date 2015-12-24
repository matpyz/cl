module Token where

import Numeric.Natural

type Num = Natural
type PIdentifier = String

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
  | ARITHOP ArithOp
  | RELOP RelOp
  | NUM Natural
  | ID PIdentifier
  deriving (Read)
