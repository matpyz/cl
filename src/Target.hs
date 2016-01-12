module Target where

import Numeric.Natural

data Target
  = HALT
  | READ Reg
  | WRITE Reg
  | LOAD Reg Ind
  | STORE Reg Ind
  | COPY Reg Reg
  | ADD Reg Reg
  | SUB Reg Reg
  | SHR Reg
  | SHL Reg
  | INC Reg
  | DEC Reg
  | RESET Reg
  | JUMP Imm
  | JZERO Reg Imm
  | JODD Reg Imm
  deriving (Show)

render :: [Target] -> String
render = unlines . map show

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  deriving (Eq, Ord, Enum, Bounded)

r :: Int -> Reg
r = toEnum

instance Show Reg where show = show . fromEnum

newtype Ind = Ind { ind :: Reg }
instance Show Ind where show = show . ind

newtype Imm = Imm { imm :: Natural }
instance Show Imm where show = show . imm
