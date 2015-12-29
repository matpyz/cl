module Main (main) where

import Parser
import Check
import Compiler.Hoopl

main :: IO ()
main = do
  contents <- runSimpleUniqueMonad . check . parse <$> getContents
  print contents
