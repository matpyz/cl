module Main (main) where

import Parser
import Check
import Compiler.Hoopl
import Intermediate

main :: IO ()
main = do
  (agraph, info) <- intermediate . check . parse <$> getContents
  putStrLn (showGraph show (runSimpleUniqueMonad (graphOfAGraph agraph)))
  print info
