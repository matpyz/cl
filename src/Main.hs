module Main (main) where

import           Allocate
import           Check
import           CodeGen
import           Compiler.Hoopl
import           Control.Monad
import           Expand
import           Intermediate
import           Parser
import           Target
import Debug.Trace

main :: IO ()
main = do
  (graph, symTab) <- allocate . runSimpleUniqueMonad . (expand <=< intermediate) . check . parse <$> getContents
  traceIO (showGraph show graph)
  putStr (render (codeGen (graph, symTab)))
