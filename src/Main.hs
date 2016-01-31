module Main (main) where

import           Allocate
import           Check
import           CodeGen
import           Compiler.Hoopl
import           Control.Lens
import           Control.Monad
import           Debug.Trace
import           Expand
import           Intermediate
import           Parser
import           SymTab
import           Target

main :: IO ()
main = do
  (graph, symTab) <- allocate . runSimpleUniqueMonad . (expand <=< intermediate) . check . parse <$> getContents
  symTab `forM_` \entry ->
    traceIO $ entry^.name._1 ++ " @ " ++ show (entry^.memory)
  traceIO (showGraph show graph)
  putStr (render (codeGen (graph, symTab)))
