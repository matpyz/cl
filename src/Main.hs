module Main (main) where

import           Allocate
import           Check
import           CodeGen
import           Compiler.Hoopl
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Debug.Trace
import           Expand
import           Intermediate
import           Parser
import           SymTab
import           System.Environment
import           System.FilePath.Lens
import           Target

main :: IO ()
main = do
  paths <- getArgs
  paths `forM_` \path -> handle (\(ErrorCall s) -> putStrLn s) $ do
    contents <- (expand <=< intermediate) . check . parse <$> readFile path
    let (graph, symTab) = allocate . runSimpleUniqueMonad $ contents
    symTab `forM_` \entry ->
      traceIO $ entry^.name._1 ++ " @ " ++ show (entry^.memory)
    traceIO (showGraph show graph)
    let path' = set extension "mr" path
    writeFile path' (render (codeGen (graph, symTab)))
