module Main (main) where

import Parser

main :: IO ()
main = do
  contents <- parseProgram <$> getContents
  print contents
