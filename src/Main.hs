module Main (main) where

import Parser

main :: IO ()
main = do
  contents <- parse <$> getContents
  print contents
