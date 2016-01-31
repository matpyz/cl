module Allocate where

import           Data.Bits
import           Data.List
import qualified Data.Map         as M
import           Data.Traversable
import           Node
import           Numeric.Natural
import           SymTab

allocate :: (Intermediate, SymTab) -> (Intermediate, SymTab)
allocate (inter, symTab) =
  (inter, M.fromList [(fst $ _name sym, sym) | sym <- list] `M.union` symTab)
  where
    list = snd $ mapAccumL alloc (0, addressSpace) $ sortOn _size
      -- [sym | sym <- M.elems symTab, kind sym /= Temp]
      (M.elems symTab)

alloc :: (Natural, [Natural]) -> Sym -> ((Natural, [Natural]), Sym)
alloc (m, ~(a:space)) sym@Sym { _size = Nothing } =
  ((max m (a + 1), space), sym { _memory = Just a })
alloc (m, space) sym@Sym { _size = Just sz } =
  ((m', space'), sym { _memory = Just a })
  where
    a | m == 0 = 0
      | otherwise = if costOf n < costOf m then n else m
        where
          n = 1 `shiftL` ceiling (logBase 2 (fromIntegral m))
    m' = a + sz
    space' = filter (\x -> a > x || x >= m') space

addressSpace :: [Natural]
addressSpace = 0 : gen [1] [2]
  where
    gen xs ys = xs ++ gen ys (sort (ys' ++ xs'))
      where
        ys' = [shiftL y 1 | y <- ys]
        xs' = [shiftL x 1 .|. 1 | x <- xs]

costOf :: Natural -> Natural
costOf 0 = 1
costOf 1 = 2
costOf x = 1 + costOf (shiftR x 1) + x .&. 1
