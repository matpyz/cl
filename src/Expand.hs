{-# LANGUAGE GADTs #-}
module Expand where

import           Compiler.Hoopl      hiding ((<*>))
import qualified Compiler.Hoopl      as H ((<*>))
import           Control.Monad.State
import           Intermediate
import           Node
import           Source
import           SymTab

expand :: (Intermediate, SymTab) -> SimpleUniqueMonad (Intermediate, SymTab)
expand (inter, symTab) = runSymTabM (expandGraph inter) symTab

expandGraph :: Intermediate -> SymTabM Intermediate
expandGraph graph = do
  (graph', _, _) <- analyzeAndRewriteFwdOx expandPass graph ()
  return graph'

expandPass :: FwdPass SymTabM Node ()
expandPass = FwdPass {
  fp_lattice = expandLattice,
  fp_rewrite = mkFRewrite3 expandCO expandOO expandOC,
  fp_transfer = mkFTransfer3 (const id) (const id) distributeFact
}

expandLattice :: DataflowLattice ()
expandLattice = DataflowLattice {
  fact_name = "expand",
  fact_bot = (),
  fact_join = joinFun
} where
  joinFun :: JoinFun ()
  joinFun _ (OldFact ()) (NewFact ()) = (NoChange, ())

expandCO :: Node C O -> () -> SymTabM (Maybe (Graph Node C O))
expandCO _ () = return Nothing

expandOO :: Node O O -> () -> SymTabM (Maybe (Graph Node O O))
expandOO (Binop d Mul s1 s2) () = Just <$> withDifferentIds expandMul d s1 s2
expandOO (Binop d Div s1 s2) () = Just <$> withDifferentIds expandDiv d s1 s2
expandOO (Binop d Mod s1 s2) () = Just <$> withDifferentIds expandMod d s1 s2
expandOO _ () = return Nothing

expandOC :: Node O C -> () -> SymTabM (Maybe (Graph Node O C))
expandOC (Cond a o b t f) () = do
  t0 <- new Temp
  (l1, l2, cd) <- case o of
    LT -> return (f, t, mkMiddle (Binop t0 Sub b a))
    GT -> return (f, t, mkMiddle (Binop t0 Sub a b))
    EQ -> do
      (cda, ida) <- toId a
      (cdb, idb) <- toId b
      let a' = VarRV ida
          b' = VarRV idb
      t1 <- new Temp
      t2 <- new Temp
      return (t, f, cda H.<*> cdb H.<*> mkMiddles [
        Binop t1 Sub a' b',
        Binop t2 Sub b' a',
        Binop t0 Add (VarRV t1) (VarRV t2)])
  Just <$> graphOfAGraph (cd H.<*> mkLast (Czero t0 l1 l2))
expandOC _ () = return Nothing

withDifferentIds :: (Id -> Id -> Id -> SymTabM Intermediate)
  -> Id -> RValue -> RValue -> SymTabM Intermediate
withDifferentIds action d s1 s2 = do
  a <- new Iter
  b <- new Iter
  c <- new Iter
  cd <- action c a b
  return (catGraphs [
    mkMiddles [Move a s1, Move b s2],
    cd, mkMiddle (Move d (VarRV c))])

expandMul :: Id -> Id -> Id -> SymTabM Intermediate
expandMul c a b = do
  swap <- freshLabel
  loop <- freshLabel
  check <- freshLabel
  shift <- freshLabel
  acc <- freshLabel
  end <- freshLabel
  let a' = VarRV a
      b' = VarRV b
      c' = VarRV c
  return (mkMiddle (
    Binop   c       Sub     a'      b'  ) H.<*> mkLast (
    Czero   c       loop    swap        ) |*><*| mkFirst (
    Label   swap                        ) H.<*> mkMiddles [
    Move    c       a'                  ,
    Move    a       b'                  ,
    Move    b       c'                  ,
    Reset   c                           ] H.<*> mkLast (
    Branch  loop                        ) |*><*| mkFirst (
    Label   loop                        ) H.<*> mkLast (
    Czero   a       end     check       ) |*><*| mkFirst (
    Label   check                       ) H.<*> mkLast (
    Codd    a       acc     shift       ) |*><*| mkFirst (
    Label   acc                         ) H.<*> mkMiddle (
    Binop   c       Add     c'      b'  ) H.<*> mkLast (
    Branch  shift                       ) |*><*| mkFirst (
    Label   shift                       ) H.<*> mkMiddles [
    Shr     a                           ,
    Shl     b                           ] H.<*> mkLast (
    Branch  loop                        ) |*><*| mkFirst (
    Label   end                         ))

expandDiv :: Id -> Id -> Id -> SymTabM Intermediate
expandDiv d a b = do
  c <- new Iter
  expandDivMod a b c d

expandMod :: Id -> Id -> Id -> SymTabM Intermediate
expandMod c a b = do
  d <- new Iter
  expandDivMod a b c d

expandDivMod :: Id -> Id -> Id -> Id -> SymTabM Intermediate
expandDivMod a b c d = do
  nonzero <- freshLabel
  loop1 <- freshLabel
  step <- freshLabel
  end1 <- freshLabel
  loop2 <- freshLabel
  shift <- freshLabel
  one <- freshLabel
  check <- freshLabel
  zero <- freshLabel
  end2 <- freshLabel
  e <- new Iter
  let a' = VarRV a
      b' = VarRV b
      e' = VarRV e
  return (mkLast (
    Czero   b       zero    nonzero     ) |*><*| mkFirst (
    Label   nonzero                     ) H.<*> mkMiddle (
    Move    e       b'                  ) H.<*> mkLast (
    Branch  loop1                       ) |*><*| mkFirst (
    Label   loop1                       ) H.<*> mkMiddle (
    Binop   d       Sub     e'      a'  ) H.<*> mkLast (
    Czero   d       step    end1        ) |*><*| mkFirst (
    Label   step                        ) H.<*> mkMiddle (
    Shl     e                           ) H.<*> mkLast (
    Branch  loop1                       ) |*><*| mkFirst (
    Label   end1                        ) H.<*> mkMiddle (
    Reset   d                           ) H.<*> mkLast (
    Branch  loop2                       ) |*><*| mkFirst (
    Label   loop2                       ) H.<*> mkMiddle (
    Binop   c       Sub     e'      a'  ) H.<*> mkLast (
    Czero   c       one     shift       ) |*><*| mkFirst (
    Label   shift                       ) H.<*> mkMiddles [
    Shl     d                           ,
    Shr     e                           ] H.<*> mkLast (
    Branch  check                       ) |*><*| mkFirst (
    Label   one                         ) H.<*> mkMiddles [
    Shl     d                           ,
    Inc     d                           ,
    Binop   a       Sub     a'      e'  ,
    Shr     e                           ] H.<*> mkLast (
    Branch  check                       ) |*><*| mkFirst (
    Label   check                       ) H.<*> mkMiddle (
    Binop   c       Sub     b'      e'  ) H.<*> mkLast (
    Czero   c       loop2   end2        ) |*><*| mkFirst (
    Label   zero                        ) H.<*> mkMiddles [
    Reset   a                           ,
    Reset   d                           ] H.<*> mkLast (
    Branch  end2                        ) |*><*| mkFirst (
    Label   end2                        ) H.<*> mkMiddle (
    Move    c       a'                  ))
