{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RegExp
-- Copyright   :  (c) 2010 Benedikt Huber
-- License     :  BSD-style
-- Stability   :  Early Demo
-- Portability :  Test with ghc-6.10
-- 
-- Regular Expression Datatype and smart constructors
-----------------------------------------------------------------------------
module RegExp (
  Reg(..),
  RegExp,
  eps, sym, opt, rep, alt, seq_, nrep, char, psym, anySym, rep1, brep,
  empty,
  evencs, manyas
) where
import Data.Generics

data Reg a c = 
    Eps a
  | Sym a c
  | Opt a (Reg a c)
  | Alt a (Reg a c) (Reg a c)
  | Seq a (Reg a c) (Reg a c)
  | Rep a (Reg a c)
  deriving (Show,Read,Typeable,Data)

type RegExp = Reg () Char

eps :: RegExp
eps = Eps ()

sym :: Char -> RegExp
sym = Sym ()

opt,rep :: RegExp -> RegExp
opt = Opt ()
rep = Rep ()

alt, seq_ :: RegExp -> RegExp -> RegExp
alt = Alt ()
seq_ = Seq ()

nrep :: Int -> RegExp -> RegExp
nrep 0 _ = Eps ()
nrep 1 r = r
nrep n r = r `seq_` (nrep (n-1) r)

char :: Char -> RegExp
char = sym

--psym :: RegExp c
psym = error "character classes are not yet supported"

anySym :: RegExp
anySym = error "/./ is not yet supported"

rep1 :: RegExp -> RegExp
rep1 re = re `seq_` rep re

brep :: (Int,Int) -> RegExp -> RegExp
brep = error "{a,b} is not yet supported"

empty :: Reg a c -> Bool
empty r = case r of
  Sym _ _ -> False
  Alt _ r1 r2 -> empty r1 || empty r2
  Seq _ r1 r2 -> empty r1 && empty r2
  _ -> True

-- examples
-- ((a|b)*c(a|b)*c)*(a|b)*
evencs :: RegExp
evencs = (rep (onec `seq_` onec)) `seq_` nocs where
  nocs = rep ((sym 'a')  `alt` (sym 'b'))
  onec = nocs `seq_` (sym 'c')

manyas :: RegExp
manyas = (nrep 500 (opt (sym 'a'))) `seq_` (nrep 500 (sym 'a'))
