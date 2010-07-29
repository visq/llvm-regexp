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
  CharClass(..),
  CharSet(..),
  Reg(..),
  RegExp,
  eps, sym, opt, rep, alt, seq_, nrep, char, psym, anySym, rep1, brep,
  empty,
  evencs, manyas
) where
import Data.Char
import Data.Generics

data CharClass =
    ClsNone
  | ClsAny
  | ClsChar Char
  | ClsClass Char
  | ClsRange (Char,Char)
  | ClsNegate CharClass
  | ClsUnion CharClass CharClass
  deriving (Show,Read,Eq,Ord)

data Reg a = 
    Eps a
  | Sym a CharSet
  | Opt a (Reg a)
  | Alt a (Reg a) (Reg a)
  | Seq a (Reg a) (Reg a)
  | Rep a (Reg a)
  deriving (Show,Read,Typeable,Data)

data CharSet =
    CharSet [Char]
  | AnyChar
  deriving (Show,Read,Typeable,Data)
  
type RegExp = Reg ()

eps :: RegExp
eps = Eps ()

sym :: Char -> RegExp
sym c = Sym () (CharSet [c])

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

psym :: CharClass -> RegExp
psym cs = case flatten cs of
        []                       -> error "empty character set [] not supported"
        cs' | any (==ClsAny) cs' -> Sym () AnyChar
        cs'                      -> Sym () (CharSet (map fromChar cs'))
  where fromChar (ClsChar c) = c
        fromChar _ = error "character classes like \\w are not yet supported"
        flatten :: CharClass -> [CharClass]
        flatten (ClsNegate cls) = error "negated character classes are not yet supported"
        flatten (ClsUnion cs1 cs2) = flatten cs1 ++ flatten cs2
        flatten ClsNone = []
        flatten cls     = [cls]

anySym :: RegExp
anySym = Sym () AnyChar

rep1 :: RegExp -> RegExp
rep1 re = re `seq_` rep re

brep :: (Int,Int) -> RegExp -> RegExp
brep = error "{a,b} is not yet supported"

empty :: Reg a -> Bool
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
