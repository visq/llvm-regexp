{-# LANGUAGE DeriveDataTypeable #-}
module RegExp where
import Data.Generics

data Reg a c = 
    Eps a
  | Sym a c
  | Opt a (Reg a c)
  | Alt a (Reg a c) (Reg a c)
  | Seq a (Reg a c) (Reg a c)
  | Rep a (Reg a c)
  deriving (Show,Read,Typeable,Data)

type RegExp c = Reg () c

eps :: RegExp c
eps = Eps ()

sym :: c -> RegExp c
sym = Sym ()

opt,rep :: RegExp c -> RegExp c
opt = Opt ()
rep = Rep ()

alt, seq_ :: RegExp c -> RegExp c -> RegExp c
alt = Alt ()
seq_ = Seq ()

nrep :: Int -> RegExp c -> RegExp c
nrep 0 _ = Eps ()
nrep 1 r = r
nrep n r = r `seq_` (nrep (n-1) r)

char :: Char -> RegExp Char
char = sym

--psym :: RegExp c
psym = error "character classes are not yet supported"

anySym :: RegExp c
anySym = error "/./ is not yet supported"

rep1 :: RegExp c -> RegExp c
rep1 re = re `seq_` rep re

brep :: (Int,Int) -> RegExp c -> RegExp c
brep = error "{a,b} is not yet supported"

empty :: Reg a c -> Bool
empty r = case r of
  Sym _ _ -> False
  Alt _ r1 r2 -> empty r1 || empty r2
  Seq _ r1 r2 -> empty r1 && empty r2
  _ -> True

-- ((a|b)*c(a|b)*c)*(a|b)*
evencs :: RegExp Char
evencs = (rep (onec `seq_` onec)) `seq_` nocs where
  nocs = rep ((sym 'a')  `alt` (sym 'b'))
  onec = nocs `seq_` (sym 'c')

manyas :: RegExp Char
manyas = (nrep 500 (opt (sym 'a'))) `seq_` (nrep 500 (sym 'a'))
