-- Copyright (c) 2010, Thomas Wilke, Frank Huch, Sebastian Fischer
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
-- 
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
-- 
--  3. Neither the name of the author nor the names of his contributors
--     may be used to endorse or promote products derived from this
--     software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-missing-signatures #-}

module Parser ( parse ) where

import RegExp
  ( eps, char, psym, anySym, alt, seq_, rep, rep1, opt, brep, CharClass(..) )

import Data.Char ( isSpace, toLower, isAlphaNum, isDigit )

}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
       sym { Sym $$ }
       '*' { Ast }
       seq { Seq }
       '|' { Bar }
       '(' { L }
       ')' { R }
       '+' { Pls }
       '?' { Que }
       bnd { Bnd $$ }
       cls { Cls $$ }
       '.' { Dot }

%right '|'
%right seq
%right '*' '+' '?' bnd

%%

RegExp : {- empty -}       { eps }
       | sym               { char $1 }
       | RegExp '*'        { rep $1 }
       | RegExp seq RegExp { seq_ $1 $3 }
       | RegExp '|' RegExp { alt $1 $3 }
       | '(' RegExp ')'    { $2 }
       | RegExp '+'        { rep1 $1 }
       | RegExp '?'        { opt $1 }
       | RegExp bnd        { brep $2 $1 }
       | cls               { psym $1 }
       | '.'               { anySym }

{

parse = parseTokens . scan

data Token = Seq | Sym Char | Ast | Bar | L | R
           | Pls | Que | Bnd (Int,Int)
           | Cls CharClass | Dot


token :: Char -> Token
token '*'  = Ast
token '|'  = Bar
token '('  = L
token ')'  = R
token '?'  = Que
token '+'  = Pls
token '.'  = Dot
token c    = Sym c

scan :: String -> [Token]
scan = insertSeqs . process

insertSeqs :: [Token] -> [Token]
insertSeqs []           = []
insertSeqs [t]          = [t]
insertSeqs (a:ts@(b:_))
  | lseq a && rseq b    = a : Seq : insertSeqs ts
  | otherwise           = a : insertSeqs ts

lseq :: Token -> Bool
lseq Bar = False
lseq L   = False
lseq _   = True

rseq :: Token -> Bool
rseq (Sym _) = True
rseq L       = True
rseq (Cls _) = True
rseq Dot     = True
rseq _       = False

process :: String -> [Token]
process []            = []

process ('\\':c:cs)   = Cls (symClassPred c) : process cs

process ('{':cs)      = case reads cs of
                          (n,'}':s1) : _ -> Bnd (n,n) : process s1
                          (n,',':s1) : _ ->
                              case reads s1 of
                                (m,'}':s2) : _ -> Bnd (n,m) : process s2
                                _              -> token '{' : process cs
                          _              -> token '{' : process cs

process ('[':'^':cs)  = Cls (ClsNegate p) : process xs
 where (s,p,xs) = processCls cs

process ('['    :cs)  = Cls (p) : process xs
 where (s,p,xs) = processCls cs

process (c:cs)        = token c : process cs

processCls :: String -> (String, CharClass, String)

processCls []           = parseError []

processCls (']':cs)     = ("]", ClsNone, cs)

processCls ('\\':c:cs)
  | isSymClassChar c    = ('\\':c:s, symClassPred c `ClsUnion` clss, xs)
 where (s,clss,xs) = processCls cs

processCls ('\\':c:cs)  = ('\\':c:s, ClsChar c `ClsUnion` clss, xs)
 where (s,clss,xs) = processCls cs

processCls (c:'-':e:cs) | e /= ']'
                        = (c:'-':e:s, ClsRange (c,e) `ClsUnion` clss, xs)
 where (s,clss,xs) = processCls cs

processCls (c:cs)       = (c:s, ClsChar c `ClsUnion` clss, xs)
 where (s,clss,xs) = processCls cs

isSymClassChar :: Char -> Bool
isSymClassChar = (`elem`"wWdDsS")

symClassPred :: Char -> CharClass
symClassPred  c  | isSymClassChar c  = ClsClass c
                 | otherwise         = ClsChar c

isWordChar :: Char -> Bool
isWordChar c = c == '_' || isAlphaNum c

parseError :: [Token] -> a
parseError _ = error "cannot parse regular expression"
}
