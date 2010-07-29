{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-missing-signatures #-}

module Parser ( parse ) where

import RegExp
  ( eps, char, psym, anySym, alt, seq_, rep, rep1, opt, brep, CharClass(..) )

import Data.Char ( isSpace, toLower, isAlphaNum, isDigit )

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

action_0 (5) = happyShift action_3
action_0 (9) = happyShift action_4
action_0 (14) = happyShift action_5
action_0 (15) = happyShift action_6
action_0 (4) = happyGoto action_2
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (6) = happyShift action_8
action_2 (7) = happyShift action_9
action_2 (8) = happyShift action_10
action_2 (11) = happyShift action_11
action_2 (12) = happyShift action_12
action_2 (13) = happyShift action_13
action_2 (16) = happyAccept
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (5) = happyShift action_3
action_4 (9) = happyShift action_4
action_4 (14) = happyShift action_5
action_4 (15) = happyShift action_6
action_4 (4) = happyGoto action_7
action_4 _ = happyReduce_1

action_5 _ = happyReduce_10

action_6 _ = happyReduce_11

action_7 (6) = happyShift action_8
action_7 (7) = happyShift action_9
action_7 (8) = happyShift action_10
action_7 (10) = happyShift action_16
action_7 (11) = happyShift action_11
action_7 (12) = happyShift action_12
action_7 (13) = happyShift action_13
action_7 _ = happyFail

action_8 _ = happyReduce_3

action_9 (5) = happyShift action_3
action_9 (9) = happyShift action_4
action_9 (14) = happyShift action_5
action_9 (15) = happyShift action_6
action_9 (4) = happyGoto action_15
action_9 _ = happyReduce_1

action_10 (5) = happyShift action_3
action_10 (9) = happyShift action_4
action_10 (14) = happyShift action_5
action_10 (15) = happyShift action_6
action_10 (4) = happyGoto action_14
action_10 _ = happyReduce_1

action_11 _ = happyReduce_7

action_12 _ = happyReduce_8

action_13 _ = happyReduce_9

action_14 (6) = happyShift action_8
action_14 (7) = happyShift action_9
action_14 (8) = happyShift action_10
action_14 (11) = happyShift action_11
action_14 (12) = happyShift action_12
action_14 (13) = happyShift action_13
action_14 _ = happyReduce_5

action_15 (6) = happyShift action_8
action_15 (7) = happyShift action_9
action_15 (11) = happyShift action_11
action_15 (12) = happyShift action_12
action_15 (13) = happyShift action_13
action_15 _ = happyReduce_4

action_16 _ = happyReduce_6

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 (eps
	)

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyTerminal (Sym happy_var_1))
	 =  HappyAbsSyn4
		 (char happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (rep happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (seq_ happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (alt happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (rep1 happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  4 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (opt happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyTerminal (Bnd happy_var_2))
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (brep happy_var_2 happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 (HappyTerminal (Cls happy_var_1))
	 =  HappyAbsSyn4
		 (psym happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  4 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn4
		 (anySym
	)

happyNewToken action sts stk [] =
	action 16 16 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Sym happy_dollar_dollar -> cont 5;
	Ast -> cont 6;
	Seq -> cont 7;
	Bar -> cont 8;
	L -> cont 9;
	R -> cont 10;
	Pls -> cont 11;
	Que -> cont 12;
	Bnd happy_dollar_dollar -> cont 13;
	Cls happy_dollar_dollar -> cont 14;
	Dot -> cont 15;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseTokens tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
