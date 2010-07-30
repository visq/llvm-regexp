{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RegExpLLVM
-- Copyright   :  (c) 2010 Benedikt Huber
-- License     :  BSD-style
-- Stability   :  Early Demo
-- Portability :  Test with ghc-6.10
-- Dependencies : bytestring, llvm
-- 
-- Demo: Generating LLVM code to match regular expressions
-----------------------------------------------------------------------------
module Text.RegExp.LLVM where
import Prelude hiding (and,or)
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import Data.Char (ord)
import Data.Generics
import Data.Word
import Data.Int
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Loop

import Text.RegExp.LLVM.RegExp

-- count AST nodes fullfilling the given predicate
count :: (Data a) => (Reg a -> Bool) -> Reg a -> Int
count p = synthesize 0 (+) (mkQ (const 0) (\re -> if p re then (1+) else id))

-- the next three functions could certainly be implemented using
-- a more general idioms

countStateIndices :: (Data a) => Reg a -> Int
countStateIndices re = count `flip` re $ \subreg -> case subreg of
  Sym _ _ -> True
  _       -> False

-- number state indices (Sym and Rep)
numberStateIndices :: Reg a -> Reg Int
numberStateIndices = (evalState `flip` 0) . numberM
  where
  ticket = State $ \n -> (n, n+1)
  numberM :: Reg a -> State Int (Reg Int)
  numberM (Sym _ c) = liftM (\t -> (Sym t c)) ticket
  numberM (Rep _ r) = liftM (Rep (-1)) (numberM r)
  numberM (Eps _)   = return (Eps (-1))
  numberM (Opt _ r)  = liftM (Opt (-1)) (numberM r)
  numberM (Alt _ r1 r2) = liftM2 (Alt (-1)) (numberM r1) (numberM r2)
  numberM (Seq _ r1 r2) = liftM2 (Seq (-1)) (numberM r1) (numberM r2)

-- collect final states
finalStates :: Reg Int -> [Int]
finalStates re = case re of
  Sym a _     -> [a]
  Alt _ r1 r2 -> finalStates r1 ++ finalStates r2
  Seq _ r1 r2 -> [ v | v <- finalStates r1, empty r2 ] ++ finalStates r2
  Opt _ r     -> finalStates r
  Eps _       -> []
  Rep _ r     -> finalStates r

-- | Run the given low-level query, which takes an array of characters and returns
-- a boolean value, on the given bytestring
runMatcher :: (Ptr Word8 -> Word32 -> IO Word32) -> BS.ByteString -> IO Bool
runMatcher f bs = do
  let (fptr,offset,len) = toForeignPtr bs
  r <- withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` offset) (fromIntegral len)
  return (r > 0)

-- | Generate code for matching the given regular expression
regexMatcher :: RegExp -> CodeGenModule (Function (Ptr Word8 -> Word32 -> IO Word32))
regexMatcher regexp = do
  createNamedFunction ExternalLinkage "matcher" $ \string len -> do
    let re = numberStateIndices regexp
    
    -- basic block declarations
    top <- getCurrentBasicBlock
    arrayInitStart <- newNamedBasicBlock "arrayinit_start"
    loop <- newNamedBasicBlock "loop"
    body <- newNamedBasicBlock "body"
    computeFinal <- newNamedBasicBlock "computefinal"
    exit <- newNamedBasicBlock "exit"

    -- check for empty string
    let final_empty = valueOf (empty re)
    is_empty <- icmp IntEQ len (w32 0)                                    -- check if len==0
    condBr is_empty exit arrayInitStart                                   -- return if empty
    
    defineBasicBlock arrayInitStart
    let arrSize = fromIntegral (countStateIndices re) :: Word32           -- size of the state array
    arr <- arrayAlloca arrSize :: CodeGenFunction r (Value (Ptr Bool))    -- allocate array
    forLoop (w32 0) (w32 arrSize) () $ \ix _ -> do                        -- memset all elements in array to 0
      arrptr <- getElementPtr arr (ix, ())
      store (valueOf False) arrptr
    arrayInitEnd <- getCurrentBasicBlock                                  -- we are now in a different basic block
    br loop

    defineBasicBlock loop
    first <- phi [(valueOf True, arrayInitEnd)]                           -- initially, first is True
    strIx <- phi [(w32 0, arrayInitEnd)]                                  -- i = 0
    t     <- icmp IntEQ strIx len                                         -- check if i==len
    condBr t computeFinal body                                            -- enter loop if i!=len

    defineBasicBlock body                                                 -- Define the loop body
    strp  <- getElementPtr string (strIx, ())                             -- get character
    ch    <- load (strp :: Value (Ptr (Word8)))                           -- ch = str[i]
    generateRegexpCode re first arr ch                                    -- generate regexp matcher code    
    strIx_next <- add strIx (w32 1)                                       -- add 1 to string index
    addPhiInputs strIx [(strIx_next, body)]
    addPhiInputs first [(valueOf False, body)]                            -- first = false from second iteration on
    br loop                                                               -- and loop

    defineBasicBlock computeFinal
    let finalIxs = finalStates re
    final_non_empty <- genFinalStateCheck finalIxs arr (valueOf False)    -- check whether we are in final state 
    br exit
    
    defineBasicBlock exit
    accept <- phi [(final_empty, top), (final_non_empty,computeFinal)]    -- merge accept for empty and non-empty strings
    (accept32 :: Value Word32) <- zext accept                             -- LLVM bindings (currently) do not have Generic Bool
    ret accept32
  where
    w32 v   = valueOf v :: Value Word32

generateRegexpCode :: Reg Int -> Value Bool -> Value (Ptr Bool) -> Value Word8 -> CodeGenFunction r (Value Bool)
generateRegexpCode re first bitmask ch = genC first re where
  genC :: Value Bool -> Reg Int -> CodeGenFunction r (Value Bool)
  genC next regexp = case regexp of
    Sym n c      -> do  tmp1 <- matchCharSet c ch
                        tmp2 <- and next tmp1
                        let nIx = fromIntegral n :: Word32
                        bp   <- getElementPtr bitmask (nIx, ()) 
                        next' <- load bp
                        store tmp2 bp
                        return next'
    Seq _ r1 r2 -> do next1 <- genC next  r1
                      next2 <- genC next1 r2
                      next2 `or` (if (empty r2) then next1 else valueOf False)
    Alt _ r1 r2 -> do next1 <- genC next r1
                      next2 <- genC next r2
                      tmp   <- next1 `or` next2
                      tmp `or` (if (empty r1 || empty r2) then next else valueOf False)
    Rep n r     -> do next' <- genFinalStateCheck (finalStates r) bitmask next
                      tmp   <- genC next' r
                      tmp `or` next
    Opt _ r     -> do next1 <- genC next r
                      next1 `or` next  
    Eps _       -> do return next

matchCharSet :: CharSet -> Value Word8 -> CodeGenFunction r (Value Bool)
matchCharSet AnyChar _ = return (valueOf True) 
matchCharSet (CharSet cs) ch = go cs where
  go []     = error "empty charset not yet supported"
  go [c]    = icmp IntEQ ch (valueOf (fromIntegral (ord c) :: Word8))
  go (c:cs) = do
    rc <- go [c]
    rcs <- go cs
    rc `or` rcs

genFinalStateCheck :: [Int] -> Value (Ptr Bool) -> Value Bool -> CodeGenFunction r (Value Bool)
genFinalStateCheck [] _ b   = return b
genFinalStateCheck (n:ns) bitset b = do
  bitsetPtr   <- getElementPtr bitset (fromIntegral n :: Word32, ())
  finalState <- load bitsetPtr
  tmp <- b `or` (finalState :: Value Bool)
  genFinalStateCheck ns bitset tmp
