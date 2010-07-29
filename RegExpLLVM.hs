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
import Prelude hiding (and,or)
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import Data.Char (ord)
import Data.Word
import Data.Int
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Loop
import LLVM.Util.File
import System.IO.Unsafe
import Text.Printf

data Reg c = Eps
         | Sym c
         | Opt (Reg c)
         | Alt (Reg c) (Reg c)
         | Seq (Reg c) (Reg c)
         | Rep (Reg c)
         deriving (Show,Read)
         
rep :: Int -> Reg c -> Reg c
rep 0 _ = Eps
rep 1 r = r
rep n r = r `Seq` (rep (n-1) r)

empty :: Reg c -> Bool
empty r = case r of
  Sym _ -> False
  Alt r1 r2 -> empty r1 || empty r2
  Seq r1 r2 -> empty r1 && empty r2
  _ -> True

count :: Reg c -> Int
count r = case r of
  Sym _ -> 1
  Alt r1 r2 -> count r1 + count r2
  Seq r1 r2 -> count r1 + count r2
  Opt r -> count r
  Eps -> 0
  Rep r -> count r

number :: Reg c -> Reg (c, Int)
number = (evalState `flip` 0) . numberM
  where
    ticket = State $ \s -> (s,s+1)
    numberM r = case r of
      Sym c -> liftM (Sym . (,) c) ticket
      Alt r1 r2 -> liftM2 Alt (numberM r1) (numberM r2)
      Seq r1 r2 -> liftM2 Seq (numberM r1) (numberM r2)
      Opt r     -> liftM Opt (numberM r)
      Eps       -> return Eps
      Rep r     -> liftM Rep (numberM r)

finalStates :: Reg (c,Int) -> [Int]
finalStates r = case r of
  Sym (_,n) -> [n]
  Alt r1 r2 -> finalStates r1 ++ finalStates r2
  Seq r1 r2 -> [ v | v <- finalStates r1, empty r2 ] ++ finalStates r2
  Opt r     -> finalStates r
  Eps       -> []
  Rep r     -> finalStates r

-- | Run the given low-level query, which takes an array of characters and returns
-- a boolean value, on the given bytestring
runOnString :: (Ptr Word8 -> Word32 -> IO Word32) -> BS.ByteString -> IO Bool
runOnString f bs = do
  let (fptr,offset,len) = toForeignPtr bs
  r <- withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` offset) (fromIntegral len)
  return (r > 0)

-- | Generate code for matching the given regular expression
regexMatcher :: Reg Char -> CodeGenModule (Function (Ptr Word8 -> Word32 -> IO Word32))
regexMatcher re = do
  createNamedFunction ExternalLinkage "matcher" $ \string len -> do
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
    let arrSize = fromIntegral (count re) :: Word32                       -- size of the state array
    arr <- arrayAlloca arrSize :: CodeGenFunction r (Value (Ptr Word32))  -- allocate array
    forLoop (w32 0) (w32 arrSize) () $ \ix _ -> do                        -- memset all elements in array to 0
      ap <- getElementPtr arr (ix, ())
      store (valueOf 0) ap
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
    let finalIxs = finalStates $ number re
    final_non_empty <- genFinalStateCheck finalIxs arr (valueOf False)    -- check whether we are in final state 
    br exit
    
    defineBasicBlock exit
    accept <- phi [(final_empty, top), (final_non_empty,computeFinal)]    -- merge accept for empty and non-empty strings
    (accept32 :: Value Word32) <- zext accept                             -- LLVM bindings (currently) do not have Generic Bool
    ret accept32
  where
    w32 v   = valueOf v :: Value Word32

generateRegexpCode :: Reg Char -> Value Bool -> Value (Ptr Word32) -> Value Word8 -> CodeGenFunction r (Value Bool)
generateRegexpCode re first bitmask ch = genC first (number re) where
  genC :: Value Bool -> Reg (Char,Int) -> CodeGenFunction r (Value Bool)
  genC next r = case r of
    Sym (c,n) -> do tmp1 <- icmp IntEQ ch (valueOf (fromIntegral (ord c) :: Word8))
                    tmp2 <- and next tmp1
                    let nIx = fromIntegral n :: Word32
                    bp   <- getElementPtr bitmask (nIx, ()) 
                    r <- load bp >>= trunc
                    tmp3 <- zext tmp2 :: CodeGenFunction r (Value Word32)
                    store tmp3 bp
                    return r
    Seq r1 r2 -> do next1 <- genC next  r1
                    next2 <- genC next1 r2
                    next2 `or` (if (empty r2) then next1 else valueOf False)
    Alt r1 r2 -> do next1 <- genC next r1
                    next2 <- genC next r2
                    tmp   <- next1 `or` next2
                    tmp `or` (if (empty r1 || empty r2) then next else valueOf False)
    Rep r     -> do next' <- genFinalStateCheck (finalStates r) bitmask next
                    tmp   <- genC next' r
                    tmp `or` next
    Opt r     -> do next1 <- genC next r
                    next1 `or` next  
    Eps       -> do return next

genFinalStateCheck :: [Int] -> Value (Ptr Word32) -> Value Bool -> CodeGenFunction r (Value Bool)
genFinalStateCheck [] _ b   = return b
genFinalStateCheck (n:ns) arr b = do
  ap   <- getElementPtr arr (fromIntegral n :: Word32, ())
  finalState <- load ap >>= trunc
  tmp <- b `or` (finalState :: Value Bool)
  genFinalStateCheck ns arr tmp

evencs :: Reg Char
evencs = Seq (Rep (Seq onec onec)) nocs where
  nocs = Rep (Alt (Sym 'a') (Sym 'b'))
  onec = Seq nocs (Sym 'c')

manyas :: Reg Char
manyas = (rep 500 (Opt (Sym 'a'))) `Seq` (rep 500 (Sym 'a'))

main :: IO ()
main = do
    let matcherCode = regexMatcher evencs
    writeCodeGenModule "matcher.bc" matcherCode
    
    initializeNativeTarget
    matches <- liftM ((unsafePerformIO.) . runOnString) (simpleFunction matcherCode)

    input <- BS.getContents
    forM_ (BS.split (fromIntegral (ord '\n')) input) $ \line -> do
      if matches line
        then putStrLn "Match"
        else putStrLn "No Match"
    return ()