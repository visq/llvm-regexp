-- Quick'n'Dirty LLVM Regexp matcher
import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.Word
import Data.Int
import Foreign.Marshal.Array
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.FFI.ExecutionEngine (linkInJIT)
import LLVM.Util.Loop
import LLVM.Util.File
import Text.Printf
import Prelude hiding (and,or)
import Debug.Trace

data Reg c = Eps
         | Sym c
         | Opt (Reg c)
         | Alt (Reg c) (Reg c)
         | Seq (Reg c) (Reg c)
         | Rep (Reg c)

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

getcProto :: CodeGenModule (Function (IO Int32))
getcProto = newNamedFunction ExternalLinkage "getchar_unlocked"  

dumpProto :: CodeGenModule (Function (Ptr Word32 -> Word32 -> Bool -> IO ()))
dumpProto =  newNamedFunction ExternalLinkage "matcher_dump"  

-- No Generic instance for Bool
getcMatcher :: Reg Char -> CodeGenModule (Function (IO Word32))
getcMatcher re = do
  getc <- getcProto
  dump <- dumpProto
  createNamedFunction ExternalLinkage "matcher" $ do

    arr <- arrayAlloca arrSize :: CodeGenFunction r (Value (Ptr Word32))  -- allocate arr
    forLoop (v32 0) (v32 arrSize) () $ \ix _ -> do                        -- memset all elements in arr to 0
      ap <- getElementPtr arr (ix, ())
      store (valueOf 0) ap

    -- a_init <- getElementPtr arr (v32 0, ())                               -- a -> last buffer
    -- b_init <- getElementPtr arr (v32 (fromIntegral$ count re), ())        -- b -> current buffer

    top <- getCurrentBasicBlock                                           -- loop initialization stuff
    loop <- newBasicBlock
    loop_r <- newBasicBlock
    [body,body2] <- replicateM 2 newBasicBlock
    exit <- newBasicBlock
    br loop

    defineBasicBlock loop
    first <- phi [(valueOf True, top)]                                    -- initially, first is True
    final <- phi [(valueOf (if empty re then True else False), top)]      -- initially, top is True if re accepts eps
    ch  <- invoke loop_r exit getc                                        -- invoke getc to get one character
  
    defineBasicBlock loop_r
    t <- icmp IntSLT ch (valueOf (0 :: Int32))
    condBr t exit body

    defineBasicBlock body                                                 -- Define the loop body
    final' <- genFinalStateCheck (finalStates $ number re) arr (valueOf False) -- check whether we are in final state 
    generateRegexpCode re first arr ch                                    -- generate regexp matcher code
    --invoke body2 exit dump arr (valueOf arrSize) final'
    br body2
    
    defineBasicBlock body2
    addPhiInputs first [(valueOf False, body2)]                          -- first = false from second iteration on
    addPhiInputs final [(final', body2)]                                 
    br loop                                                              -- And loop
     
    defineBasicBlock exit
    
    final_w32 <- zext final                                               -- LLVM bindings currently do not have Generic Bool
    ret $ (final_w32 :: Value Word32)
  where
    arrSize = fromIntegral (count re) :: Word32
    v32 v = valueOf v :: Value Word32

generateRegexpCode :: Reg Char -> Value Bool -> Value (Ptr Word32) -> Value Int32 -> CodeGenFunction r (Value Bool)
generateRegexpCode re first bitmask ch = genC first (number re) where
  genC :: Value Bool -> Reg (Char,Int) -> CodeGenFunction r (Value Bool)
  genC next r = case r of
    Sym (c,n) -> do tmp1 <- icmp IntEQ ch (valueOf (fromIntegral (ord c) :: Int32))
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

genFinalStateCheck :: [Int] -> Value (Ptr Word32) -> Value Bool -> CodeGenFunction r (Value Bool)
genFinalStateCheck [] _ b   = return b
genFinalStateCheck (n:ns) arr b = do
  ap   <- getElementPtr arr (fromIntegral n :: Word32, ())
  finalState <- load ap >>= trunc
  tmp <- b `or` (finalState :: Value Bool)
  genFinalStateCheck ns arr tmp
    
ab = (Seq (Sym 'a') (Sym 'b'))
aorb = (Alt (Sym 'a') (Sym 'b'))

-- ((a|b)*c(a|b)*c)*(a|b)
evencs :: Reg Char
evencs = Seq (Rep (Seq onec onec)) nocs
nocs = Rep (Alt (Sym 'a') (Sym 'b'))
onec = Seq nocs (Sym 'c')

main = do
    let matcher = getcMatcher evencs
    writeCodeGenModule "matcher.bc" matcher

    linkInJIT
    stdoutMatcher <- do
      m <- newNamedModule "matcher"
      func <- defineModule m matcher
      prov <- createModuleProviderForExistingModule m
      runEngineAccess $ do
        addModuleProvider prov
        generateFunction func    

    r <- stdoutMatcher
    if r>0
      then putStrLn "Match"
      else putStrLn "No Match"
    return ()