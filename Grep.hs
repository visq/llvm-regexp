-- Demo: Generating LLVM code to match regular expressions
-- grep alike (last line)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (ord)
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.File
import System.Environment
import RegExpLLVM
import System.IO.Unsafe

import Parser

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ ioError (userError "Usage: ./RegExpLLVM regexp < file")
    regex <- liftM (parse . head) getArgs

    let matcherCode = regexMatcher regex
    writeCodeGenModule "matcher.bc" matcherCode
    
    initializeNativeTarget
    matches <- liftM ((unsafePerformIO.) . runMatcher) (simpleFunction matcherCode)

    input <- BS.getContents
    forM_ (zip [1..] $ BS.split (fromIntegral (ord '\n')) input) $ \(ix, line) -> do
      when (matches line) $ putStrLn $ "Line " ++ show ix ++ " matches"
