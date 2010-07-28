-- Input: 5000 a’s (stdin)
-- Re: (a?){5000}a{5000}
import Text.RegExp
regexp1 = fromString "(a?){5000}a{5000}" 
regexp2 = fromString "a{0,5000}a{5000}"
regexp3 = let a = sym 'a' in rep_exact_list 5000 (opt a) `seq_` rep_exact_list 5000 a
regexp4 = let a = sym 'a' in rep_atmost 5000 a `seq_` rep_exact_list 5000 a
rep_exact_list n r | n == 1    = r
                   | otherwise = r `seq_` (rep_exact_list (n-1) r)
rep_atmost n r | n == 1 = opt r
               | n == 2 = (opt r) `seq_` (opt r)
               | otherwise = let (n1,n2) = ((n+1) `div` 2, n `div` 2)
                             in seq_ (rep_atmost n1 r) (rep_atmost n2 r)
evencs = rep (onec `seq_` onec) `seq_` nocs
  where
    nocs = rep (alt (sym 'a') (sym 'b'))
    onec = nocs `seq_` (sym 'c')

main :: IO ()	
main = do
  inp <- getContents
  let re = evencs
  print (re =~ inp)