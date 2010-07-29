import Text.RegExp

manyas = fromString "(a?){500}a{500}" 
evencs = rep (onec `seq_` onec) `seq_` nocs
  where
    nocs = rep (alt (sym 'a') (sym 'b'))
    onec = nocs `seq_` (sym 'c')

main :: IO ()	
main = do
  inp <- getContents
  print (evencs =~ inp)