euclid::Integer->Integer->Integer
euclid n p
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + p

rest = print $ (euclid 2 3)

main :: IO ()
main =  print $ (euclid 2 3)
-- >ghc --make -O2 euclid.hs -o test