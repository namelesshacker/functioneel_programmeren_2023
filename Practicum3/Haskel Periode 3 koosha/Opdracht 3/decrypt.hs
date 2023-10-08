module Decrypt
 where
 import Data.Char
 import System.Random
 import Control.Monad
 import System.IO
 import Data.List

 add a = a+97
 convert a =  map ord a
 convertBack a = map chr$map add a

 getKey string seed = take (length(string)) $randomRs (0, 99) (mkStdGen seed) :: [Int]

    --a = encrypted b = key
 vernam a b = ((a `mod` 10)-(b `mod` 10)) `mod` 10+(a-(a `mod` 10))-(b-(b `mod` 10))

 doDecrypt string seed = decrypt (convert string) (getKey string seed)

 spitOut :: [String] -> String
 spitOut ss = "" ++ (intercalate "" ss) ++ ""

 replace :: Eq a => [a] -> [a] -> [a] -> [a]
 replace [] _ _ = []
 replace s find repl =
  if take (length find) s == find
   then repl ++ (replace (drop (length find) s) find repl)
   else [head s] ++ (replace (tail s) find repl)

 decrypt :: [Int] -> [Int] -> [Int]
 decrypt [] [] = []
 decrypt (b:bs) (x:xs) = vernam b x: decrypt bs xs

 main = do
 text <- readFile("writeFile.txt")
 randomNum <- readFile("key.txt")
 let key = read randomNum::Int
 let content = lines text
 let a = spitOut content
 let b = doDecrypt a key
 let c = replace (convertBack b) "{" " "
 writeFile("textfile.txt") c
 return c