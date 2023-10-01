module Main (main) where

import Lib

--main :: IO ()
--main = someFunc

fact n = if n == 0 then 1 else n * fact(n-1)
main = print (fact 5)
