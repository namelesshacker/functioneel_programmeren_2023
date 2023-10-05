


module egcd
    ( someFunc,euclid
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

euclid::Integer->Integer->Integer
euclid n p
  | n < 0            = 0
  | n `mod` 17 == 2  = -15
  | otherwise        = n + p

--rest = print $ (euclid 2 3)





--main :: IO ()
--main = putStrLn "Test suite not yet implemented"


-- https://serokell.io/blog/introduction-to-template-haskell

-- http://wiki.haskell.org/Template_Haskell

-- https://downloads.haskell.org/~ghc/7.0.2/docs/html/users_guide/template-haskell.html


-- https://srid.ca/haskell-template

-- https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html


-- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/

-- https://github.com/PHPirates/haskell-template-project/blob/master/Setup.hs


-- https://www.joachim-breitner.de/blog/772-Template_Haskell_recompilation

 
-- https://downloads.haskell.org/~ghc/6.0/docs/html/users_guide/template-haskell.html



--opgave 1b

egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
   let (g, s, t) = egcd (b `mod` a) a
   in (g, t - (b `div` a) * s, s)


--let temp =Square 12 Yellow
main = print $ ( egcd 2 3)

aaa x y = let r = 3*x
              s = 6*y
              in  r + s

			  -- modulair inverse
			  -- https://rosettacode.org/wiki/Modular_inverse#Haskell
			  -- https://github.com/acmeism/RosettaCodeData/blob/master/Task/Modular-inverse/Haskell/modular-inverse.hs
			  -- https://github.com/metaleap/rosetta-haskell-dump/blob/master/modular-inverse.hs
			  -- modular inverse in haskell
			  -- https://titanwolf.org/Network/Articles/Article?AID=a9da3c8a-3fb7-4b08-9825-52fa2770c7fa#gsc.tab=0
			  -- https://titanwolf.org/Network/Articles/Article?AID=a9da3c8a-3fb7-4b08-9825-52fa2770c7fa#gsc.tab=0
			  -- modular inverse
			  -- https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
			  -- https://cdsmith.wordpress.com/2009/07/20/calculating-multiplicative-inverses-in-modular-arithmetic/
			  -- https://www.reddit.com/r/haskell/comments/937gj/calculating_multiplicative_inverses_in_modular/
			  -- euclid
			  -- https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
			  -- http://www.jimshapiro.com/haskell_number_theory_mod.html
			  -- https://stackoverflow.com/questions/33326716/modular-inverse-in-haskell

