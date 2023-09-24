


module MIMA
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



module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"



--differential quotient
differentieer::(Double->Double)->Double->Double->Double
differentieer f p x  = x - (f x)/(f  p)





{-



linecut :: Function -> Interval -> Double
linecut f (a,b) = (a*f(b)-b*f(a))/(f(b)-f(a))

newtonit :: Function -> Function -> Function
newtonit f df x = x - (f x)/(df x)


secant :: Double -> Function -> Function
secant h f x = (f (x+h) - f x)/h



secantseq :: Double -> Function -> Double -> [Double]
secantseq h f x = (secant h f x) : secantseq (h/2) f x



ynext :: Double -> Double -> Double -> Function2 -> Double
ynext h x y f = y + (h * (f x y))


eulerseq :: Double -> Double -> Double -> Function2 -> [Double]
eulerseq h x y f = w : (eulerseq h (x+h) w f)
where w = (ynext h x y f)



euler :: Double -> Double -> Double -> Function2 -> Int -> Double
euler a b y0 f n = (eulerseq h a y0 f)!!n
where h = (b-a)/fromIntegral(n)



k1 :: Double -> Double -> Function2 -> Double
k1 x y f = f x y
k2 :: Double -> Double -> Double -> Function2 -> Double
k2 h x y f = f (0.5*h+x) (y+0.5*(k1 x y f)*h)
k3 :: Double -> Double -> Double -> Function2 -> Double
k3 h x y f = f (0.5*h+x) (y+0.5*(k2 h x y f)*h)
k4 :: Double -> Double -> Double -> Function2 -> Double
k4 h x y f = f (x+h) (y+(k3 h x y f) *h)



ksum :: Double -> Double -> Double -> Function2 -> Double
ksum h x y f = (k1 x y f) + 2*(k2 h x y f)
+ 2*(k3 h x y f) + (k4 h x y f)



rk4 :: Double -> Double -> Double -> Function2 -> Double
rk4 h x y f = y + (1/6)*h*(ksum h x y f)



rk4seq :: Double -> Double -> Double -> Function2 -> [Double]
rk4seq h x y f = w: rk4seq h (x+h) w f
where w = (rk4 h x y f)



rk4meth :: Double -> Double -> Double -> Function2
-> Int -> Double
rk4meth a y0 b f n = (rk4seq h a y0 f)!!n
where h = (b-a)/fromIntegral(n)




-}
