--module Lib
--    ( someFunc
--    ) where
--
--someFunc :: IO ()
--someFunc = putStrLn "someFunc"


{-
do_integral :: (Ord p, Num p, Fractional p) => (p -> p) -> p -> p -> p -> p
do_integral f start end dx
  | end <= start = 0
  | otherwise = (height * dx) + do_integral f (start + dx) end dx
    where
      height = f (start + (dx / 2))

main = print $ do_integral (\x -> x**2) (I (-1,1) 100)
-- https://gist.github.com/jakevossen5/ae3321742556e53d40b7585469c740d5
-}

{-
riemann integration in haskell
https://www.chrishenson.net/article/integral
https://www.schoolofhaskell.com/user/Sam567/computational-physics/beginner-s-tools/simple-integration
https://stackoverflow.com/questions/60381775/computing-left-handed-riemann-sums-in-haskell
https://abailly.github.io/posts/cm-infra-2.html
http://worldcomp-proceedings.com/proc/p2013/FEC7326.pdf
https://rextester.com/discussion/KHC32406/Calculate-Integral-of-function
https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/
-}






--ghc 7.10
 
integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p = a+b+p

intA :: Double -> Double
intA x = x

intB :: Double -> Double
intB x = x    

  
-- calculates one block of the rieman function
riemanBlock :: Double -> Double -> (Double -> Double) -> Double
riemanBlock (intA, intB) f
  | intA > intB     = riemanBlock (intB, intA) f
  | otherwise       = ((f intA) + (f intB)) / 2.0 * (intB-intA)


-- calculates one block of the rieman function
riemanBlock :: Double -> Double -> (Double -> Double) -> Double
riemanBlock (intA, intB) f
  | intA > intB     = riemanBlock (intB, intA) f
  | otherwise       = ((f intA) + (f intB)) / 2.0 * (intB-intA)

-- iterates over all blocks of the rieman sum and gives a list of block sizes
riemanIterator :: [Double] -> (Double -> Double) -> [Double]
riemanIterator [a] _ = []
riemanIterator (p:q:ps) f = (riemanBlock (p, q) f) : (riemanIterator (q:ps) f)

-- calculates the rieman sum for the given partition
riemanSum :: [Double] -> (Double -> Double) -> Double
riemanSum partition f  = sum (riemanIterator partition f)

-- creates a partition of a given interval with n sub-intervals
createPartition :: (Double, Double) -> Double -> [Double]
createPartition (intA, intB) n
  | intA > intB = createPartition (intB, intA) n
  | otherwise   = [intA, intA+(intB-intA)/n .. intB]

-- this does all the work, calculate riemanSums over more and more finely divided partitions until the difference between this one and the last one is below prec
integralIterator :: Double -> Double -> (Double, Double) -> (Double -> Double) -> Double -> Double
integralIterator lastInt lastN (intA, intB) f prec
  | prec > abs (lastInt - currentInt) = currentInt
  | otherwise                         = integralIterator currentInt currentN (intA, intB) f prec
  where currentN = lastN * 2
        currentInt = riemanSum (createPartition (intA, intB) currentN) f

-- calculates the integral of f over the interval (intA, intB). The precision can be set in the let-clause
integral :: (Double, Double) -> (Double -> Double) -> Double
integral (intA, intB) f  = let prec = 0.0000000001
                           in integralIterator (riemanSum [intA, intB] f) 1 (intA, intB) f prec


-- int sin(x) from 0 to pi should be 2
main = print $ integral (0, 3.14159) sin
