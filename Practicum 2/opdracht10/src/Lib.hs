

module Main where  

  

   

  

data Anniversary = Vierkant String Int Int Int         

  

                 | Rechthoek String String Int Int Int   

  

                 | Diehoek String String Int Int Int  

  

                 | Cirkel String String Int Int Int  

  

message :: String -> String  

  

message name  

  

  | name == "Dave" = "I can't do that."  

  

  | name == "Sam"  = "Play it again."  

  

  | True           = "Hello."  

  

   

  

class Tree t where  

  

    nil   :: t a  

  

    node  :: t a -> a -> t a -> t a  

  

   

  

class  Enum a  where    

  

    succ, pred     :: a -> a    

  

    toEnum         :: Int -> a    

  

    fromEnum       :: a -> Int    

  

    enumFrom       :: a -> [a]            -- [n..]    

  

    enumFromThen   :: a -> a -> [a]       -- [n,n'..]    

  

    enumFromTo     :: a -> a -> [a]       -- [n..m]    

  

    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]        

  

   

  

addThree :: Int -> Int -> Int -> Int    

  

addThree x y z = x + y + z   

  

   

  

   

  

   

  

let anneal_tick :: MotionFunction a -> TransitionProbabilityFunction ->  

  

EnergyFunction a -> Float -> (StdGen,a) -> (StdGen,a)  

  

anneal_tick mf tpf ef t (r,p) = let (r2,p2) = mf r p  

  

(n ,r3) = random r2  

  

in (r3,  

  

if n < tpf (ef p) (ef p2) t  

  

then p2  

  

else p)  

  

let anneal :: EnergyFunction a -> MotionFunction a ->  

  

TransitionProbabilityFunction -> TemperatureFunction -> Int -> StdGen -> a -> a  

  

anneal ef mf tpf tf m r s = snd $ foldl' (flip (anneal_tick mf tpf ef))  

  

(r,s) (map (tf m) [0..m])  

  

random_generator <- getStdGen  

  

putStr "starting annealing... "  

  

putStr "number of annealing steps: "  

  

print annealing_time  

  

let ideal_placement = anneal  

  

(picnicEnergy sitting)  

  

(picnicMotion walking)  

  

picnicTransitionalProbability  

  

picnicTemperature  

  

annealing_time  

  

random_generator  

  

starting_placement  

  

writeFile "tut9.svg" $ writePolygons $ map (similarityLine ideal_placement)  

  

sitting  

  

putStr "Done!\nfinal energy: "  

  

print $ picnicEnergy sitting ideal_placement  

  

putStr "final temperature: "  

  

print $ picnicTemperature 0 annealing_time  

  

   

  

   

  

main :: IO ()  

  

main = putStrLn ("hello")  

  

   
