module Encrypt
    where
    import Data.Char
    import System.Random
    import Control.Monad
    import System.IO
    import Data.List

    substract a = a-97
    add a = a
    convert a =  map substract $map ord $map toLower a
    convertBack a = map chr$map add a

    getKey string seed = take (length(string)) $randomRs (0, 99) (mkStdGen seed) :: [Int]

    vernam a b = ((a `mod` 10)+(b `mod` 10)) `mod` 10+(a-(a `mod` 10))+(b-(b `mod` 10))

    doEncrypt string seed = encrypt (convert string) (getKey string seed)

    spitOut :: [String] -> String
    spitOut ss = "" ++ (intercalate "" ss) ++ ""

    replace :: Eq a => [a] -> [a] -> [a] -> [a]
    replace [] _ _ = []
    replace s find repl =
     if take (length find) s == find
    then repl ++ (replace (drop (length find) s) find repl)
    else [head s] ++ (replace (tail s) find repl)

    encrypt :: [Int] -> [Int] -> [Int]
    encrypt [] [] = []
    encrypt (b:bs) (x:xs) = vernam b x: encrypt bs xs

    main = do
    text <- readFile("textfile.txt")
    randomNum <- randomRIO(1, 999999)
    let content = lines text
    let a = spitOut content
    let b = doEncrypt (replace a " " "{") randomNum
    let c = convertBack b
    --let key = getKey content randomNum
    print b
    writeFile("key.txt") $show randomNum
    writeFile("writeFile.txt") c
    writeFile("textfile.txt") ""
    return c
