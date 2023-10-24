

{-
https://www.daniweb.com/programming/software-development/threads/453667/poker-probability-program

-}


data Card = Card { suit :: Int, rank :: Int }

main :: IO ()
main = do
    let suits = ["...", "Clubs", "Diamonds", "Hearts", "Spades"]
    let ranks = ["...", "Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"]
    let deck = makeDeck
    let flushes = countFlushes deck 1000000
    let twoKind = countTwoKind deck 1000000
    let threeKind = countThreeKind deck 1000000
    let fourKind = countFourKind deck 1000000
    let straight = countStraight deck 1000000
    let straightFlush = countStraightFlush deck 1000000
    let fullHouse = countFullHouse deck 1000000
    putStrLn $ show flushes ++ " flushes \n" ++ show twoKind ++ " two of a kinds \n" ++ show threeKind ++ " three of a kinds \n" ++ show fourKind ++ " four of a kinds \n" ++ show straight ++ " straights \n" ++ show straightFlush ++ " straight flushes \n" ++ show fullHouse ++ " full houses \n\nin 1,000,000 hands"

makeDeck :: [Card]
makeDeck = [Card suit rank | suit <- [0..3], rank <- [1..13]]

printDeck :: [Card] -> IO ()
printDeck cards = mapM_ printCard cards

printCard :: Card -> IO ()
printCard c = do
    let suits = ["...", "Clubs", "Diamonds", "Hearts", "Spades"]
    let ranks = ["...", "Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"]
    putStrLn $ ranks !! rank c ++ " of " ++ suits !! suit c

makeHand :: [Card] -> [Card]
makeHand deck = take 5 $ shuffle deck

isFlush :: [Card] -> Bool
isFlush hand = any (\s -> length (filter (\c -> suit c == s) hand) > 4) [1..4]

isThreeKind :: [Card] -> Int
isThreeKind hand = case count of
    3 -> 2
    4 -> 3
    5 -> 4
    2 -> 1
    _ -> 0
    where
        count = maximum [length (filter (\c -> rank c == r) hand) | r <- [1..13]]

isStraight :: [Card] -> Bool
isStraight hand = all (\(r1, r2) -> r2 == r1 + 1) $ zip ranks (tail ranks)
    where
        ranks = map rank hand

isFullHouse :: [Card] -> Bool
isFullHouse hand = isThreeKind hand == 3 && any (\r -> length (filter (\c -> rank c == r) hand) == 2) ranks
    where
        ranks = map rank hand

shuffle :: [a] -> [a]
shuffle [] = []
shuffle xs = x : shuffle ys
    where
        (x, ys) = removeRandom xs

removeRandom :: [a] -> (a, [a])
removeRandom xs = (xs !! index, take index xs ++ drop (index + 1) xs)
    where
        index = randomR (0, length xs - 1) (mkStdGen 42)

countFlushes :: [Card] -> Int -> Int
countFlushes deck n = length $ filter isFlush $ take n $ map makeHand $ repeat deck

countTwoKind :: [Card] -> Int -> Int
countTwoKind deck n = length $ filter (\hand -> isThreeKind hand == 2) $ take n $ map makeHand $ repeat deck

countThreeKind :: [Card] -> Int -> Int
countThreeKind deck n = length $ filter (\hand -> isThreeKind hand == 3) $ take n $ map makeHand $ repeat deck

countFourKind :: [Card] -> Int -> Int
countFourKind deck n = length $ filter (\hand -> isThreeKind hand == 4) $ take n $ map makeHand $ repeat deck

countStraight :: [Card] -> Int -> Int
countStraight deck n = length $ filter isStraight $ take n $ map makeHand $ repeat deck

countStraightFlush :: [Card] -> Int -> Int
countStraightFlush deck n = length $ filter (\hand -> isStraight hand && isFlush hand) $ take n $ map makeHand $ repeat deck

countFullHouse :: [Card] -> Int -> Int
countFullHouse deck n = length $ filter isFullHouse $ take n $ map makeHand $ repeat deck