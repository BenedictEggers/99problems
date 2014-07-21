-- Ben Eggers <ben.eggers36@gmail.com>
-- Problems 21-28

module EvenMoreLists where

import System.Random

-- Problem 21
insertAt y [] 1 = [y]
insertAt y [] _ = error "That doesn't even make sense"
insertAt y (x:xs) n
    | (< 1) n = error "That doesn't even make sense"
    | (== 1) n = y:x:xs
    | otherwise = x:(insertAt y xs (n - 1))

-- Problem 22
range' bot top
    | bot > top = reverse (range' top bot)
    | bot == top = [top]
    | bot < top  = bot:range' (bot + 1) top

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    g <- getStdGen
    return $ take n [xs !! i | i <- randomRs (0, (length xs) - 1) g]

-- Problem 24


-- Problem 25


-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ combinations n xs

-- Problem 27


-- Problem 28a
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = (lsort (filter (\l -> length l < length x) xs)) ++
               [x] ++
               (lsort (filter (\l -> length l >= length x) xs))

-- Problem 28b
lfsort :: [[a]] -> [[a]]
lfsort xs = concat $ lsort (groupByLength (lsort xs))
    where groupByLength [] = []
          groupByLength (x:xs) =
            (x : (takeWhile (\l -> length l == length x) xs)) :
            groupByLength (dropWhile (\l -> length l == length x) xs)