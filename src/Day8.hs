module Main where
import Data.List(transpose)

readInput :: IO [[Int]]
readInput = fmap (fmap (read . pure)) . lines <$> readFile "input/day8.txt"

testInput :: [[Int]]
testInput =
    [ [3,0,3,7,3]
    , [2,5,5,1,2]
    , [6,5,3,3,2]
    , [3,3,5,4,9]
    , [3,5,3,9,0]
    ]

-- This whole solution is very inefficient but I don't care enough to make it
-- actually good

rowVisibility :: [Int] -> [Bool]
rowVisibility lst = snd $
    foldr (\x (highestSeen, l) -> 
              if x <= highestSeen 
                  then (highestSeen, False:l)
                  else (x, True:l))
          (-1, [])
          lst

visibleFromRight :: [[Int]] -> [[Bool]]
visibleFromRight = fmap rowVisibility

visibleFromBelow :: [[Int]] -> [[Bool]]
visibleFromBelow = transpose . visibleFromRight . transpose

visibleFromLeft :: [[Int]] -> [[Bool]]
visibleFromLeft = fmap reverse . visibleFromRight . fmap reverse

visibleFromAbove :: [[Int]] -> [[Bool]]
visibleFromAbove = transpose . fmap reverse . visibleFromRight . fmap reverse . transpose

mergeWithOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
mergeWithOr = zipWith (zipWith (||))

mergeVisibilities :: [[Int]] -> [[Bool]]
mergeVisibilities lst = 
    foldr1 mergeWithOr [ visibleFromRight lst
                       , visibleFromLeft lst
                       , visibleFromAbove lst
                       , visibleFromBelow lst
                       ]

part1 :: [[Int]] -> Int
part1 = length . filter id . concat . mergeVisibilities

visibleLeftFrom :: [[Int]] -> (Int, Int) -> [Int]
visibleLeftFrom m (x, y) = reverse $ take x $ m !! y

visibleRightFrom ::  [[Int]] -> (Int, Int) -> [Int]
visibleRightFrom m (x, y) = visibleLeftFrom (fmap reverse m) (length (head m) - 1 - x, y)

visibleAboveFrom :: [[Int]] -> (Int, Int) -> [Int]
visibleAboveFrom m (x, y) = visibleLeftFrom (transpose m) (y, x)

visibleBelowFrom :: [[Int]] -> (Int, Int) -> [Int]
visibleBelowFrom m (x, y) = 
    visibleLeftFrom (reverse <$> transpose m) (length m - 1 - y, x)

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore m (x, y) = product $ fmap countVisible treeDirs
  where
    treeHeight = (m !! y) !! x
    countVisible :: [Int] -> Int
    countVisible l = length $ take (1 + length (takeWhile (< treeHeight) l)) l
    treeDirs = 
        [ visibleLeftFrom m (x, y)
        , visibleRightFrom m (x, y)
        , visibleAboveFrom m (x, y)
        , visibleBelowFrom m (x, y)
        ]

part2 :: [[Int]] -> Int
part2 m = maximum $ fmap (scenicScore m) [(x, y) | x <- [0..length (head m) - 1], y <- [0..length m - 1]]

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)