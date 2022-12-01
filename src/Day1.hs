module Main where
import Data.List(sortBy)

-- basically copied from the definition of `lines`
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn f lst = case break f lst of 
  (pre, rest) -> pre : case rest of
                         []      -> []
                         _:rest' -> splitOn f rest'

readInput :: IO [[Integer]]
readInput = fmap (fmap read) . splitOn (==[]) . lines <$> readFile "input/day1.txt"

part1 :: [[Integer]] -> Integer
part1 = maximum . fmap sum

part2 :: [[Integer]] -> Integer
part2 = sum . take 3 . sortBy (flip compare) . fmap sum

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)