module Main where
import qualified Data.Set as S
import Data.List(tails, findIndex)

readInput :: IO String
readInput = readFile "input/day6.txt"

allDifferent :: Ord a => [a] -> Bool
allDifferent l = length l == S.size (S.fromList l)

part1 :: String -> Maybe Int
part1 = fmap (4 +) . findIndex (allDifferent . take 4) . tails

part2 :: String -> Maybe Int
part2 = fmap (14 +) . findIndex (allDifferent . take 14) . tails

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)