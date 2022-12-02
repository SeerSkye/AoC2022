module Main where

readInput :: IO [String]
readInput = lines <$> readFile "input/day2.txt"

part1rps :: String -> Int
part1rps (a:' ':b:[])
  | a == 'A' && b == 'X' = 3+1 -- rock ties with rock
  | a == 'A' && b == 'Y' = 6+2 -- paper beats rock
  | a == 'A' && b == 'Z' = 0+3 -- scissors loses to rock
  | a == 'B' && b == 'X' = 0+1 -- rock loses to paper
  | a == 'B' && b == 'Y' = 3+2 -- paper ties with paper
  | a == 'B' && b == 'Z' = 6+3 -- scissors beats paper
  | a == 'C' && b == 'X' = 6+1 -- rock beats scissors
  | a == 'C' && b == 'Y' = 0+2 -- paper loses to scissors
  | a == 'C' && b == 'Z' = 3+3 -- scissors ties with scissors
part1rps e = error $ "invalid strategy guide line: " ++ e

part1 :: [String] -> Int
part1 = sum . fmap part1rps

part2rps :: String -> Int
part2rps (a:' ':b:[])
  | a == 'A' && b == 'X' = 0+3 -- lose to rock means choosing scissors
  | a == 'A' && b == 'Y' = 3+1 -- tie rock means choosing rock
  | a == 'A' && b == 'Z' = 6+2 -- beat rock means choosing paper
  | a == 'B' && b == 'X' = 0+1 -- lose to paper mean choosing rock
  | a == 'B' && b == 'Y' = 3+2 -- tie paper means choosing paper
  | a == 'B' && b == 'Z' = 6+3 -- beat paper means choosing scissors
  | a == 'C' && b == 'X' = 0+2 -- lose to scissors means choose paper
  | a == 'C' && b == 'Y' = 3+3 -- tie scissors means choose scissors
  | a == 'C' && b == 'Z' = 6+1 -- beat scissors means choose rock
part2rps e = error $ "invalid strategy guide line: " ++ e

part2 :: [String] -> Int
part2 = sum . fmap part2rps

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)