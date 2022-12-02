module Main where

readInput :: IO [String]
readInput = lines <$> readFile "input/day2.txt"

part1rps :: String -> Int
part1rps "A X" = 3+1 -- rock ties with rock
part1rps "A Y" = 6+2 -- paper beats rock
part1rps "A Z" = 0+3 -- scissors loses to rock
part1rps "B X" = 0+1 -- rock loses to paper
part1rps "B Y" = 3+2 -- paper ties with paper
part1rps "B Z" = 6+3 -- scissors beats paper
part1rps "C X" = 6+1 -- rock beats scissors
part1rps "C Y" = 0+2 -- paper loses to scissors
part1rps "C Z" = 3+3 -- scissors ties with scissors
part1rps e = error $ "invalid strategy guide line: " ++ e

part1 :: [String] -> Int
part1 = sum . fmap part1rps

part2rps :: String -> Int
part2rps "A X" = 0+3 -- lose to rock means choosing scissors
part2rps "A Y" = 3+1 -- tie rock means choosing rock
part2rps "A Z" = 6+2 -- beat rock means choosing paper
part2rps "B X" = 0+1 -- lose to paper mean choosing rock
part2rps "B Y" = 3+2 -- tie paper means choosing paper
part2rps "B Z" = 6+3 -- beat paper means choosing scissors
part2rps "C X" = 0+2 -- lose to scissors means choose paper
part2rps "C Y" = 3+3 -- tie scissors means choose scissors
part2rps "C Z" = 6+1 -- beat scissors means choose rock
part2rps e = error $ "invalid strategy guide line: " ++ e

part2 :: [String] -> Int
part2 = sum . fmap part2rps

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)