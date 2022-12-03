module Main where
import Data.List(intersect)
import Data.Char(ord, isAsciiUpper, isAsciiLower)

readInput :: IO [(String, String)]
readInput = fmap (\l -> splitAt (length l `div` 2) l) . lines <$> readFile "input/day3.txt"

itemTypePriority :: Char -> Int
itemTypePriority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27
  | otherwise = error $ "Invalid character: " ++ show c

groupElves :: [String] -> [[String]]
groupElves [] = []
groupElves l = take 3 l : groupElves (drop 3 l)

part1 :: [(String, String)] -> Int
part1 = sum . fmap (sum . fmap itemTypePriority . take 1 . uncurry intersect)

part2 :: [(String, String)] -> Int
part2 = sum 
      . fmap (sum 
             . fmap itemTypePriority 
             . take 1 
             . foldr intersect (['a'..'z'] ++ ['A' .. 'Z'])) 
      . groupElves 
      . fmap (uncurry (++))

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)