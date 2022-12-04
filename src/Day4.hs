module Main where
import Data.Bifunctor

-- Like `break`, but also drops the element it broke the list at
breakAndDrop :: (a -> Bool) -> [a] -> ([a], [a])
breakAndDrop = (second (drop 1) .) . break

both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)

type RangePair = ((Int, Int),(Int, Int))

readInput :: IO [RangePair]
readInput = fmap (both (both read . breakAndDrop (=='-')) . breakAndDrop (==',')) . lines <$> readFile "input/day4.txt"

fullyContains :: RangePair -> Bool
fullyContains ((low1, high1),(low2, high2))
    | low1 >= low2 && high1 <= high2 = True -- range 2 contains range 1
    | low2 >= low1 && high2 <= high1 = True -- range 1 contains range 2
    | otherwise = False

part1 :: [RangePair] -> Int
part1 = length . filter fullyContains

overlaps :: RangePair -> Bool
overlaps ((low1, high1),(low2, high2))
    | low1 <= high2 && high1 >= low2 = True
    | low2 <= high1 && high2 >= low1 = True
    | otherwise = False

part2 :: [RangePair] -> Int
part2 = length . filter overlaps

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)