
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<|>))
import Control.Monad((>=>), replicateM_, forM_)
import Control.Monad.Trans.State
import Data.Attoparsec.Text hiding (take)
import Data.Either(fromRight)
import Data.Foldable(toList)
import Data.List(transpose)
import Data.Maybe(catMaybes, listToMaybe, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Text.IO as TIO

data Instruction = Move Int Int Int
  deriving Show

stackLetter :: Parser (Maybe Char)
stackLetter = Just <$> (char '[' *> anyChar <* char ']')

stackBlank :: Parser (Maybe Char)
stackBlank = Nothing <$ "   "

stackRow :: Parser [Maybe Char]
stackRow = (stackLetter <|> stackBlank) `sepBy1` char ' ' <* endOfLine

stack :: Parser (Seq [Char])
stack = S.fromList . fmap catMaybes . transpose <$> many1 stackRow

instruction :: Parser Instruction
instruction = Move 
    <$> ("move " *> decimal)
    <*> (" from " *> decimal) 
    <*> (" to " *> decimal) 
    <*  (endOfLine <|> endOfInput)

inputParser :: Parser (Seq [Char], [Instruction])
inputParser = (,) 
    <$> stack <* skipMany (notChar 'm')
    <*> many1 instruction 
    <* endOfInput

readInput :: IO (Seq [Char], [Instruction])
readInput = 
    fromRight (error "parse failure") . parseOnly inputParser <$> TIO.readFile "input/day5.txt"

popFrom :: Int -> State (Seq [Char]) (Maybe Char)
popFrom i = do
    c <- (S.lookup i >=> listToMaybe) <$> get
    modify (S.adjust (drop 1) i)
    pure c

pushTo :: Int -> Char -> State (Seq [Char]) ()
pushTo i c = modify (S.adjust (c:) i)

doInstruction :: Instruction -> State (Seq [Char]) ()
doInstruction (Move cnt from to) = replicateM_ cnt $ do
    mc <- popFrom (from - 1)
    case mc of
        Just c -> pushTo (to - 1) c
        Nothing -> pure ()

part1 :: (Seq [Char], [Instruction]) -> [Char]
part1 (startState, instructions) =
    toList $ head <$> execState (forM_ instructions doInstruction) startState

doInstruction2 :: Instruction -> State (Seq [Char]) ()
doInstruction2 (Move cnt from to) = do
    cs <- take cnt . fromMaybe [] . S.lookup (from - 1) <$> get
    modify (S.adjust (drop cnt) (from - 1))
    modify (S.adjust (cs ++) (to - 1))

part2 :: (Seq [Char], [Instruction]) -> [Char]
part2 (startState, instructions) =
    toList $ head <$> execState (forM_ instructions doInstruction2) startState

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)