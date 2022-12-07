{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text(Text)
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text
import Control.Applicative((<|>), many)
import Data.Either(fromRight)
import Data.List(foldl')

-- Oh this is going to be a fun one
-- So, I want to do this "right" and actually build up the tree structure explored
-- by the input. However, haskell's immutable-by-default paradigm makes it tricky
-- to build a tree like this. We have two options, the simpler one would be to
-- keep track of the current file-path and then add nodes to the tree from the root.
-- The more fun option though, and the one I want to try out, is to build a `Zipper`
-- which is an interesting functional idiom to describe tree-like data structures you
-- can walk though.

-- Everything in the file system is either a file, with a name and a size, or it's a
-- directory which itself contains a list of files/directories
data FileSystem = File Text Int | Directory Text [FileSystem]
    deriving Show

-- We want to create a new data structure that can represent the idea of "walking"
-- up and down this tree. In order to do this we keep track of the path we've walked
-- from the root of the tree, and all the subtrees we've passed along the way

data Path = Top -- Either we're at the top of the tree
    | Node [FileSystem] Path Text [FileSystem]
    deriving Show
-- But at any node we need to keep track of our left siblings, our right siblings, the
-- path back, and the name of the directory we're in

-- With this we can say that a file system location is a FileSystem
-- as well as a path from which we can rebuild the rest of the FileSystem
type FSLocation = (FileSystem, Path)

-- With this we can define some operations

-- cd .. (change the location to the parent directory)
cdUp :: FSLocation -> FSLocation
cdUp (a, Top) = (a, Top) -- We're already at the top, we can't go higher
cdUp (fs, Node lefts path name rights) = 
    (Directory name (lefts ++ fs:rights), path)

-- Move down to a new directory specified by a String.
-- If we can't successfully move, just stay where we are.
cdDir :: Text -> FSLocation -> FSLocation
cdDir _ loc@(File _ _, _) = loc-- can't move down from a file
cdDir newDir loc@(Directory currDir files, path) = 
    case break (named newDir) files of
        (_, []) -> loc -- We didn't find a directory with that name 
        (lefts, x:rights) -> (x, Node lefts path currDir rights)
  where
    named searchName (File name _)      = searchName == name
    named searchName (Directory name _) = searchName == name 

-- Modify the current subtree we're looking at
fsModify :: (FileSystem -> FileSystem) -> FSLocation -> FSLocation
fsModify f (fs, path) = (f fs, path)

-- And some simple functions to help build our filesystem
-- Add a file/directory under the root node of our FileSystem
addDir :: Text -> FileSystem -> FileSystem
addDir _ fs@(File _ _) = fs -- can't add to a file
addDir name (Directory dirName contents) = 
    if any (named name) contents
        then Directory dirName contents
        else Directory dirName (Directory name [] : contents)
  where
    named searchName (File n _)      = searchName == n
    named searchName (Directory n _) = searchName == n

addFile :: Text -> Int -> FileSystem -> FileSystem
addFile _ _ fs@(File _ _) = fs 
addFile name size (Directory dirName contents) =
    Directory dirName (File name size : contents)

-- And some conversion
fsToLoc :: FileSystem -> FSLocation
fsToLoc fs = (fs, Top)

locToFs :: FSLocation -> FileSystem
locToFs (fs, Top) = fs
locToFs loc@(_, Node {}) = locToFs (cdUp loc)

-- Now onto the parsing and building of our FileSystem
-- We'll parse it as a list of commands, then use those to
-- build our filesystem later
data Command = CD Text | AddFile Int Text | AddDir Text
    deriving Show

parseCd :: Parser Command
parseCd = CD <$> ("$ cd " *> takeTill isEndOfLine) <* endOfLine

parseAddFile :: Parser Command
parseAddFile = AddFile <$> decimal <* " " <*> takeTill isEndOfLine <* endOfLine

parseAddDir :: Parser Command
parseAddDir = AddDir <$> ("dir " *> takeTill isEndOfLine) <* endOfLine

parseLs :: Parser [Command]
parseLs = "$ ls\n" *> many (parseAddFile <|> parseAddDir)

parseInput :: Parser [Command]
parseInput = concat <$> ("$ cd /\n" *> many (fmap pure parseCd <|> parseLs))

readInput :: IO [Command]
readInput = 
    fromRight (error "parse failure") . parseOnly parseInput <$> TIO.readFile "input/day7.txt"

doCommand :: Command -> FSLocation -> FSLocation
doCommand (AddFile size name) = fsModify $ addFile name size
doCommand (AddDir name) = fsModify $ addDir name
doCommand (CD name)
    | name == ".." = cdUp
    | otherwise    = cdDir name

makeFileSystem :: [Command] -> FileSystem
makeFileSystem =
    locToFs . foldl' (flip doCommand) (fsToLoc (Directory "/" []))

getTotalSize :: FileSystem -> Int
getTotalSize (File _ size) = size
getTotalSize (Directory _ contents) = sum (getTotalSize <$> contents)

-- this is inefficient and does a bunch of duplicated work but I just kind of want to
-- finish part 1 at this point haha
getDirectoryTotalSizes :: FileSystem -> [Int]
getDirectoryTotalSizes (File _ _) = []
getDirectoryTotalSizes fs@(Directory _ contents) = 
    getTotalSize fs : concatMap getDirectoryTotalSizes contents

part1 :: FileSystem -> Int
part1 = sum . filter (<= 100000) . getDirectoryTotalSizes

part2 :: FileSystem -> Int
part2 fs = minimum
    $ filter (>= 30000000 - (70000000 - getTotalSize fs)) 
             (getDirectoryTotalSizes fs)

main :: IO ()
main = do
  input <- makeFileSystem <$> readInput
  putStrLn $ "Part 1 Solution: " ++ show (part1 input)
  putStrLn $ "Part 2 Solution: " ++ show (part2 input)