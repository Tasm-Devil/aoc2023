module Day10 where

-- this task sucks! I should use the BFS algorithm. But I am not in the mood today

import Data.Char (chr, isDigit, ord)
import Data.Matrix

-------------------- PARSING --------------------

parseFile :: FilePath -> IO (Matrix Char)
parseFile fp = do
  inputFile <- readFile fp
  let inputLines = fromLists . lines . map (\c -> case c of 
                                                      '7' -> 'A'
                                                      'S' -> '0'
                                                      _ -> c) $ inputFile
  return inputLines

-------------------- SOLVING EASY --------------------

processInputEasy :: Matrix Char -> Int
processInputEasy = undefined

step :: Int -> Int -> Matrix Char -> (Matrix Char, Maybe (Int, Int))
step x y m -- (rows, cols) = (nrows m, ncols m)
  | m ! (x, y) == 'L' && x > 1 && y < ncols m =
      if isDigit $ m ! (x - 1, y)
        then (changeMatrix (-1) 0, Just (x, y))
        else
          if isDigit $ m ! (x, y + 1)
            then (changeMatrix 0 1, Just (x, y))
            else (m, Nothing)
  | m ! (x, y) == 'F' && x < nrows m && y < ncols m =
      if isDigit $ m ! (x + 1, y)
        then (changeMatrix 1 0, Just (x, y))
        else
          if isDigit $ m ! (x, y + 1)
            then (changeMatrix 0 1, Just (x, y))
            else (m, Nothing)
  | m ! (x, y) == 'A' && x < nrows m && y > 1 =
      if isDigit $ m ! (x + 1, y)
        then (changeMatrix 1 0, Just (x, y))
        else
          if isDigit $ m ! (x, y - 1)
            then (changeMatrix 0 (-1), Just (x, y))
            else (m, Nothing)
  | m ! (x, y) == 'J' && x > 1 && y > 1=
      if isDigit $ m ! (x - 1, y)
        then (changeMatrix (-1) 0, Just (x, y))
        else
          if isDigit $ m ! (x, y - 1)
            then (changeMatrix 0 (-1), Just (x, y))
            else (m, Nothing)
  | m ! (x, y) == '|' && x > 1 && x < nrows m =
      if isDigit $ m ! (x - 1, y)
        then (changeMatrix (-1) 0, Just (x, y))
        else
          if isDigit $ m ! (x + 1, 1)
            then (changeMatrix 1 0, Just (x, y))
            else (m, Nothing)
  | m ! (x, y) == '-' && y > 1 && y < ncols m =
      if isDigit $ m ! (x, y - 1)
        then (changeMatrix 0 (-1), Just (x, y))
        else
          if isDigit $ m ! (x, y + 1)
            then (changeMatrix 0 1, Just (x, y))
            else (m, Nothing)
  | otherwise = (m, Nothing)
  where
    changeMatrix dx dy = setElem (chr (1 + ord (m ! (x + dx, y + dy)))) (x, y) m

numberOfNeighbors :: Int -> Int -> Matrix Char -> Int
numberOfNeighbors x y m =
  let (rows, cols) = (nrows m, ncols m)
      above = ((x > 1) && isDigit (m ! (x - 1, y)))
      below = ((x < rows) && isDigit (m ! (x + 1, y)))
      left = ((y > 1) && isDigit (m ! (x, y - 1)))
      right = ((y < cols) && isDigit (m ! (x, y + 1)))
      num = sum $ map fromEnum [above, below, right, left]
   in num

main :: IO ()
main = do
  m <- parseFile smallFile -- "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ"
  let (startx, starty) = (3, 1)
  let (s1b, _) = step startx (starty+1) m
  print s1b

-------------------- SOLVING HARD --------------------

processInputHard :: Matrix Char -> Int
processInputHard = undefined

-------------------- BOILERPLATE --------------------

dayNum :: Int
dayNum = 10

solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = do
  let input = parseFile fp
  Just . processInputEasy <$> input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = do
  let input = parseFile fp
  Just . processInputHard <$> input

smallFile :: FilePath
smallFile = "inputs_2023/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2023/day_" <> show dayNum <> "_large.txt"

easySmall :: IO (Maybe Int)
easySmall = solveEasy smallFile

easyLarge :: IO (Maybe Int)
easyLarge = solveEasy largeFile

hardSmall :: IO (Maybe Int)
hardSmall = solveHard smallFile

hardLarge :: IO (Maybe Int)
hardLarge = solveHard largeFile