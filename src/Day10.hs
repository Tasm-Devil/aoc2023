module Day10 where

import Data.Matrix

-------------------- PARSING --------------------

parseFile :: FilePath -> IO (Matrix Char)
parseFile fp = do
  inputFile <- readFile fp
  let inputLines = fromLists . lines $ inputFile
  return inputLines

-------------------- SOLVING EASY --------------------

nextStep :: Int -> (Int, Int) -> (Int, Int) -> Matrix Char -> Int
nextStep counter (prevRow, prevCol) (thisRow, thisCol) m -- 3 1  3 2 -> 2 2
  | m ! (thisRow, thisCol) == 'L'
      && prevRow == thisRow
      && prevCol == thisCol + 1
      && thisRow - 1 >= 1 =
      go (thisRow - 1, thisCol) -- go up
  | m ! (thisRow, thisCol) == 'L'
      && prevRow == thisRow - 1
      && prevCol == thisCol
      && thisCol + 1 <= cols =
      go (thisRow, thisCol + 1) -- go right
  | m ! (thisRow, thisCol) == 'J'
      && prevRow == thisRow
      && prevCol == thisCol - 1
      && thisRow - 1 >= 1 =
      go (thisRow - 1, thisCol) -- go up
  | m ! (thisRow, thisCol) == 'J'
      && prevRow == thisRow - 1
      && prevCol == thisCol
      && thisCol - 1 >= 1 =
      go (thisRow, thisCol - 1) -- go left
  | m ! (thisRow, thisCol) == 'F'
      && prevRow == thisRow
      && prevCol == thisCol + 1
      && thisRow + 1 <= rows =
      go (thisRow + 1, thisCol) -- go down
  | m ! (thisRow, thisCol) == 'F'
      && prevRow == thisRow + 1
      && prevCol == thisCol
      && thisCol + 1 <= cols =
      go (thisRow, thisCol + 1) -- go right
  | m ! (thisRow, thisCol) == '7'
      && prevRow == thisRow
      && prevCol == thisCol - 1
      && thisRow + 1 <= rows =
      go (thisRow + 1, thisCol) -- go down
  | m ! (thisRow, thisCol) == '7'
      && prevRow == thisRow + 1
      && prevCol == thisCol
      && thisCol - 1 >= 1 =
      go (thisRow, thisCol - 1) -- go left
  | m ! (thisRow, thisCol) == '|'
      && prevRow == thisRow - 1
      && prevCol == thisCol
      && thisRow + 1 <= rows =
      go (thisRow + 1, thisCol) -- go down
  | m ! (thisRow, thisCol) == '|'
      && prevRow == thisRow + 1
      && prevCol == thisCol
      && thisRow - 1 >= 1 =
      go (thisRow - 1, thisCol) -- go up
  | m ! (thisRow, thisCol) == '-'
      && prevRow == thisRow
      && prevCol == thisCol - 1
      && thisCol + 1 <= cols =
      go (thisRow, thisCol + 1) -- go right
  | m ! (thisRow, thisCol) == '-'
      && prevRow == thisRow
      && prevCol == thisCol + 1
      && thisCol - 1 >= 1 =
      go (thisRow, thisCol - 1) -- go left
  | otherwise = counter
  where
    (rows, cols) = (nrows m, ncols m)
    go (nextRow, nextCol) = nextStep (counter + 1) (thisRow, thisCol) (nextRow, nextCol) m

main :: IO ()
main = do
  m <- parseFile largeFile
  -- print m
  let cyclelength = nextStep 0 (25, 94) (26, 94) m
  print . (floor :: Double -> Int) $ (fromIntegral cyclelength + 1) / 2 -- == 6956

-------------------- SOLVING HARD --------------------

processInputHard :: Matrix Char -> Int
processInputHard = undefined

-------------------- BOILERPLATE --------------------

dayNum :: Int
dayNum = 10

smallFile :: FilePath
smallFile = "inputs_2023/day_" <> show dayNum <> "_small.txt"

largeFile :: FilePath
largeFile = "inputs_2023/day_" <> show dayNum <> "_large.txt"
