module Day21 where

import Data.Matrix (Matrix (nrows, ncols), fromLists, safeGet, getElem)
import Data.Set as Set

-------------------- PARSING --------------------

type Coord = (Int, Int)

parseFile :: FilePath -> IO (Matrix Char)
parseFile fp = do
  inputFile <- readFile fp
  let inputLines = fromLists . lines $ inputFile
  return inputLines

-------------------- SOLVING EASY --------------------

possibleNeighborsOf :: Matrix Char -> Coord -> Set Coord
possibleNeighborsOf matrix (x, y) = Set.filter filterGardenPlots all4Neighbors
  where
    all4Neighbors :: Set Coord
    all4Neighbors = Set.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    filterGardenPlots :: Coord -> Bool
    filterGardenPlots co = case uncurry safeGet co matrix of
      Just c | c `elem` ".S" -> True
      _ -> False

nextStep :: Matrix Char -> Set Coord -> Set Coord
nextStep matrix = Set.foldr foldPred Set.empty
  where
    foldPred :: Coord -> Set Coord -> Set Coord
    foldPred c acc = Set.union acc $ possibleNeighborsOf matrix c

solve :: Int -> Matrix Char -> Set Coord -> (Matrix Char -> Set Coord -> Set Coord) -> Set Coord
solve 0 _ start _ = start
solve num matrix start fun = solve (num - 1) matrix (fun matrix start) fun

findStart :: Matrix Char -> Coord
findStart mat = case findElem 'S' mat 1 1 of
    Just a -> a
    Nothing -> error "Character 'S' not found in the matrix"
  where
    findElem :: Eq a => a -> Matrix a -> Int -> Int -> Maybe Coord
    findElem c m i j
      | i > nrows m = Nothing
      | j > ncols m = findElem c m (i + 1) 1
      | getElem i j m == c = Just (i, j)
      | otherwise = findElem c m i (j + 1)

processInputEasy :: Matrix Char -> Int
processInputEasy matrix = length $ solve 100 matrix (Set.fromList [start]) nextStep
  where 
    start = findStart matrix

-------------------- SOLVING HARD --------------------

processInputHard :: Matrix Char -> Int
processInputHard = undefined

-------------------- BOILERPLATE --------------------

dayNum :: Int
dayNum = 21

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