module Day9 where

-------------------- PARSING --------------------

parseFile :: FilePath -> IO [[Int]]
parseFile fp = do
    inputFile <- readFile fp
    let inputLines = map (map (read :: String -> Int) . words) . lines $ inputFile
    return inputLines

-------------------- SOLVING EASY --------------------

diffList :: [Int] -> [Int]
diffList [] = []
diffList [_] = []
diffList (x1 : x2 : rest) = (x2 - x1) : diffList (x2 : rest)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame (x : xs) = all (== x) xs

computeNextInteger :: [Int] -> Int
computeNextInteger lst =
  let differences = diffList lst
   in if allTheSame differences
        then last lst + last differences
        else last lst + computeNextInteger differences

processInputEasy :: [[Int]] -> Int
processInputEasy = sum . map computeNextInteger

-- this is how the programm looked without the boilerplate at the bottom of the file
main :: IO ()
main = do
  inputFile <- readFile "inputs_2023/day_9_large.txt"
  let inputLines = map (map (read :: String -> Int) . words) . lines $ inputFile
  --inputLines <- parseFile largeFile
  let nextIntegers = map computeNextInteger inputLines
  -- mapM_ print nextIntegers
  print . sum $ nextIntegers

-------------------- SOLVING HARD --------------------

processInputHard :: [[Int]] -> Int
processInputHard = undefined

-------------------- BOILERPLATE --------------------

dayNum :: Int
dayNum = 9

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