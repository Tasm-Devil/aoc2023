module Day12 where

-- this code is garbage. It runs forever.

import Data.List (elemIndices, intercalate)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, oneOf, optional, sepEndBy1, some)
import Text.Megaparsec.Char (char, hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Regex.TDFA ((=~))
import Utils (parseFile)

-------------------- PARSING --------------------

type Line = (String, [Int])

parseInput :: ParsecT Void Text m [Line]
parseInput = ((,) <$> some (oneOf ['.', '#', '?']) <* hspace1 <*> some (decimal <* optional (char ','))) `sepEndBy1` newline

-------------------- SOLVING EASY --------------------
numberOfUnknownBroken :: Line -> Int
numberOfUnknownBroken (str, nums) = sum nums - length (filter (== '#') str)

isPlausible :: [Int] -> String -> Bool
isPlausible nums str = str =~ ("^\\.*#{" ++ intercalate "}\\.+#{" (map show nums) ++ "}\\.*$")

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item list = take n list ++ [item] ++ drop (n + 1) list

permutations :: String -> Int -> [String]
permutations str 0 = [str] -- Base case: no more replacements needed
permutations str count =
  let indices = elemIndices '?' str -- Find indices of '?'
      replaceAndRecurse idx = permutations (replaceAtIndex idx '#' str) (count - 1)
   in concatMap replaceAndRecurse indices

{-
findPlausiblePermutations :: Line -> [String]
findPlausiblePermutations (str, nums) =
  let n = numberOfUnknownBroken (str, nums)
      repl '?' = '.'
      repl c = c
   in filter (isPlausible nums) . nub $ map (map repl) $ permutations str n
-}

findPlausiblePermutations :: Line -> [String]
findPlausiblePermutations (str, nums) =
  let repl '?' = '.'
      repl c = c
      possiblePermutations = permutations str $ numberOfUnknownBroken (str, nums)
      filteredPermutations = filter (isPlausible nums) $ map (map repl) possiblePermutations
   in Set.toList $ Set.fromList filteredPermutations -- Convert to Set to remove duplicates efficiently

solve :: [Line] -> [[String]]
solve = map findPlausiblePermutations

processInputEasy :: [Line] -> Int
processInputEasy = sum . map (length . findPlausiblePermutations)

-------------------- SOLVING HARD --------------------

processInputHard :: [Line] -> Int
processInputHard = undefined

-------------------- BOILERPLATE --------------------

dayNum :: Int
dayNum = 12

solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = do
  let input = parseFile parseInput fp
  Just . processInputEasy <$> input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = do
  let input = parseFile parseInput fp
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