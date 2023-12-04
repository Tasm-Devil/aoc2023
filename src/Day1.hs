module Day1 where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Utils (parseFile)
import Text.Megaparsec
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char (eol, alphaNumChar)

dayNum :: Int
dayNum = 1

-------------------- PUTTING IT TOGETHER --------------------

solveEasy :: FilePath -> IO (Maybe Int)
solveEasy fp = do
  let input = parseFile parseInput fp
  Just . processInputEasy <$> input

solveHard :: FilePath -> IO (Maybe Int)
solveHard fp = do
  let input = parseFile parseInput fp
  Just . processInputHard <$> input

-------------------- PARSING --------------------

type InputType = [String]

parseInput :: ParsecT Void Text m InputType
parseInput = sepEndBy1 parseLine eol

parseLine :: ParsecT Void Text m String
parseLine = some alphaNumChar

-------------------- SOLVING EASY --------------------
type EasySolutionType = Int

processInputEasy :: InputType -> EasySolutionType
processInputEasy = sum . map firstAndLastDigit
  where
    firstAndLastDigit [] = 0
    firstAndLastDigit str = do
      let digits = filter isDigit str
      case digits of
        [] -> 0
        (x : xs) -> read (x : [last (x : xs)])

-------------------- SOLVING HARD --------------------
type HardSolutionType = EasySolutionType

firstDigit :: String -> String
firstDigit str =
  case str of
    [] -> "0"
    (x : _) | isDigit x -> [x]
    s | "one" `isPrefixOf` s -> "1"
    s | "two" `isPrefixOf` s -> "2"
    s | "three" `isPrefixOf` s -> "3"
    s | "four" `isPrefixOf` s -> "4"
    s | "five" `isPrefixOf` s -> "5"
    s | "six" `isPrefixOf` s -> "6"
    s | "seven" `isPrefixOf` s -> "7"
    s | "eight" `isPrefixOf` s -> "8"
    s | "nine" `isPrefixOf` s -> "9"
    (_ : xs) -> firstDigit xs

lastDigit :: String -> String
lastDigit str =
  case str of
    [] -> "0"
    (x : _) | isDigit x -> [x]
    s | reverse "one" `isPrefixOf` s -> "1"
    s | reverse "two" `isPrefixOf` s -> "2"
    s | reverse "three" `isPrefixOf` s -> "3"
    s | reverse "four" `isPrefixOf` s -> "4"
    s | reverse "five" `isPrefixOf` s -> "5"
    s | reverse "six" `isPrefixOf` s -> "6"
    s | reverse "seven" `isPrefixOf` s -> "7"
    s | reverse "eight" `isPrefixOf` s -> "8"
    s | reverse "nine" `isPrefixOf` s -> "9"
    (_ : xs) -> lastDigit xs

processInputHard :: InputType -> HardSolutionType
processInputHard = sum . map ((read :: String -> Int) . firstAndLastDigit2)
  where
    firstAndLastDigit2 [] = "0"
    firstAndLastDigit2 str = firstDigit str ++ lastDigit (reverse str)

-------------------- BOILERPLATE --------------------
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