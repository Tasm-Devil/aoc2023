module Day5 where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parseFile)
import Text.Megaparsec.Char (newline, hspace1)

dayNum :: Int
dayNum = 5

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

type InputType = [Card]

newtype Card = Card Int  deriving (Show)

parseInput :: ParsecT Void Text m InputType
parseInput = parseLine `sepEndBy1` newline

parseLine :: ParsecT Void Text m Card
parseLine = undefined

parseNumbers :: ParsecT Void Text m [Int]
parseNumbers = decimal `sepEndBy1` hspace1

-------------------- SOLVING EASY --------------------

processInputEasy :: InputType -> Int
processInputEasy = undefined

-------------------- SOLVING HARD --------------------

processInputHard :: InputType -> Int
processInputHard = undefined

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