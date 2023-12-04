{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use map with tuple-section" #-}
module Day4 where

import Data.IntSet (IntSet, fromList, intersection, size)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceShowId)
import Text.Megaparsec
import Text.Megaparsec.Char (char, hspace1, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parseFile)

dayNum :: Int
dayNum = 4

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

data Card = Card Int IntSet IntSet deriving (Show)

parseInput :: ParsecT Void Text m InputType
parseInput = parseLine `sepEndBy1` newline

parseLine :: ParsecT Void Text m Card
parseLine = do
  _ <- string "Card"
  _ <- hspace1
  cardId <- decimal
  _ <- char ':'
  _ <- hspace1
  num1 <- parseNumnbers
  _ <- char '|'
  _ <- hspace1
  num2 <- parseNumnbers
  return $ Card cardId (fromList num1) (fromList num2)

parseNumnbers :: ParsecT Void Text m [Int]
parseNumnbers = decimal `sepEndBy1` hspace1

-------------------- SOLVING EASY --------------------

processCards :: [Card] -> [IntSet]
processCards = map (\(Card _ set1 set2) -> set1 `intersection` set2)

processInputEasy :: InputType -> Int
processInputEasy = sum . map (\set -> if size set > 0 then 2 ^ (size set - 1) else 0) . traceShowId . processCards

-------------------- SOLVING HARD --------------------

copyCard :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
copyCard 0 _ liste = liste
copyCard w1 a1 liste = case liste of
  [] -> []
  (w2, a2) : xs -> (w2, a2 + a1) : copyCard (w1 - 1) a1 xs

winCopies :: [(Int, Int)] -> [(Int, Int)]
winCopies liste = case liste of
  [] -> []
  (w, a) : xs -> (w, a) : winCopies (copyCard w a xs)

processInputHard :: InputType -> Int
processInputHard = foldr (\(_, n) acc -> acc + n) 0 . winCopies . flip zip (repeat 1) . map size . processCards

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