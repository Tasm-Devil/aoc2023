{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Foldable (find)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, sepBy, some)
import Text.Megaparsec.Char (letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parseFile)

dayNum :: Int
dayNum = 2

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

data Game = Game Int [Draw] deriving (Show)

data Draw = Draw {red :: !Int, green :: !Int, blue :: !Int} deriving (Show)

parseInput :: ParsecT Void Text m [Game]
parseInput = parseGame `sepBy` newline

parseGame :: ParsecT Void Text m Game
parseGame = do
  gameId <- parseGameId
  Game gameId <$> parseDraws

parseGameId :: ParsecT Void Text m Int
parseGameId = do
  _ <- string "Game "
  gameId <- decimal
  _ <- string ": "
  return gameId

parseDraws :: ParsecT Void Text m [Draw]
parseDraws = do
  parseDraw `sepBy` string "; "

parseDraw :: ParsecT Void Text m Draw
parseDraw = do
  counts <- parseColor `sepBy` string ", "
  let (reds, greens, blues) =
        foldr
          ( \(color, num) (r, g, b) -> case color of
              "red" -> (num, g, b)
              "green" -> (r, num, b)
              "blue" -> (r, g, num)
              _ -> (r, g, b)
          )
          (0, 0, 0)
          counts
  return $ Draw reds greens blues

parseColor :: ParsecT Void Text m (String, Int)
parseColor = do
  num <- decimal
  _ <- space
  color <- some letterChar
  return (color, num)

-------------------- SOLVING EASY --------------------

processInputEasy :: [Game] -> Int
processInputEasy =
  foldr (\(Game n _) acc -> acc + n) 0
    . filter checkGame

checkGame :: Game -> Bool
checkGame (Game _ draws) = isNothing $ find test draws
  where
    test :: Draw -> Bool
    test draw = case draw of
      (Draw r _ _) | r > 12 -> True
      (Draw _ g _) | g > 13 -> True
      (Draw _ _ b) | b > 14 -> True
      _ -> False

-------------------- SOLVING HARD --------------------

processInputHard :: [Game] -> Int
processInputHard = sum . map ((\(x, y, z) -> x * y * z) . maxColorsPerGame)

maxColorsPerGame :: Game -> (Int, Int, Int)
maxColorsPerGame (Game _ draws) =
  foldr
    ( \(Draw r g b) (rMax, gMax, bMax) ->
        ( max r rMax,
          max g gMax,
          max b bMax
        )
    )
    (0, 0, 0)
    draws

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
