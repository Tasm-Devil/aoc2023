module Day3 where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
    ( ParsecT,
      SourcePos(sourceColumn, sourceLine),
      MonadParsec(lookAhead, try),
      (<|>),
      unPos,
      many,
      optional,
      satisfy,
      getSourcePos )
import Text.Megaparsec.Char ( digitChar, char, newline )
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parseFile)

type Parser = ParsecT Void Text

dayNum :: Int
dayNum = 3

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
type Coord = (Int, Int)

type InputType = ([(Int, Coord)], [(Char, Coord)])

-- Convert SourcePos to (Int, Int)
convertPos :: SourcePos -> Coord
convertPos pos = (unPos . sourceLine $ pos, unPos . sourceColumn $ pos)

-- Parse all numbers with positions in the text
parseNumbersWithPos :: Parser m [(Int, Coord)]
parseNumbersWithPos =
  many
    ( try $ do
        optional (many (satisfy (not . isDigit)))
        num <- decimal
        pos <- getSourcePos
        return (num, convertPos pos)
    )

-- Parse all the positions of the symbols in the text
parseSymbolsWithPos :: Parser m [(Char, Coord)]
parseSymbolsWithPos =
  many
    ( try $ do
        optional (many (digitChar <|> char '.' <|> newline))
        part <- satisfy (\c -> not (isDigit c) && c /= '.' && c /= '\n')
        pos <- getSourcePos
        return (part, convertPos pos)
    )

-- Combine both parsers to parse numbers (with positions) and symbols (only positions) together
parseInput :: Parser m InputType
parseInput = do
  numbers <- lookAhead parseNumbersWithPos
  symbols <- parseSymbolsWithPos
  return (numbers, symbols)

-------------------- SOLVING EASY --------------------

-- Check if two coordinates are adjacent
isAdjacentTo :: Coord -> Coord -> Bool
isAdjacentTo (x1, y1) (x2, y2) =
  abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1 && (x1 /= x2 || y1 /= y2)

-- Check if all coordinates from all digits of a number is adjacent to any of the symbol coordinates
isPartNumber :: [(Char, Coord)] -> (Int, Coord) -> Bool
isPartNumber symbols (number, (x, y)) =
  let isAdjacentToSymbol numCoord = any (isAdjacentTo numCoord . snd)
      numberOfDigits = length . show $ number
      lookupDigitsToTheLeft 0 = False
      lookupDigitsToTheLeft n = ((x, y - n + 1) `isAdjacentToSymbol` symbols) || lookupDigitsToTheLeft (n - 1)
   in lookupDigitsToTheLeft numberOfDigits

-- sulotion to the first problem of day 3
processInputEasy :: InputType -> Int
processInputEasy = foldr (\(a, _) acc -> acc + a) 0 . filterNumbersAdjacentToSymbols
  where filterNumbersAdjacentToSymbols (numbers, symbols) = filter (isPartNumber symbols) numbers

-------------------- SOLVING HARD --------------------

isGearOf :: Coord -> (Int, Coord) -> Bool
isGearOf starCoord (number, (x, y)) =
  let numberOfDigits = length . show $ number
      lookupNumbers 0 = False
      lookupNumbers n = ((x, y - n + 1) `isAdjacentTo` starCoord) || lookupNumbers (n - 1)
   in lookupNumbers numberOfDigits

getNumbersAdjacentToStars :: InputType -> [[Int]]
getNumbersAdjacentToStars (numbers, symbols) = filter (\a -> length a > 1) $ map (getNumbersAdjacentToStar . snd) symbols
  where
    getNumbersAdjacentToStar :: Coord -> [Int]
    getNumbersAdjacentToStar starCoord = map fst $ filter (isGearOf starCoord) numbers

processInputHard :: InputType -> Int
processInputHard = foldr (\gears acc -> acc + product gears) 0 . getNumbersAdjacentToStars . filterStars
  where
    filterStars :: InputType -> InputType
    filterStars (numbers, symbols) = (numbers, filter (\(symbol, _) -> symbol == '*') symbols)

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
