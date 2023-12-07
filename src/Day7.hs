module Day7 where

import Control.Applicative (liftA2)
import Data.List (group, sort, sortBy)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parseFile)
import Data.Char (isDigit)
import Data.Function (on)

dayNum :: Int
dayNum = 7

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

type Line = (Hand, Int)
newtype Hand = Hand {getHand :: String}

parseInput :: ParsecT Void Text m [Line]
parseInput = ((,) <$> (Hand <$> some alphaNumChar) <* hspace1 <*> decimal) `sepEndBy1` newline

-------------------- SOLVING EASY --------------------

hasFifeOfKind :: String -> Bool
hasFifeOfKind str = any ((== 5) . length) (group . sort $ str)

hasFourOfKind :: String -> Bool
hasFourOfKind str = any ((== 4) . length) (group . sort $ str)

hasThreeOfKind :: String -> Bool -- finds Full House too
hasThreeOfKind str = any ((== 3) . length) (group . sort $ str)

hasPair :: String -> Bool -- finds also twoPairs and Full House
hasPair str = any ((== 2) . length) (group . sort $ str)

hasFullHouse :: String -> Bool
hasFullHouse = liftA2 (&&) (any ((== 3) . length)) (any ((== 2) . length)) . group . sort

hasTwoPairs :: String -> Bool
hasTwoPairs str = length (filter ((== 2) . length) . group . sort $ str) == 2

instance Show Hand where
  show = getHand

instance Eq Hand where
  (Hand hand1) == (Hand hand2) = hand1 == hand2

-- Define an instance of Ord for String based on the hand evaluation functions
instance Ord Hand where
  compare hand1 hand2
    | (hasFifeOfKind . getHand $ hand1) && not (hasFifeOfKind . getHand $ hand2) = GT
    | not (hasFifeOfKind . getHand $ hand1) && (hasFifeOfKind . getHand $ hand2) = LT
    | (hasFourOfKind . getHand $ hand1) && not (hasFourOfKind . getHand $ hand2) = GT
    | not (hasFourOfKind . getHand $ hand1) && (hasFourOfKind . getHand $ hand2) = LT
    | (hasFullHouse . getHand $ hand1) && not (hasFullHouse . getHand $ hand2) = GT
    | not (hasFullHouse . getHand $ hand1) && (hasFullHouse . getHand $ hand2) = LT
    | (hasThreeOfKind . getHand $ hand1) && not (hasThreeOfKind . getHand $ hand2) = GT
    | not (hasThreeOfKind . getHand $ hand1) && (hasThreeOfKind . getHand $ hand2) = LT
    | (hasTwoPairs . getHand $ hand1) && not (hasTwoPairs . getHand $ hand2) = GT
    | not (hasTwoPairs . getHand $ hand1) && (hasTwoPairs . getHand $ hand2) = LT
    | (hasPair . getHand $ hand1) && not (hasPair . getHand $ hand2) = GT
    | not (hasPair . getHand $ hand1) && (hasPair . getHand $ hand2) = LT
    | otherwise = compareCardByCard (getHand hand1) (getHand hand2)

compareCardByCard :: String -> String -> Ordering
compareCardByCard hand1 hand2 = case (hand1, hand2) of
  ([], _) -> EQ
  (_, []) -> EQ
  (x:xs, y:ys) -> case (x,y) of
    ('A',b) | b /= 'A' -> GT
    (a,'A') | a /= 'A' -> LT
    ('K',b) | b /= 'K' -> GT
    (a,'K') | a /= 'K' -> LT
    ('Q',b) | b /= 'Q' -> GT
    (a,'Q') | a /= 'Q' -> LT
    ('J',b) | b /= 'J' -> GT
    (a,'J') | a /= 'J' -> LT
    ('T',b) | b /= 'T' -> GT
    (a,'T') | a /= 'T' -> LT
    (a, b) | not (isDigit a) && isDigit b -> GT
    (a, b) | isDigit a && not(isDigit b) -> LT
    (a, b) | isDigit a && isDigit b && a /= b -> compare a b
    _ -> compareCardByCard xs ys

--inp = parseFile parseInput largeFile
--sortedHands = sortBy (compare `on` fst) <$> inp
processInputEasy :: [Line] -> Int
processInputEasy = sum . zipWith (\num (_,b) -> num * b) [1..] . sortBy (compare `on` fst)

-------------------- SOLVING HARD --------------------

processInputHard :: [Line] -> Int
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