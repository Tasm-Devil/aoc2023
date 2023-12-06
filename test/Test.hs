module Main (main) where

import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day6 as D6

main :: IO ()
main = do
  day11 <- D1.easySmall
  day12 <- D1.easyLarge
  day13 <- D1.hardSmall
  day14 <- D1.hardLarge
  day21 <- D2.easySmall
  day22 <- D2.easyLarge
  day23 <- D2.hardSmall
  day24 <- D2.hardLarge
  day31 <- D3.easySmall
  day32 <- D3.easyLarge
  day33 <- D3.hardSmall
  day34 <- D3.hardLarge
  day41 <- D4.easySmall
  day42 <- D4.easyLarge
  day43 <- D4.hardSmall
  day44 <- D4.hardLarge
  day62 <- D6.easyLarge
  day64 <- D6.hardLarge
  defaultMain $ testGroup "Advent of Code Tests"
    [ testCase "Day 1-1" $ day11 @?= Just 209
    , testCase "Day 1-2" $ day12 @?= Just 54331
    , testCase "Day 1-3" $ day13 @?= Just 281
    , testCase "Day 1-4" $ day14 @?= Just 54518 
    , testCase "Day 2-1" $ day21 @?= Just 8
    , testCase "Day 2-2" $ day22 @?= Just 3059
    , testCase "Day 2-3" $ day23 @?= Just 2286
    , testCase "Day 2-4" $ day24 @?= Just 65371
    , testCase "Day 3-1" $ day31 @?= Just 4361
    , testCase "Day 3-2" $ day32 @?= Just 539713
    , testCase "Day 3-3" $ day33 @?= Just 467835
    , testCase "Day 3-4" $ day34 @?= Just 84159075
    , testCase "Day 4-1" $ day41 @?= Just 13
    , testCase "Day 4-2" $ day42 @?= Just 21138
    , testCase "Day 4-3" $ day43 @?= Just 30
    , testCase "Day 4-4" $ day44 @?= Just 7185540
    , testCase "Day 6-2" $ day62 @?= Just 840336
    , testCase "Day 6-4" $ day64 @?= Just 41382569
    ]
