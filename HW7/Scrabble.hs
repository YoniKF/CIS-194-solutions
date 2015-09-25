-- Exercise 3

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score = scoreHelp . toUpper
  where scoreHelp 'A' = 1
        scoreHelp 'B' = 3
        scoreHelp 'C' = 3
        scoreHelp 'D' = 2
        scoreHelp 'E' = 1
        scoreHelp 'F' = 4
        scoreHelp 'G' = 2
        scoreHelp 'H' = 4
        scoreHelp 'I' = 1
        scoreHelp 'J' = 8
        scoreHelp 'K' = 5
        scoreHelp 'L' = 1
        scoreHelp 'M' = 3
        scoreHelp 'N' = 1
        scoreHelp 'O' = 1
        scoreHelp 'P' = 3
        scoreHelp 'Q' = 10
        scoreHelp 'R' = 1
        scoreHelp 'S' = 1
        scoreHelp 'T' = 1
        scoreHelp 'U' = 1
        scoreHelp 'V' = 4
        scoreHelp 'W' = 4
        scoreHelp 'X' = 8
        scoreHelp 'Y' = 4
        scoreHelp 'Z' = 10
        scoreHelp _   = 0

scoreString :: String -> Score
scoreString = sum . map score
