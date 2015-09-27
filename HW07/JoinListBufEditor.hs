module Main where

import JoinList
import Editor

main :: IO ()
main = runEditor editor $ scoreLine "Initial buffer of type JoinList."
