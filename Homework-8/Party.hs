{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label forest) = f label (map (treeFold f) forest)

-- Exercise 3

nextLevel :: Employee -> [(GuestList,  GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, mempty)
nextLevel boss ls = (glCons boss without, with)
  where without = maximum $ map snd ls
        with    = maximum $ map fst ls

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5

glShow :: GuestList -> String
glShow (GL es f) = unlines $ ("Total fun: " ++ show f) : map empName es

process :: String -> String
process = glShow . maxFun . read

main :: IO ()
main = readFile "company.txt" >>= putStrLn . process
