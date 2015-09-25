{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Data.List (foldl')

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty    = Nothing
indexJ i _
  | i < 0 = Nothing
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i jl@(Append _ l r)
  | i >= count = Nothing
  | i < lCount = indexJ i l
  | otherwise  = indexJ (i - lCount) r
  where count = getSize $ size $ tag jl
        lCount =getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl
  | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n jl@(Append _ l r)
  | n >= count = Empty
  | n < lCount = dropJ n l +++ r
  | otherwise  = dropJ (n - lCount) r
  where count = getSize $ size $ tag jl
        lCount =getSize $ size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _
  | n <= 0 = Empty
takeJ _ s@(Single _ _) = s
takeJ n jl@(Append _ l r)
  | n >= count = jl
  | n < lCount = takeJ n l
  | otherwise  = l +++ takeJ (n - lCount) r
  where count = getSize $ size $ tag jl
        lCount =getSize $ size $ tag l

-- Exercise 3

scoreLine :: String -> JoinList (Score, Size) String
scoreLine s = Single (scoreString s, Size 1) s

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldl' (\jl s -> jl +++ scoreLine s) Empty . lines
  line = indexJ
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl
  numLines = getSize . snd. tag
  value = getScore . fst .tag
