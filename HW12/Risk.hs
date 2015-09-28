{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad (liftM, liftM2, replicateM)
import Control.Monad.Random
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- Exercise 2

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = liftM (outcome . uncurry zip . mapPair (sortBy (flip compare) . map unDV)) (liftM2 (,) (dice a) (dice d))
  where a = min 3 (att - 1)
        d = min 2 def
        outcome fights = Battlefield (att - deadAtt) (def - length fights + deadAtt)
          where deadAtt = length . filter (== GT) . map (uncurry compare) $ fights

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def)
  | att < 2 || def == 0 = return bf
  | otherwise           = battle bf >>= invade

-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb bf = liftM ((/ fromIntegral times) . fromIntegral . length . filter success) (replicateM times (invade bf))
  where times = 1000
        success (Battlefield _ def) = def == 0
