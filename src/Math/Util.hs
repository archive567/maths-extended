module Math.Util where

import Control.Applicative
import Data.List
import GHC.Float       (double2Int)

-- Fast variant of floor
floorD :: Double -> Int
floorD x | x < 0     = double2Int x - 1
         | otherwise = double2Int x
{-# INLINE floorD #-}

floorD' :: Double -> Double -> Double
floorD' grain x = fromIntegral (floorD (x / grain)) * grain
{-# INLINE floorD' #-}

roundD :: Double -> Double -> Double
roundD grain x = if frac > 0.5 then whole + grain else whole
    where
      whole = floorD' grain x
      frac = (x - floorD' grain x) / grain

median :: (Ord a, Fractional a) =>  [a] -> a
median xs =
    if odd len
    then xs' !! mid
    else evenMedian
  where
    xs' = sort xs
    len = length xs
    mid = len `div` 2
    evenMedian = (xs' !! mid + xs' !! (mid+1)) / 2

absdev :: (Ord a, Fractional a) =>  [a] -> a
absdev xs = sum $ abs <$> ((-) <$> xs <*> pure (median xs))
