{-# LANGUAGE DeriveDataTypeable #-}

module Statistics.Distribution.Combo
       (ComboDistribution(..), combo, combo', rs, create, comboStandard,
        getComboRandoms)
       where

import Control.Monad (replicateM)
import Control.Monad.Primitive
import Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Laplace as L
import qualified Statistics.Distribution.Normal as N
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC

data ComboDistribution =
  CD {cdSplit :: Double -- probability of normal distribution
     ,cdMean :: Double -- (both normal mean and glue point)
     ,cdStd :: Double -- normal std
     ,cdLaplaceR :: Double -- exponetial deviation right-side
     ,cdLaplaceL :: Double -- exponential deviation left-side
     ,cdSplitLaplace :: Double   -- probability of right-side
     }
  deriving (Eq,Read,Show,Typeable)

instance D.Distribution ComboDistribution where
  cumulative = cumulative
  complCumulative = complCumulative

instance D.ContDistr ComboDistribution where
  density = density
  quantile = quantile

instance D.Mean ComboDistribution where
  mean (CD split m _ r l sl) = 
    (split * m) +
    (1 - split) *
    (sl * r +
     (1 - sl) *
     l +
     m)
  {-# INLINE mean #-}

instance D.Variance ComboDistribution where
  variance (CD split _ s r l sl) = 
    split * s +
    (1 - split) *
    (sl /
     ((1 / l) *
      (1 / l)) +
     (1 - sl) /
     ((1 / r) *
      (1 / r)))
  {-# INLINE variance #-}

instance D.MaybeMean ComboDistribution where
  maybeMean = Just . D.mean

instance D.MaybeVariance ComboDistribution where
  maybeStdDev = Just . D.stdDev
  maybeVariance = Just . D.variance

instance D.ContGen ComboDistribution where
  -- genContVar :: (PrimMonad m) => ComboDistribution -> Gen (PrimState m) -> m Double
  genContVar (CD split m s r l sl) g = 
    do split' <- uniform g
       sl' <- uniform g
       n <- MWC.normal m s g
       e <- MWC.exponential 1 g
       return $
         if split' < split
            then n
            else if sl' < sl
                    then m + e / r
                    else m - e / l

density :: ComboDistribution -> Double -> Double
density (CD split m s r l sl) x = 
  split *
  D.density (N.normalDistr m s) x +
  (1 - split) *
  D.density (L.laplace (1 / r)
                       (1 / l)
                       sl
                       m)
            x

cumulative :: ComboDistribution -> Double -> Double
cumulative (CD split m s r l sl) x = 
  split *
  D.cumulative (N.normalDistr m s)
               x +
  (1 - split) *
  D.cumulative 
    (L.laplace (1 / r)
               (1 / l)
               sl
               m)
    x

complCumulative :: ComboDistribution -> Double -> Double
complCumulative (CD split m s r l sl) x = 
  split *
  D.complCumulative (N.normalDistr m s)
                    x +
  (1 - split) *
  D.complCumulative 
    (L.laplace (1 / r)
               (1 / l)
               sl
               m)
    x

quantile :: ComboDistribution -> Double -> Double
quantile d p
  | p == 0 = -inf
  | p == 1 = inf
  | p == 0.5 = cdMean d
  | p > 0 && p < 1 = x
  | otherwise = error $
                 "Statistics.Distribution.Combo.quantile: p must be in [0,1] range. Got: " ++
                 show p
  where x = 
          D.findRoot d
                     p
                     0
                     (-inf)
                     inf
        inf = 1 / 0

comboStandard :: ComboDistribution
comboStandard = CD 0.5 0.0 1.0 1.0 1.0 0.5

-- | Create the combo distribution.
combo :: Double             -- probability of normal
      -> Double             -- ^ mean
      -> Double             -- ^ stdev
      -> Double             -- ^ right-side lambda
      -> Double            -- ^ left-side lambda
      -> Double            -- ^ probability of right-side
      -> ComboDistribution
combo = CD

{-# INLINE combo #-}

-- | Create the combo distribution from a paramter list.
combo' :: [Double] -> ComboDistribution
combo' (split:m:s:r:l:sl:_) = CD split m s r l sl
combo' _ = comboStandard

{-# INLINE combo' #-}

rs :: PrimMonad m
   => Gen (PrimState m) -> [Double] -> Int -> m [Double]
rs g d n = 
  replicateM n $
  D.genContVar (combo' d)
               g

getComboRandoms :: Int -> IO [Double]
getComboRandoms n = 
  do g <- create
     let d = combo 0.5 0.0 1.2 1.2 1.2 0.5
     replicateM n $
       D.genContVar d g
