{-# LANGUAGE DeriveDataTypeable #-}

module Statistics.Distribution.Laplace where

import           Data.Typeable (Typeable)
import qualified Statistics.Distribution as D
import           System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC

data LaplaceDistribution =
  LD {ldLambdaR :: Double -- exponetial lambda right-side
     ,ldLambdaL :: Double -- exponential lambda left-side
     ,ldSplit :: Double   -- probability of right-side
     ,ldGluePoint :: Double   -- position of glue point
     }
  deriving (Eq,Read,Show,Typeable)

instance D.Distribution LaplaceDistribution where
  cumulative = cumulative
  complCumulative = complCumulative

instance D.ContDistr LaplaceDistribution where
  density = density
  quantile = quantile

instance D.Mean LaplaceDistribution where
  mean (LD r l s g) = 
    s *
    (1 / r) -
    (1 - s) *
    (1 / l) +
    g
  {-# INLINE mean #-}

instance D.Variance LaplaceDistribution where
  variance (LD r l s _) = 
    s /
    (l * l) +
    (1 - s) /
    (r * r)
  {-# INLINE variance #-}

instance D.MaybeMean LaplaceDistribution where
  maybeMean = Just . D.mean

instance D.MaybeVariance LaplaceDistribution where
  maybeStdDev = Just . D.stdDev
  maybeVariance = Just . D.variance

instance D.ContGen LaplaceDistribution where
  -- genContVar :: (PrimMonad m) => LaplaceDistribution -> Gen (PrimState m) -> m Double
  genContVar (LD r l sl glue) g = 
    do g' <- uniform g
       e <- MWC.exponential 1 g
       return $
         if g' > 1 - sl
            then glue + e * r
            else glue - e * l

cumulative :: LaplaceDistribution -> Double -> Double
cumulative (LD r l sl glue) x
  | x >= glue = 
    (1 - sl) +
    sl *
    (1 -
     exp (-r *
          (x - glue)))
  | otherwise = 
    (1 - sl) *
    exp (-l *
         (glue - x))

{-# INLINE cumulative #-}

complCumulative :: LaplaceDistribution -> Double -> Double
complCumulative d x = 
  1 -
  cumulative d x

{-# INLINE complCumulative #-}

density :: LaplaceDistribution -> Double -> Double
density (LD r l sl glue) x
  | x > glue = 
    sl * r *
    exp (-r *
         (x - glue))
  | otherwise = 
    (1 - sl) *
    l *
    exp (-l *
         (glue - x))

{-# INLINE density #-}

quantile :: LaplaceDistribution -> Double -> Double
quantile (LD r l sl glue) p
  | p == 1 = 1 / 0
  | p <=
      (1 - sl) = 
    glue +
    log (p /
         (1 - sl)) /
    l
  | p >=
      (1 - sl) &&
      p <
      1 = 
    glue -
    log (1 -
         (p - 1 + sl) /
         sl) /
    r
  | otherwise = error $ "Laplace.quantile: p must be in [0,1] range. Got: " ++
                 show p

{-# INLINE quantile #-}

-- | Create a laplace distribution.
laplace :: Double            -- ^ right-side mean
        -> Double            -- ^ left-side mean
        -> Double            -- ^ probability of right-side
        -> Double            -- ^ glue point (between right and left exponetials)
        -> LaplaceDistribution
laplace = LD

{-# INLINE laplace #-}
