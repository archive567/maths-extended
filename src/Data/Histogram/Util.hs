{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Data.Histogram.Util where

import Control.Foldl (fold)
import qualified Control.Foldl as L
import Data.Histogram (Histogram)
import qualified Data.Histogram as H
import Data.Histogram.Fill
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import Statistics.Distribution
import qualified Statistics.Distribution.Combo as OC
import Statistics.Distribution.Exponential
import Statistics.Distribution.Gamma
import qualified Statistics.Distribution.Laplace as OL
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson

bin :: BinD
bin = binDn (-10.5) 1 10.5

hist :: BinD -> V.Vector Double -> H.Histogram BinD Double
hist b = 
  fillBuilderVec (forceDouble -<< mkSimple b)

-- | make a percentile histogram
perc :: Histogram BinD Double -> Histogram BinD Double
perc h = H.map (\x -> x / H.sum h) h

-- reversing out mean and stdev
-- calculating mean using the trick that the Bin boundaries are constructed to be isomorphic to actual values
meanH :: Histogram BinD Double -> Double
meanH h = 
  let t = H.sum h
  in H.bfoldl step begin h /
     t
  where begin = 0.0
        step b bv v = b + bv * v

meanSqH :: Histogram BinD Double -> Double
meanSqH h = 
  let t = H.sum h
  in H.bfoldl step begin h /
     t
  where begin = 0.0
        step b bv v = 
          b +
          (bv ^
           (2 :: Int)) *
          v

type Pdf = BinD -> [Double] -> V.Vector Double

pdfBin :: (Double -> Double) -> BinD -> V.Vector Double
pdfBin cdf h = 
  V.map (\(x,y) -> cdf y - cdf x) binsV
  where binsV = 
          binsList h :: V.Vector (Double,Double)

pdfBinNormal :: Pdf
pdfBinNormal b [] = 
  pdfBin (cumulative $
          normalDistr 0.0 1.0)
         b
pdfBinNormal b [s] = 
  pdfBin (cumulative $
          normalDistr 0.0 s')
         b
  where s' = max 1.0e-5 s
pdfBinNormal b (m:s:_) = 
  pdfBin (cumulative $
          normalDistr m s')
         b
  where s' = max 1.0e-5 s

pdfBinLaplace :: Pdf
pdfBinLaplace b (r:l:sl:glue:_) = 
  pdfBin (cumulative $
          OL.laplace r' l' sl' glue)
         b
  where l' = max 1.0e-5 l
        r' = max 1.0e-5 r
        sl' = max 1.0e-4 sl
pdfBinLaplace _ _ = mempty

pdfBinExponential :: Pdf
pdfBinExponential b (e:_) = 
  pdfBin (cumulative $ exponential e') b
  where e' = max 1.0e-5 e
pdfBinExponential _ _ = mempty

pdfBinPoisson :: Pdf
pdfBinPoisson b (e:_) = 
  pdfBin (cumulative $ poisson e') b
  where e' = max 1.0e-5 e
pdfBinPoisson _ _ = mempty

pdfBinGamma :: Pdf
pdfBinGamma b (m:k:v:_) = 
  V.map (m *) $
  pdfBin (cumulative $
          gammaDistr k' v')
         b
  where k' = max 1.0e-5 k
        v' = max 1.0e-5 v
pdfBinGamma _ _ = mempty

pdfBinCombo :: Pdf
pdfBinCombo b (split:m:s:r:l:sl:_) = 
  pdfBin (cumulative $
          OC.combo split' m s' r' l' sl')
         b
  where split' = max 1.0e-5 split
        s' = max 1.0e-5 s
        l' = max 1.0e-5 l
        r' = max 1.0e-5 r
        sl' = max 1.0e-4 sl
pdfBinCombo _ _ = mempty

err :: Histogram BinD Double -> Pdf -> [Double] -> Double
err h pdf p = 
  do let pdfT = pdf (H.bins h) p
         pdfE = H.histData (perc h)
         errV = 
           zipWith (-)
                   (V.toList pdfE)
                   (V.toList pdfT)
         err' = fold L.sum (fmap abs errV)
     err'
