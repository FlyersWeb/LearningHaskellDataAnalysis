module LearningDataAnalysis05 where
  -- cabal install exact-combinatorics
  import Math.Combinatorics.Exact.Binomial
  import Data.List
  import LearningDataAnalysis00

  probabilityMassFunction :: Integral a => a -> a -> Double -> Double
  probabilityMassFunction k n p =
    (fromIntegral (n `choose` k))
      * (p^k) * ((1-p)^(n-k))
  
  standardDeviation :: [Double] -> Double
  standardDeviation values =
    (sqrt . sum $ map (\x -> (x-mu)*(x-mu)) values) / sqrt_nm1
    where
      mu = average values
      sqrt_nm1 = sqrt $ (genericLength values - 1)

  covariance :: [Double] -> [Double] -> Double
  covariance x y = average $ zipWith (\xi yi -> (xi-xavg) * (yi-yavg)) x y
      where
        xavg = average x
        yavg = average y

  pearsonR :: [Double] -> [Double] -> Double
  pearsonR x y = r
      where
        xstd = standardDeviation x
        ystd = standardDeviation y
        r = covariance x y / (xstd * ystd)

  pearsonRsqrd :: [Double] -> [Double] -> Double
  pearsonRsqrd x y = pearsonR x y ^ 2

  linearRegression :: [Double] -> [Double] -> (Double, Double)
  linearRegression x y = (gradient, intercept)
    where
      xavg = average x
      yavg = average y
      xstd = standardDeviation x
      gradient = covariance x y / (xstd * xstd)
      intercept = yavg - gradient * xavg
