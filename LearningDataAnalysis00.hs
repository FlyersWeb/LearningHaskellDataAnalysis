module LearningDataAnalysis00 where
    average :: (Real a, Fractional b) => [a] -> b
    average xs = realToFrac(sum xs) / fromIntegral(length xs)
