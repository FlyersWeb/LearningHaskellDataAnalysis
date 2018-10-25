module LearningDataAnalysis03 where
    import Data.List
    import Data.Either
    import Text.CSV
    import Text.Regex.Posix ((=~))
    

    myGrep :: String -> String -> IO()
    myGrep myRegex filename = do
      fileSlurp <- readFile filename
      mapM_ putStrLn $
        filter (=~ myRegex) (lines fileSlurp)
