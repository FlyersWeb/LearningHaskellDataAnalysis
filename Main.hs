module Main (main) where
    import LearningDataAnalysis00
    import LearningDataAnalysis01
    import LearningDataAnalysis02
    import LearningDataAnalysis03
    import LearningDataAnalysis04
    import LearningDataAnalysis05
    import LearningDataAnalysis06

    import Data.List
    -- install: cabal install HDBC sqlite-simple HDBC-sqlite3
    import Database.HDBC
    import Database.HDBC.Sqlite3
    -- install: cabal install csv
    import Text.CSV
    import Text.Regex.Posix ((=~))

    import System.Environment (getArgs)

    -- -- application of function to a column
    -- main = do
    --     max <- applyToColumnInCSVFile (maximum . readColumn) "all_week.csv" "mag"
    --     min <- applyToColumnInCSVFile (minimum . readColumn) "all_week.csv" "mag"
    --     avg <- applyToColumnInCSVFile (average . readColumn) "all_week.csv" "mag"
    --     print max
    --     print min
    --     print avg

    -- -- application of CSV to SQLite conversion
    -- main = convertCSVFileToSql "all_week.csv" "earthquakes.sql" "oneWeek" [
    --     "time TEXT", "latitude REAL", "longitude REAL", 
    --     "depth REAL", "mag REAL", "magType REAL", "nst INTEGER", 
    --     "gap REAL", "dmin REAL", "rms REAL", "net REAL", "id TEXT", 
    --     "updated TEXT", "place TEXT", "type TEXT",
    --     "horizontalError REAL", "depthError REAL", "magError REAL", 
    --     "magNst INTEGER", "status TEXT", "locationSource TEXT", "magSource TEXT"]

    -- -- SQLite querying application
    -- main = do
    --     conn <- connectSqlite3 "earthquakes.sql"
    --     magnitudes <- quickQuery' conn "SELECT mag FROM oneWeek" []
    --     let magnitudesDouble = map (\record -> fromSql $ head record :: Double) magnitudes
    --     let avg = average magnitudesDouble
    --     print avg

    -- -- Incorrect data application
    -- main = do
    --   csv <- parseCSVFromFile "poorFieldCounts.csv"
    --   let count = either Left (\csv -> Right $ lineNumbersWithIncorrectCount csv) csv
    --   print count

    -- -- Application of custom grep ./main Betty poorFieldCounts.csv
    -- main :: IO()
    -- main = do
    --   (myRegex:filenames) <- getArgs
    --   mapM_ (\filename -> myGrep myRegex filename) filenames

    -- -- Application of simple regex application to CSV
    -- main :: IO()
    -- main = do
    --   -- Search for empty fields
    --   tmp <- identifyInCSVFile (\x -> x =~ "^\\s*$") "poorData.csv" "Number"
    --   print tmp

    -- -- Application of invalid date search
    -- main :: IO()
    -- main = do
    --   tmp <- identifyInCSVFileFromColumn
    --     (\x -> not (x =~ "^[1-9][0-9]?/[1-9][0-9]?/[12][0-9][0-9][0-9]$"))
    --     "poorData.csv"
    --     "Number"
    --     "Birthday"
    --   print tmp

    -- -- Application of plot closing prices
    -- main :: IO()
    -- main = do
    --   aapl <- pullStockClosingPrices "AAPL.sql" "aapl"
    --   plotData2D aapl "AAPL" "aapl.png"
    --   print aapl

    -- -- Application of plot coin flip null hypothesis
    -- main :: IO()
    -- main = do
    --   plotFunction2D
    --     (\k -> probabilityMassFunction (floor k) 1000 0.5)
    --     0
    --     1000
    --     "Coin flip probabilities"
    --     "coinflips.png"
    --   print "Done"

    -- Application of standard deviation for difference between runs home and away
    main :: IO()
    main = do
      -- convertCSVFileToSql "winloss2014.csv" "winloss.sql" "winloss" ["date TEXT", "awayteam TEXT", "hometeam TEXT", "awayscore INTEGER", "homescore INTEGER"]
      runsAtHome <- queryDatabase "winloss.sql" "SELECT hometeam, SUM(homescore) FROM winloss GROUP BY hometeam ORDER BY hometeam"
      runsAway <- queryDatabase "winloss.sql" "SELECT awayteam, sum(awayscore) FROM winloss GROUP BY awayteam ORDER BY awayteam"
      let runsHomeAway = zip (readDoubleColumn runsAtHome 1) (readDoubleColumn runsAway 1)
      let runsHomeAwayDiff = map (\(a,b) -> a-b) runsHomeAway
      let std = standardDeviation runsHomeAwayDiff / (sqrt $ genericLength runsHomeAwayDiff)
      print std
