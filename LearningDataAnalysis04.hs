module LearningDataAnalysis04 where
  import Data.List
  import Database.HDBC.Sqlite3
  import Database.HDBC
  import Graphics.EasyPlot
  import LearningDataAnalysis02

  readIntegerColumn :: [[SqlValue]] -> Integer -> [Integer]
  readIntegerColumn sqlResult index = map
    (\row -> fromSql $ genericIndex row index :: Integer) sqlResult

  readDoubleColumn :: [[SqlValue]] -> Integer -> [Double]
  readDoubleColumn sqlResult index = map
    (\row -> fromSql $ genericIndex row index :: Double) sqlResult

  readStringColumn :: [[SqlValue]] -> Integer -> [String]
  readStringColumn sqlResult index = map
    (\row -> fromSql $ genericIndex row index :: String) sqlResult

  queryDatabase :: FilePath -> String -> IO([[SqlValue]])
  queryDatabase databaseFile sqlQuery = do
    conn <- connectSqlite3 databaseFile
    let result = quickQuery' conn sqlQuery []
    -- disconnect conn
    result

  pullStockClosingPrices :: String -> String -> IO([(Double, Double)])
  pullStockClosingPrices databaseFile table = do
    sqlResult <- queryDatabase databaseFile ("SELECT rowid, adjclose FROM " ++ table)
    return $ zip
      (reverse $ readDoubleColumn sqlResult 0)
      (readDoubleColumn sqlResult 1)

  customPlot :: [(Double, Double)] -> String -> String -> IO(Bool)
  customPlot dataSerie title fileName = do
    plot (PNG fileName) $ Data2D [Title title] [] $ dataSerie
