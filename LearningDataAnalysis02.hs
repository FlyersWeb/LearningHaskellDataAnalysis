module LearningDataAnalysis02 where
    import Data.List
    import Data.Either
    import Text.CSV
    import Database.HDBC
    import Database.HDBC.Sqlite3

    convertCSVToSql :: String -> FilePath -> [String] -> CSV -> IO()
    convertCSVToSql tableName outFileName fields records =
        if nfieldsInFile == nfieldsInFields then do
            conn <- connectSqlite3 outFileName
            run conn createStatement []
            stmt <- prepare conn insertStatement
            executeMany stmt (tail
                (filter (\record -> nfieldsInFile == length record) sqlRecords))
            commit conn
            disconnect conn
            putStrLn "Successfull"
        else
            putStrLn "The number of input fields differ"
        where
            nfieldsInFile = length $ head records
            nfieldsInFields = length fields
            createStatement = "CREATE TABLE " ++ tableName ++ " (" ++ (intercalate ", " fields) ++ ")"
            insertStatement = "INSERT INTO " ++ tableName ++ " VALUES(" ++ (intercalate ", " (replicate nfieldsInFile "?")) ++ ")"
            sqlRecords = map (\record -> map (\element -> toSql element) record) records

    convertCSVFileToSql :: String -> String -> String -> [String] -> IO()
    convertCSVFileToSql inFileName outFileName tableName fields = do
        input <- readFile inFileName
        let records = parseCSV inFileName input
        either handleCSVError convertTool records
        where
            convertTool = convertCSVToSql tableName outFileName fields
            handleCSVError csv = putStrLn "Not a valid CSV file"