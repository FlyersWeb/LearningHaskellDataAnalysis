module LearningDataAnalysis01 where
    import Data.List
    import Data.Either
    import Text.CSV

    readColumn :: [String] -> [Double]
    readColumn = map read

    getColumnInCSV :: CSV -> String -> Either String Integer
    getColumnInCSV csv columnName =
        case lookupResponse of
            Nothing -> Left
                "The column does not exists"
            Just x -> Right (fromIntegral x)
        where
            lookupResponse = findIndex (== columnName) (head csv)

    numberColumnInCSV :: CSV -> Int
    numberColumnInCSV csv =
        length $ head csv

    applyToColumnInCSV :: ([String] -> b) -> CSV -> String -> Either String b
    applyToColumnInCSV func csv column = fmap (func . elements)
            columnIndex
        where
            columnIndex = getColumnInCSV csv column
            nFieldsInCsv = numberColumnInCSV csv
            records = tail $ filter (\record -> nFieldsInCsv == length record) csv
            elements ci = map (\record -> genericIndex record ci) records

    applyToColumnInCSVFile :: ([String] -> b) -> FilePath -> String -> IO (Either String b)
    applyToColumnInCSVFile func inFileName column = do
        input <- readFile inFileName
        let records = parseCSV inFileName input
        return $ either
            handleCSVError
            (\csv -> applyToColumnInCSV func csv column)
            records
        where
            handleCSVError csv = Left "This does not appear to be a valid CSV"

    countFieldsInEachRecord :: CSV -> [Integer]
    countFieldsInEachRecord csv = map genericLength (init csv)

    lineNumbersWithIncorrectCount :: CSV -> [(Integer, Integer)]
    lineNumbersWithIncorrectCount(fields:csv) = filter
      (\(_, thisCount) -> thisCount /= nfields)
      lineNoCountPairs
      where
        nfields = genericLength fields
        count = countFieldsInEachRecord csv
        lineNoCountPairs = zip [1..] count

