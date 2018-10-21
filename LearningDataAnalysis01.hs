module LearningDataAnalysis01 where
    import Data.List
    import Data.Either
    import Text.CSV

    average :: (Real a, Fractional b) => [a] -> b
    average xs = realToFrac(sum xs) / fromIntegral(length xs)

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
            nFieldsInCsv = length $ head csv
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
