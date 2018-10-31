module LearningDataAnalysis03 where
    import Data.List
    import Data.Either
    import Text.CSV
    import Text.Regex.Posix ((=~))
    import LearningDataAnalysis01

    myGrep :: String -> String -> IO()
    myGrep myRegex filename = do
      fileSlurp <- readFile filename
      mapM_ putStrLn $
        filter (=~ myRegex) (lines fileSlurp)

    identifyMatchingFields :: (String -> Bool) -> [String] -> [String] -> Integer -> [(String, String, String)]
    identifyMatchingFields myStringCmpFunc record headings idColumnIndex = filter
        (\(_,_,field) -> myStringCmpFunc field)
      keyvalue
      where
        nfields = length headings
        keyvalue = zip3 (replicate nfields (genericIndex record idColumnIndex)) headings record

    identifyInCSV :: (String -> Bool) -> CSV -> String -> Either String [(String, String, String)]
    identifyInCSV myFieldFunc csv idColumn = either
        Left
        (\ci -> Right $ concatMap
          (\record ->
            identifyMatchingFields
            myFieldFunc record (head csv) ci
          )
          (tail csv)
        )
        columnIndex
        where
          headings = head csv
          columnIndex = getColumnInCSV csv idColumn

    identifyInCSVFile :: (String -> Bool) -> String -> String -> IO(Either String [(String, String, String)])
    identifyInCSVFile myStringCmpFunc inFileName idColumn = do
      records <- parseCSVFromFile inFileName
      return $ either
        (\err -> Left "This does not appear to be a CSV File")
        (\csv -> identifyInCSV myStringCmpFunc (init csv) idColumn) records

    identifyInCSVFileFromColumn :: (String -> Bool) -> String -> String -> String -> IO(Either String [(String, String, String)])
    identifyInCSVFileFromColumn myRegexFunc inFileName idColumn desiredHeading = do
      allFields <- identifyInCSVFile myRegexFunc inFileName idColumn
      return $ either
        Left
        (\af -> Right $ filter (\(_, heading, _) -> heading == desiredHeading) af) allFields
