module Main (main) where
    import LearningDataAnalysis01
    import LearningDataAnalysis02
    import Database.HDBC
    import Database.HDBC.Sqlite3

    -- main = convertCSVFileToSql "all_week.csv" "earthquakes.sql" "oneWeek" [
    --     "time TEXT", "latitude REAL", "longitude REAL", 
    --     "depth REAL", "mag REAL", "magType REAL", "nst INTEGER", 
    --     "gap REAL", "dmin REAL", "rms REAL", "net REAL", "id TEXT", 
    --     "updated TEXT", "place TEXT", "type TEXT",
    --     "horizontalError REAL", "depthError REAL", "magError REAL", 
    --     "magNst INTEGER", "status TEXT", "locationSource TEXT", "magSource TEXT"]

    main = do
        conn <- connectSqlite3 "earthquakes.sql"
        magnitudes <- quickQuery' conn "SELECT mag FROM oneWeek" []
        let magnitudesDouble = map (\record -> fromSql $ head record :: Double) magnitudes
        let avg = average magnitudesDouble
        print avg
