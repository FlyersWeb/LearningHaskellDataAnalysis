{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LearningDataAnalysis06 where
  import Data.List as L
  -- cabal install unordered-containers
  import Data.Hashable
  import Data.HashMap.Strict as HM
  import Database.HDBC.Sqlite3
  import Database.HDBC
  import Control.Concurrent
  import Data.Char
  -- cabal install http-conduit
  import Network.HTTP.Conduit
  -- cabal install authenticate-oauth
  import Web.Authenticate.OAuth
  import Data.Aeson
  import GHC.Generics

  myoauth :: OAuth
  myoauth = newOAuth {
    oauthServerName         = "api.twitter.com"
    , oauthConsumerKey      = "YOUR CONSUMER KEY"
    , oauthConsumerSecret   = "YOUR CONSUMER SECRET"
  }

  mycred :: Credential
  mycred = newCredential  "YOUR ACCESS TOKEN"
                          "YOUR ACCESS TOKEN SECRET"

  data User =
    User { screenName :: !String } deriving (Show, Generic)

  data Tweet = 
    Tweet  { text :: !String
            , lang :: !String
            , user :: !User } deriving (Show, Generic)

  data Search =
    Search { statuses :: ![Tweet] } deriving (Show, Generic)

  instance FromJSON User
  -- instance ToJSON User
  instance FromJSON Tweet
  -- instance ToJSON Tweet
  instance FromJSON Search
  -- instance ToJSON Search

  loadConfig :: IO (OAuth, Credential)
  loadConfig = do
    myoauth <- read <$> readFile "twitter_oauth.hs"
    mycred  <- read <$> readFile "twitter_cred.hs"
    return (myoauth, mycred)

  twitterSearch :: String -> IO (Either String Search)
  twitterSearch term = do
    -- (myoauth, mycred) <- loadConfig
    req <- parseUrl $
      "https://api.twitter.com/1.1/search/tweets.json?count=100&q=" ++ term
    -- Using a HTTP manager, we authenticate the request and send it to get a response.
    manager <- newManager tlsManagerSettings
    -- OAuth Authentication.
    signedreq <- signOAuth myoauth mycred req
    -- Send request and Decode the response body.
    res <- httpLbs signedreq manager
    return $ eitherDecode $ responseBody res

  createTweetsDatabase :: IO()
  createTweetsDatabase = do
    conn <- connectSqlite3 "tweets.sql"
    run conn createStatement []
    commit conn
    disconnect conn
    putStrLn "Successfully created database"
    where
      createStatement = "CREATE TABLE tweets (message TEXT, user TEXT, language TEXT)"

  insertTweetsInDatabase :: [Tweet] -> IO()
  insertTweetsInDatabase tweets = do
    conn <- connectSqlite3 "tweets.sql"
    stmt <- prepare conn insertStatement
    executeMany stmt sqlRecords
    commit conn
    disconnect conn
    putStrLn "Successfully inserted in database"
    where
      insertStatement = "INSERT INTO tweets VALUES (?, ?, ?)"
      sqlRecords = L.map (\(Tweet message language (User user)) -> [toSql message, toSql user, toSql language]) tweets

  collectTweetsIntoDatabase :: IO()
  collectTweetsIntoDatabase = do
    status <- twitterSearch "eco"
    either
      putStrLn (\(Search statuses) -> insertTweetsInDatabase statuses)
      status
    threadDelay 5000

  frequency :: (Eq k, Data.Hashable.Hashable k, Integral v) => [k] -> HashMap k v
  frequency [] = HM.empty
  frequency (x:xs) = HM.insertWith (+) x 1 (frequency xs)
