{-# LANGUAGE OverloadedStrings #-}

module Shorty.Database where

import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Control.Applicative
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.FromRow

data UrlEntry = UrlEntry
  { code          :: Int
  , url           :: T.Text
  , openCount     :: Int
  }

instance FromRow UrlEntry where
    fromRow = UrlEntry <$> field <*> field <*> field

instance Show UrlEntry where
    show (UrlEntry code url openCount) =
      "UrlEntry { code: " ++ show code ++ ", url: " ++ T.unpack url ++ ", openCount: " ++ show openCount ++ " }n"

connectDB :: String -> IO Connection
connectDB name = connect defaultConnectInfo { connectDatabase = name }

hDB :: IO Connection
hDB = connectDB "haskell_shorty"

initEnv :: IO Connection
initEnv = do
    conn <- hDB
    createTables conn
    return conn

insertLink :: String -> IO Int
insertLink url = do
    conn <- hDB
    execute conn "INSERT INTO stats (url) VALUES (?)" $ Only url
    res <- query conn "select * from stats where url=(?) order by code desc limit 1" $ Only url
    return $ code ((res :: [UrlEntry]) !! 0)

createTables :: Connection -> IO ()
createTables c = do
    execute_ c "DROP TABLE if exists stats"
    execute_ c "DROP SEQUENCE if exists stats_codes_seq"
    execute_ c "CREATE SEQUENCE stats_codes_seq START WITH 3844 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1"
    execute_ c "CREATE UNLOGGED TABLE IF NOT EXISTS stats \
      \(code integer NOT NULL DEFAULT nextval('stats_codes_seq') PRIMARY KEY,\
      \url varchar NOT NULL,open_count integer NOT NULL DEFAULT 0)"
    execute_ c "CREATE UNIQUE INDEX index_stats_on_code ON stats USING btree (code)"
    return ()
