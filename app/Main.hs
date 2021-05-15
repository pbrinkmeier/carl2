module Main where

import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Text as T
import Database.Persist.Sqlite
import System.Environment
import Telegram.Bot.Simple

import Carl2.BotImpl

main :: IO ()
main = do
  token      <- getEnvToken "CARL2_BOT_TOKEN"
  connString <- getEnv      "CARL2_CONN_STRING"

  runStderrLoggingT $ withSqliteConn (T.pack connString) (liftIO . runBot token)
