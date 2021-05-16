module Main where

import Data.Text as T
import System.Environment
import Telegram.Bot.Simple

import Carl2.Bot as Bot
import Carl2.BotImpl as BotImpl

main :: IO ()
main = do
  token      <- getEnvToken "CARL2_BOT_TOKEN"
  connString <- getEnv      "CARL2_CONN_STRING"

  Bot.runBot token (T.pack connString) BotImpl.cfg
