{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Carl2.Bot where

import Data.Text as T
import Database.Persist.Sqlite
import qualified Telegram.Bot.API as TG
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug

runBot token conn = do
  env <- TG.defaultTelegramClientEnv token
  bot <- initBot conn
  startBot_ (traceBotDefault bot) env

initBot conn = do
  pure BotApp
    { botInitialModel = Model 0
    , botAction       = handleUpdate
    , botHandler      = handleAction
    , botJobs         = []
    }

data Model = Model Int

instance Show Model where
  show _ = "<model>"

data Action
  = NoAction
  | GotText Text
  deriving (Show)

pattern UpdateText text =
  TG.Update {
    TG.updateMessage =
      Just (TG.Message { TG.messageText = Just text })
  }

-- Turn incoming updates into Actions.

handleUpdate (UpdateText text) _ =
  Just $ GotText text

handleUpdate _                 _ =
  Just NoAction

-- React to incoming Actions.

handleAction NoAction model = pure model
handleAction action model@(Model conn) =
  model <# handleAction' action conn

handleAction' action conn =
  case action of
    GotText text -> do
      replyText $ "You said: " <> text
      pure NoAction
