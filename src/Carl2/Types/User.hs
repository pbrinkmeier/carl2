{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Carl2.Types.User where

import Carl2.Fields.UserState
import Carl2.Schema

import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sqlite

-- User state machine stuff

getState :: User -> UserState
getState = userState

setState :: UserId -> UserState -> ReaderT SqlBackend IO ()
setState userId state = update userId [ UserState =. state ]

-- DB stuff

create chatId =
  insertBy $ User chatId Initial

getByTelegramId chatId =
  getBy $ UniqueUserTelegramId chatId
