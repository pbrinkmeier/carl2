{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Carl2.Types.User where

import Carl2.Schema

import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sqlite

-- User state machine stuff

data UserState
  = Initial
  | MealEntry MealId
  deriving (Show)

getState :: User -> UserState
getState user =
  case user of
    User { userState = "initial" } ->
      Initial
    User { userState = "meal_entry", userStateMEMeal = Just mealId } ->
      MealEntry mealId

setState :: UserId -> UserState -> ReaderT SqlBackend IO ()
setState userId state =
  update userId $ mkUpdateList state
  where
    mkUpdateList Initial =
      [ UserState =. "initial"
      ]
    mkUpdateList (MealEntry mealId) =
      [ UserState       =. "meal_entry"
      , UserStateMEMeal =. Just mealId
      ]

-- DB stuff

create chatId =
  insertBy $ User chatId "initial" Nothing

getByTelegramId chatId =
  getBy $ UniqueUserTelegramId chatId
