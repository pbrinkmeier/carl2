{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Carl2.BotImpl where

import Data.Text as T
import Database.Persist.Sqlite
import qualified Telegram.Bot.API as TG

import Carl2.Bot
import qualified Carl2.Schema as Schema
import Carl2.Fields.UserState
import qualified Carl2.Types.Ingredient as Ingredient
import qualified Carl2.Types.Meal as Meal
import qualified Carl2.Types.User as User

cfg :: BotCfg Action
cfg = BotCfg
  { botCfgMigration    = Schema.migrateAll
  , botCfgHandleUpdate = handleUpdate
  , botCfgHandleAction = handleAction
  }

data Action
  = NoAction
  | Start
  | Meal
  | Done
  | GotText Text
  deriving (Show)

pattern UpdateText text =
  TG.Update {
    TG.updateMessage =
      Just (TG.Message { TG.messageText = Just text })
  }

-- Turn incoming updates into Actions.

handleUpdate (UpdateText text) =
  case T.words text of
    ["/start"] -> Just Start
    ["/meal"]  -> Just Meal
    ["/done"]  -> Just Done
    _          -> Just $ GotText text

handleUpdate _                 =
  Nothing

-- React to incoming Actions.

handleAction Start = do
  res <- currentChatId
  maybe (pure ()) createUser res
  where
    createUser chatId = do
      res <- runSql $ User.create chatId
      case res of
        Left _ -> do
          replyText "Already started!"
        Right _ -> do
          replyText "Welcome!"
handleAction action = withUser $ handleAction' action

withUser f = do
  res <- currentChatId
  case res of
    Nothing -> do
      replyText "Missing chat id."
    Just chatId -> do
      res  <- runSql $ User.getByTelegramId chatId
      case res of
        Nothing -> do
          replyText "You need to /start first."
        Just (Entity userId user) -> do
          f userId user

pattern a :<< b = (a, b)

handleAction' action userId user = do
  case (User.getState user, action) of
    Initial :<< Meal -> do
      now <- getCurrentTime
      mealId <- runSql $ Meal.create userId now
      runSql $ User.setState userId $ MealEntry $ fromSqlKey mealId
      replyText "Enter ingredients by sending me \"<amount> <unit> <ingredient> <calories>\"."

    MealEntry _mealId :<< Done -> do
      runSql $ User.setState userId Initial
      replyText "Meal done."

    MealEntry mealId :<< GotText text -> do
      case Ingredient.parse text of
        Nothing -> do
          replyText "I didn't get that :("
        Just (amount, unit, ingredient, cals) -> do
          runSql $ Ingredient.create (toSqlKey mealId) amount unit ingredient cals
          total <- runSql $ Meal.getTotal $ toSqlKey mealId
          replyText $ "Thamgs. Current total: " <> T.pack (show total)

    _ -> do
      replyText "Invalid action."
