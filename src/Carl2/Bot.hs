{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Carl2.Bot where

import Control.Monad.IO.Class
import Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import qualified Telegram.Bot.API as TG
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug

import qualified Carl2.Schema as Schema
import qualified Carl2.Fields.UserState as US
import qualified Carl2.Types.Ingredient as Ingredient
import qualified Carl2.Types.Meal as Meal
import qualified Carl2.Types.User as User

runBot token conn = do
  env <- TG.defaultTelegramClientEnv token
  bot <- initBot conn
  startBot_ (traceBotDefault bot) env

initBot conn = do
  runSqlConn (runMigration Schema.migrateAll) conn
  pure BotApp
    { botInitialModel = Model conn
    , botAction       = handleUpdate
    , botHandler      = handleAction
    , botJobs         = []
    }

data Model = Model SqlBackend

instance Show Model where
  show _ = "<model>"

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

handleUpdate (UpdateText text) _ =
  case T.words text of
    ["/start"]      -> Just Start
    ["/meal"]       -> Just Meal
    ["/done"]       -> Just Done
    _               -> Just $ GotText text

handleUpdate _                 _ =
  Just NoAction

-- React to incoming Actions.

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model              = pure model
handleAction Start    model@(Model conn) = model <# do
  chatId <- currentChatId
  maybe (pure NoAction) createUser chatId
  where
    createUser chatId = do
      res <- liftIO $ runSqlConn (User.create chatId) conn
      case res of
        Left _ -> do
          replyText "Already started!"
          pure NoAction
        Right _ -> do
          replyText "Welcome!"
          pure NoAction
handleAction action model@(Model conn) =
  model <# (withUser conn $ handleAction' conn action)

pattern a :<< b = (a, b)

handleAction' :: SqlBackend -> Action -> (Schema.UserId, Schema.User) -> BotM Action
handleAction' conn action (userId, user) =
  case (User.getState user, action) of
    US.Initial :<< Meal -> do
      now <- liftIO $ getCurrentTime
      mealId <- liftIO $ runSqlConn (Meal.create userId now) conn
      liftIO $ runSqlConn (User.setState userId $ US.MealEntry $ fromSqlKey mealId) conn
      replyText "Enter ingredients by sending me \"<amount> <unit> <ingredient> <calories>\"."
      pure NoAction

    US.MealEntry mealId :<< Done -> do
      liftIO $ runSqlConn (User.setState userId US.Initial) conn
      replyText "Meal done."
      pure NoAction
    
    US.MealEntry mealId :<< GotText text ->
      case Ingredient.parse text of
        Nothing -> do
          replyText "I didn't get that :("
          pure NoAction
        Just (amount, unit, ingredient, cals) -> do
          let createMealQuery = Ingredient.create (toSqlKey mealId) amount unit ingredient cals
          liftIO $ runSqlConn createMealQuery conn
          total <- liftIO $ runSqlConn (Meal.getTotal $ toSqlKey mealId) conn
          replyText $ "Thamgs. Current total: " <> T.pack (show total)
          pure NoAction

    _ -> do
      replyText "Invalid action."
      pure NoAction

withUser conn f = do
  res <- currentChatId
  case res of
    Nothing -> do
      pure NoAction
    Just chatId -> do
      users  <- liftIO $ runSqlConn (User.getByTelegramId chatId) conn
      case users of
        Just (Entity userId user) -> do
          f (userId, user)
        Nothing -> do
          replyText "You need to /start first."
          pure NoAction
