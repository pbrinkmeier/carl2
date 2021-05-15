{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Carl2.Schema where

import Data.Text
import Data.Time
import Database.Persist.TH
import qualified Telegram.Bot.API as TG

import Carl2.Fields.ChatId
import Carl2.Fields.UserState

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    telegramId  TG.ChatId
    state       UserState

    UniqueUserTelegramId telegramId
    deriving (Show)

  Meal
    user        UserId
    created     UTCTime
    deriving (Show)

  Ingredient
    meal        MealId
    amount      Double
    unit        Text
    text        Text
    calories    Double
    deriving (Show)
|]
