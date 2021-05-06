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

import Database.Persist.TH
import qualified Telegram.Bot.API as TG

import Carl2.ChatIdField

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    userTelegramId TG.ChatId
    deriving (Show)
|]
