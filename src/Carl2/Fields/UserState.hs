{-# LANGUAGE TemplateHaskell #-}

module Carl2.Fields.UserState where

import Data.Int (Int64)
import Database.Persist.TH

data UserState
  = Initial
  | MealEntry Int64
  deriving (Read, Show)

derivePersistField "UserState"
