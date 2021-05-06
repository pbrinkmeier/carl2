{-# LANGUAGE GADTs #-}

module Carl2.Types.User where

import qualified Carl2.Schema as Schema

import Database.Persist

create chatId =
  insertBy $ Schema.User chatId
