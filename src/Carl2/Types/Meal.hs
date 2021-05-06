{-# LANGUAGE GADTs #-}

module Carl2.Types.Meal where

import Carl2.Schema

import Database.Persist

-- DB stuff

create userId timestamp =
  insert $ Meal userId timestamp
