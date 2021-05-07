{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Carl2.Types.Ingredient where

import Carl2.Schema

import Data.Text as T
import Database.Persist
import Text.Regex.TDFA

-- Message parsing stuff
pattern :: Text
pattern = "([0-9]+(\\.[0-9]+)?)[ ]*([A-Za-z.]+) (.+) ([0-9]+(\\.[0-9]+)?)" 

parse :: Text -> Maybe (Double, Text, Text, Double)
parse text = do
  (_, _, _, [strAmount, _, unit, ingredient, strCals, _]) <-
    (text =~~ pattern) :: Maybe (Text, Text, Text, [Text])
  pure (read $ T.unpack strAmount, unit, ingredient, read $ T.unpack strCals)

-- DB stuff

create mealId amount unit text calories =
  insert $ Ingredient mealId amount unit text calories
