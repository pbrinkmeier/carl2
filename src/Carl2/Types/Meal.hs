{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Carl2.Types.Meal where

import Carl2.Schema

import Database.Esqueleto
import qualified Database.Persist as P

-- DB stuff

create userId timestamp =
  P.insert $ Meal userId timestamp

getTotal :: MealId -> SqlReadT IO Double
getTotal mealId = do
  [Value (Just total)] <- select $ from $ \ing -> do
    where_ (ing ^. IngredientMeal ==. val mealId)
    pure $ sum_ $ ing ^. IngredientCalories

  pure total
