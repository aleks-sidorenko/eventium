{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Cafe.Models.Tab
  ( TabState (..),
    Drink (..),
    Food (..),
    MenuItem (..),
    allDrinks,
    allFood,
    TabProjection,
    tabProjection,
    TabCommandHandler,
    tabCommandHandler,
    TabCommand (..),
    TabEvent (..),
    TabCommandError (..),
    setIndexesToNothing,
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (catMaybes, isJust)
import Eventium

data MenuItem
  = MenuItem
  { description :: String,
    price :: Double
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''MenuItem

newtype Drink = Drink {unDrink :: MenuItem}
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Food = Food {unFood :: MenuItem}
  deriving (Show, Eq, FromJSON, ToJSON)

data TabState
  = TabState
  { isOpen :: Bool,
    -- | All drinks that need to be served. 'Nothing' indicates the drink was
    -- served or cancelled.
    outstandingDrinks :: [Maybe Drink],
    -- | All food that needs to be made. 'Nothing' indicates the food was made
    -- or cancelled.
    outstandingFood :: [Maybe Food],
    -- | All food that has been made. 'Nothing' indicates the food was served.
    preparedFood :: [Maybe Food],
    -- | All items that have been served.
    servedItems :: [MenuItem]
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''TabState

tabSeed :: TabState
tabSeed = TabState True [] [] [] []

data TabEvent
  = DrinksOrdered
      { drinks :: [Drink]
      }
  | FoodOrdered
      { food :: [Food]
      }
  | DrinksCancelled
      { drinkIndexes :: [Int]
      }
  | FoodCancelled
      { foodIndexes :: [Int]
      }
  | DrinksServed
      { drinkIndexes :: [Int]
      }
  | FoodPrepared
      { foodIndexes :: [Int]
      }
  | FoodServed
      { foodIndexes :: [Int]
      }
  | TabClosed Double
  deriving (Show, Eq)

deriveJSON defaultOptions ''TabEvent

applyTabEvent :: TabState -> TabEvent -> TabState
applyTabEvent state (DrinksOrdered newDrinks) = state {outstandingDrinks = state.outstandingDrinks ++ map Just newDrinks}
applyTabEvent state (FoodOrdered newFood) = state {outstandingFood = state.outstandingFood ++ map Just newFood}
applyTabEvent state (DrinksCancelled indexes) = state {outstandingDrinks = setIndexesToNothing indexes state.outstandingDrinks}
applyTabEvent state (FoodCancelled indexes) = state {outstandingFood = setIndexesToNothing indexes state.outstandingFood}
applyTabEvent state (DrinksServed indexes) =
  state
    { servedItems = state.servedItems ++ fmap (\(Drink mi) -> mi) (catMaybes $ getListItemsByIndexes indexes state.outstandingDrinks),
      outstandingDrinks = setIndexesToNothing indexes state.outstandingDrinks
    }
applyTabEvent state (FoodPrepared indexes) =
  state
    { preparedFood = state.preparedFood ++ getListItemsByIndexes indexes state.outstandingFood,
      outstandingFood = setIndexesToNothing indexes state.outstandingFood
    }
applyTabEvent state (FoodServed indexes) =
  state
    { servedItems = state.servedItems ++ fmap (\(Food mi) -> mi) (catMaybes $ getListItemsByIndexes indexes state.preparedFood),
      preparedFood = setIndexesToNothing indexes state.preparedFood
    }
applyTabEvent state (TabClosed _) = state {isOpen = False}

setIndexesToNothing :: [Int] -> [Maybe a] -> [Maybe a]
setIndexesToNothing indexes = zipWith (\i x -> if i `elem` indexes then Nothing else x) [0 ..]

getListItemsByIndexes :: [Int] -> [a] -> [a]
getListItemsByIndexes indexes = map snd . filter ((`elem` indexes) . fst) . zip [0 ..]

type TabProjection = Projection TabState TabEvent

tabProjection :: TabProjection
tabProjection = Projection tabSeed applyTabEvent

data TabCommand
  = PlaceOrder [Food] [Drink]
  | -- CancelDrinks [Int]
    -- CancelFood [Int]
    MarkDrinksServed [Int]
  | MarkFoodPrepared [Int]
  | MarkFoodServed [Int]
  | CloseTab Double
  deriving (Show, Eq)

data TabCommandError
  = TabAlreadyClosed
  | CannotCancelServedItem
  | TabHasUnservedItems
  | MustPayEnough
  deriving (Show, Eq)

applyTabCommand :: TabState -> TabCommand -> Either TabCommandError [TabEvent]
applyTabCommand TabState {isOpen = False} _ = Left TabAlreadyClosed
applyTabCommand state (CloseTab cash)
  | amountOfNonServedItems > 0 = Left TabHasUnservedItems
  | cash < totalServedWorth = Left MustPayEnough
  | otherwise = Right [TabClosed cash]
  where
    amountOfNonServedItems =
      length (filter isJust state.outstandingDrinks)
        + length (filter isJust state.outstandingFood)
        + length (filter isJust state.preparedFood)
    totalServedWorth = foldl' (+) 0 (fmap (.price) (state.servedItems))
applyTabCommand _ (PlaceOrder newFood newDrinks) = Right [FoodOrdered newFood, DrinksOrdered newDrinks]
-- TODO: Check if index exceeds list length or if item is already marked null
-- for the next 3 commands.
applyTabCommand _ (MarkDrinksServed indexes) = Right [DrinksServed indexes]
applyTabCommand _ (MarkFoodPrepared indexes) = Right [FoodPrepared indexes]
applyTabCommand _ (MarkFoodServed indexes) = Right [FoodServed indexes]

type TabCommandHandler = CommandHandler TabState TabEvent TabCommand TabCommandError

tabCommandHandler :: TabCommandHandler
tabCommandHandler = CommandHandler applyTabCommand tabProjection

-- | List of all drinks. The menu could be its own CommandHandler in the future.
allDrinks :: [Drink]
allDrinks =
  map
    Drink
    [ MenuItem "Beer" 3.50,
      MenuItem "Water" 0.00,
      MenuItem "Soda" 1.00
    ]

-- | List of all food. The menu could be its own CommandHandler in the future.
allFood :: [Food]
allFood =
  map
    Food
    [ MenuItem "Sandwich" 6.50,
      MenuItem "Steak" 10.00,
      MenuItem "Chips" 1.00
    ]
