module Data.LocalStorage where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import getItemImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> Effect (Maybe String)

getItem :: String -> Effect (Maybe String)
getItem = getItemImpl Just Nothing

foreign import setItem :: String -> String -> Effect Unit
