module App.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', default, int, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Todo
  = List
  | Add
  | Edit Int

derive instance Generic Todo _
instance Show Todo where
  show = genericShow

data Route
  = Home
  | Todo Todo

derive instance Generic Route _
instance Show Todo => Show Route where
  show = genericShow

todos :: RouteDuplex' Todo
todos = sum
  { "List": "list" / noArgs
  , "Add": "add" / noArgs
  , "Edit": "edit" / int segment
  }

routes :: RouteDuplex' Route
routes = default (Todo List) $ root $ sum
  { "Home": noArgs
  , "Todo": "todo" / todos
  }
