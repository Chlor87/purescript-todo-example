module Main where

import Prelude

import App (mkApp)
import App.Router (mkRouter)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Todo.Ctx (mkTodoProvider)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = unsafePartial $ do
  Just root <- querySelector (QuerySelector "#root") <<< toParentNode =<< document =<< window
  router <- mkRouter
  todos <- mkTodoProvider
  app <- mkApp
  renderRoot <@> router [ todos [ app unit ] ] =<< createRoot root
