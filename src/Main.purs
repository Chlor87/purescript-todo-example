module Main where

import Prelude

import App (mkApp)
import App.Router (mkRouter, useRouter)
import App.Routes (Route(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM (button, div_, h1_, p_, text)
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, fragment, useState, (/\))
import React.Basic.Hooks as R
import Todo.Ctx (mkTodoProvider)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

mkHome :: Component Unit
mkHome = do
  component "Home" \_ -> R.do
    state /\ setState <- useState 0
    { route } <- useRouter
    pure $ fragment
      [ h1_
          [ text case route of
              Home -> "home"
              _ -> "not home"
          ]
      , div_
          [ button { children: [ text "click" ], onClick: handler_ $ setState $ add 1 }
          , p_ [ text $ show state ]
          ]
      ]

main :: Effect Unit
main = unsafePartial $ do
  Just root <- querySelector (QuerySelector "#root") <$> toParentNode =<< document =<< window
  router <- mkRouter
  todos <- mkTodoProvider
  app <- mkApp
  createRoot root >>= renderRoot <@> router [ todos [ app unit ] ]
