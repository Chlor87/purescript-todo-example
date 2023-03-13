module App where

import Prelude hiding (div)

import App.Router (useRouter)
import App.Routes (Route(..), Todo(..))
import Data.Array (mapWithIndex, (!!))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import React.Basic.DOM (a, css, div, li, nav, text, ul)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component, fragment, keyed, useContext, (/\))
import React.Basic.Hooks as R
import Todo.AddEdit (mkTodoAddEdit)
import Todo.Ctx (emptyTodo, todoCtx)
import Todo.List (mkTodoList)

mkView :: Component Unit
mkView = do
  todoList <- mkTodoList
  todoAddEdit <- mkTodoAddEdit
  component "View" \_ -> R.do
    router <- useRouter
    { todos } <- useContext todoCtx
    pure $ case router.route of
      Todo List -> todoList unit
      Todo Add -> todoAddEdit { todo: emptyTodo, isEdit: false }
      Todo (Edit idx) -> todoAddEdit
        { todo: fromMaybe emptyTodo $ todos !! idx
        , isEdit: true
        }
      r -> text $ show r

mkNav :: Component Unit
mkNav =
  let
    mkLink :: (Route -> Effect Unit) -> Int -> Tuple Route String -> JSX
    mkLink navigate idx (route /\ name) = keyed (show idx) $ li
      { className: "nav-item"
      , style: css { marginRight: "1rem" }
      , children:
          [ a
              { href: "javascript:void null"
              , onClick: handler_ $ navigate route
              , children: [ text name ]
              }
          ]
      }

    links :: Array (Tuple Route String)
    links =
      [ Todo List /\ "List"
      , Todo Add /\ "Add"
      ]
  in
    component "Nav" \_ -> R.do
      { navigate } <- useRouter
      pure $ nav
        { className: "navbar navbar-expand-lg"
        , style: css { backgroundColor: "#333" }
        , children:
            [ div
                { className: "container-fluid"
                , style: css { justifyContent: "flex-start" }
                , children:
                    [ a
                        { className: "navbar-brand"
                        , href: "javascript:void null"
                        , onClick: handler_ $ navigate Home
                        , children: [ text "PS ToDo" ]
                        }
                    , ul
                        { className: "navbar-nav me-auto mb-2 mb-lg-0"
                        , children: mapWithIndex (mkLink navigate) links
                        }
                    ]
                }
            ]
        }

mkApp :: Component Unit
mkApp = do
  nav <- mkNav
  view <- mkView
  component "App" $ const $ pure $ fragment
    [ nav unit
    , div
        { className: "container"
        , children: [ view unit ]
        }
    ]
