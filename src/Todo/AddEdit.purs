module Todo.AddEdit where

import Prelude hiding (div)

import App.Router (useRouter)
import App.Routes as Routes
import Data.Array (snoc)
import Data.Lens (set)
import Data.Lens.Index (ix)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.DOM (button, css, div, h1, input, label, text)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
import React.Basic.Hooks as R
import Todo.Ctx (Todo, _desc, _name, useTodos)

textField :: String -> String -> (String -> Effect Unit) -> JSX
textField name value update = div
  { className: "col-sm-12 mt-3"
  , children:
      [ label
          { className: "form-label"
          , htmlFor: name
          , children: [ text name ]
          }
      , input
          { id: name
          , className: "form-control"
          , type: "text"
          , value: value
          , onChange: handler targetValue $ update <<< fromMaybe ""
          }
      ]
  }

mkTodoAddEdit :: Component ({ todo :: Todo, isEdit :: Boolean })
mkTodoAddEdit = component "TodoAddEdit" \{ todo: todo', isEdit } -> R.do
  todo /\ setTodo <- useState todo'
  { setTodos } <- useTodos
  { navigate, route } <- useRouter
  pure $ div
    { className: "row mt-5"
    , children:
        [ h1
            { className: "col-sm-12"
            , children: [ text $ (if isEdit then "Edit" else "Add") <> " Todo" ]
            }
        , textField "Name" todo.name $ setTodo <<< set _name
        , textField "Description" todo.desc $ setTodo <<< set _desc
        , div
            { className: "col-sm-12 mt-3"
            , style: css { display: "flex", justifyContent: "flex-end" }
            , children:
                [ button
                    { className: "btn btn-sm btn-primary"
                    , onClick: handler_ do
                        when isEdit do
                          case route of
                            Routes.Todo (Routes.Edit idx) ->
                              setTodos $ set (ix idx) todo
                            _ -> pure unit
                        when (not isEdit) $ setTodos $ snoc <@> todo
                        navigate $ Routes.Todo Routes.List
                    , children: [ text "Save" ]
                    }
                ]
            }
        ]
    }
