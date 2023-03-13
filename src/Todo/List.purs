module Todo.List where

import Prelude hiding (div)

import App.Router (useRouter)
import App.Routes (Route(..), Todo(..))
import Data.Array (deleteAt, mapWithIndex)
import Data.Lens (over)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import React.Basic.DOM (button, css, div, h1, table, tbody_, td_, text, th, th_, thead_, tr_)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, keyed, useContext)
import React.Basic.Hooks as R
import Todo.Ctx (todoCtx)

mkTodoList :: Component Unit
mkTodoList = component "TodoList" \_ -> R.do
  { todos, setTodos } <- useContext todoCtx
  { navigate } <- useRouter
  pure $ div
    { className: "row mt-5"
    , children:
        [ h1
            { className: "col-sm-12"
            , children: [ text "ToDo List" ]
            }
        , table
            { className: "table col-sm-12"
            , children:
                [ thead_
                    [ tr_
                        [ th_ [ text "Name" ]
                        , th_ [ text "Description" ]
                        , th_ [ text "Done" ]
                        , th
                            { className: "col-sm-3"
                            , children: [ text "Actions" ]
                            }
                        ]
                    ]
                , tbody_ $ mapWithIndex <@> todos $ \idx { name, desc, done } ->
                    keyed (show idx) $ tr_
                      [ td_ [ text name ]
                      , td_ [ text desc ]
                      , td_ [ text $ show done ]
                      , td_
                          [ div
                              { style: css
                                  { display: "flex"
                                  , alignItems: "center"
                                  , justifyContent: "space-between"
                                  }
                              , children:
                                  [ button
                                      { className: "btn btn-sm btn-" <> if done then "danger" else "success"
                                      , onClick: handler_ $ setTodos $ over (ix idx) \t -> t { done = not t.done }
                                      , children: [ text $ if done then "undone" else "done" ]
                                      }
                                  , button
                                      { className: "btn btn-sm btn-primary"
                                      , onClick: handler_ $ navigate $ Todo $ Edit idx
                                      , children: [ text "edit" ]
                                      }
                                  , button
                                      { className: "btn btn-sm btn-warning"
                                      , onClick: handler_ $ setTodos \_ -> do
                                          case deleteAt idx todos of
                                            Just new -> new
                                            _ -> todos
                                      , children: [ text "remove" ]
                                      }
                                  ]
                              }
                          ]
                      ]
                ]
            }
        ]
    }
