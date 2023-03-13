module Todo.Ctx
  ( Todo
  , Todos
  , TodoCtx
  , emptyTodo
  , todoCtx
  , mkTodoProvider
  , _name
  , _desc
  , _done
  ) where

import Prelude

import Data.Either (hush)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.LocalStorage (getItem, setItem)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (JSX, ReactContext, createContext, provider)
import React.Basic.Hooks (Component, component, useEffect, useState, (/\))
import React.Basic.Hooks as R
import Simple.JSON (readJSON, writeJSON)
import Type.Proxy (Proxy(..))

type Todo =
  { name :: String
  , desc :: String
  , done :: Boolean
  }

type Todos = Array Todo

type TodoCtx =
  { todos :: Todos
  , setTodos :: (Todos -> Todos) -> Effect Unit
  }

emptyTodo :: Todo
emptyTodo = { name: "", desc: "", done: false }

_name :: ∀ a r. Lens' { name :: a | r } a
_name = prop (Proxy :: Proxy "name")

_desc :: ∀ a r. Lens' { desc :: a | r } a
_desc = prop (Proxy :: Proxy "desc")

_done :: ∀ a r. Lens' { done :: a | r } a
_done = prop (Proxy :: Proxy "done")

todoCtx :: ReactContext TodoCtx
todoCtx = unsafePerformEffect $ createContext { todos: [], setTodos: mempty }

loadTodos :: Effect Todos
loadTodos = do
  mTodos <- getItem "todos"
  case mTodos of
    Just str -> pure $ fromMaybe [] $ hush $ readJSON str
    _ -> pure []

mkTodoProvider :: Component (Array JSX)
mkTodoProvider = do
  lsTodos <- loadTodos
  component "TodoProvider" \children -> R.do
    todos /\ setTodos <- useState lsTodos
    useEffect [ todos ] do
      setItem "todos" $ writeJSON todos
      pure mempty
    pure $ provider todoCtx { todos, setTodos } children
