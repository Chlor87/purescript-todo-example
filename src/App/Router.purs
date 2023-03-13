module App.Router where

import Prelude

import App.Routes (Route(..), Todo(..), routes)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks
  ( Hook
  , JSX
  , ReactContext
  , UseContext
  , Component
  , coerceHook
  , component
  , createContext
  , provider
  , useContext
  , useEffectOnce
  , useState'
  , (/\)
  )
import React.Basic.Hooks as R
import Routing.Duplex (parse, print)
import Web.Router as Router
import Web.Router.PushState as PushState

type Router =
  { route :: Route
  , navigate :: Route -> Effect Unit
  , redirect :: Route -> Effect Unit
  }

newtype UseRouter :: Type -> Type
newtype UseRouter hooks = UseRouter (UseContext Router hooks)

derive instance Newtype (UseRouter hooks) _

useRouter :: Hook UseRouter Router
useRouter = coerceHook do
  useContext routerContext

routerContext :: ReactContext Router
routerContext = unsafePerformEffect $ createContext
  { route: Home
  , navigate: mempty
  , redirect: mempty
  }

mkRouter :: Component (Array JSX)
mkRouter = do
  ref <- Ref.new mempty
  let
    onNavigation _ Home = Router.redirect $ Todo List
    onNavigation _ _ = Router.continue

    onEvent = case _ of
      Router.Routing _ _ -> pure unit
      Router.Resolved _ route -> do
        -- get `setRoute` from ref
        setRoute <- Ref.read ref
        setRoute route
  driver <- PushState.mkInterface (parse routes) (print routes)
  { navigate, redirect, initialize } <- Router.mkInterface onNavigation onEvent driver
  component "Router" \children -> R.do
    route /\ setRoute <- useState' $ Home
    useEffectOnce do
      -- set `setRoute` in ref for use in `onEvent`
      Ref.write setRoute ref
      initialize
    pure $ provider routerContext { route, navigate, redirect } children
