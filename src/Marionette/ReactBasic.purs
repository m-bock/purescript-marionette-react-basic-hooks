module Marionette.ReactBasic
  ( UseMarionette
  , useMarionette
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Newtype (over)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Marionette (defaultConfig, noRenderer, runProgram)
import Marionette as Marionette
import Marionette.Types (Controller, Renderer(..), State(..))
import React.Basic.Hooks (type (&), type (/\), Hook, Render, UseState, useState, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- Hook
--------------------------------------------------------------------------------

-- | Opaque type of the `useMarionette` hook

newtype UseMarionette msg sta hooks = UseMarionette
  ( hooks
      & UseState sta
      & UseState (msg -> Effect Unit)
      & UseAff Unit Unit
  )

-- | Configuration options for the `useMarionette` hook.
-- |
-- | Fields
-- | - `initialState` Initial state value of the state machine 
-- | - `controller` specifies the how the control flow is handled 

type Config msg sta =
  { initialState :: sta
  , controller :: Controller msg sta
  }

-- | Main function of this library. it works in the same manner as e.g. `useReducer`.
-- |
-- | ```
-- | import Marionette.Controllers.Monadic (mkController)
-- |
-- | type State = Int
-- | type Msg = CountUp | CountDown
-- | 
-- | control = case _ of
-- |   CountUp -> modify_ (_ + 1)
-- |   CountDown -> modify_ (_ - 1)
-- |
-- | mkApp :: Component {}
-- | mkApp = component "App" \_ -> React.do
-- |   state /\ act <- useMarionette
-- |     { initialState: 0
-- |     , controller: mkController myControl
-- |     }
-- |   pure $
-- |     R.div'
-- |       [ R.div' [ text $ show state ]
-- |       , R.button { onClick: handler_ $ act CountUp }
-- |       ]
-- | ```

useMarionette
  :: forall msg sta
   . Eq sta
  => Config msg sta
  -> Hook (UseMarionette msg sta) (sta /\ (msg -> Effect Unit))
useMarionette config = coerceHookWith UseMarionette $
  React.do
    state /\ setState <- useState config.initialState

    act /\ setAct <- useState' (\_ -> pure unit)

    useAff unit do
      let
        renderer :: Renderer msg sta
        renderer = noRenderer
          # over Renderer
              _ { onInit = \act' -> liftEffect $ setAct (act' >>> launchAff_) }

      let
        program :: Marionette.Program msg sta
        program =
          { controller: config.controller
          , initialState: config.initialState
          , renderer
          }

      let
        marionetteConfig :: Marionette.Config msg sta
        marionetteConfig = defaultConfig
          { stateHandler = \_ -> pure $ State \f -> reactSetterToState setState f
          }

      void $
        runProgram program marionetteConfig

    pure (state /\ act)

--------------------------------------------------------------------------------
--- Util
--------------------------------------------------------------------------------

type TupleApi a b =
  { fst :: Tuple a b -> a
  , snd :: Tuple a b -> b
  }

tupleApi :: forall a b. TupleApi a b
tupleApi = { snd, fst }

foreign import reactSetterToStateImpl
  :: forall a s
   . TupleApi a s
  -> ((s -> s) -> Effect Unit)
  -> (s -> Tuple a s)
  -> Effect (Promise a)

-- | Turns a React state setter into a general State implementation

reactSetterToState :: forall s a. ((s -> s) -> Effect Unit) -> (s -> Tuple a s) -> Aff a
reactSetterToState setState f = toAffE $
  reactSetterToStateImpl tupleApi setState f

-- | Used instead of `coerceHook` which would require a Newtype instance

coerceHookWith :: forall a b b' c. (b -> b') -> Render a b c -> Render a b' c
coerceHookWith = unsafeCoerce
