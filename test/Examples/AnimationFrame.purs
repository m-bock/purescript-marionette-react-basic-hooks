module Test.Examples.AnimationFrame where

import Prelude

import Control.Monad.State (class MonadState, modify_, state)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Number (pi)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple as Tpl
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (throw)
import Marionette.Controllers.Monadic (MarionetteT, sendMsg)
import Marionette.Controllers.Monadic as Monadic
import Marionette.ReactBasic (useMarionette)
import React.Basic.DOM (css, text)
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Simplified.Generated as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, (/\))
import React.Basic.Hooks as React
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (cancelAnimationFrame, document, requestAnimationFrame)

--------------------------------------------------------------------------------
--- Model
--------------------------------------------------------------------------------

data State
  = Paused Int
  | Running Int

derive instance Eq State

derive instance Generic State _

instance Show State where
  show = genericShow

data Msg
  = Start
  | Tick
  | Pause

--------------------------------------------------------------------------------
--- Control
--------------------------------------------------------------------------------

control :: Msg -> MarionetteT Msg State Aff Unit
control = case _ of
  Start -> modifyDo case _ of
    Paused n -> Running n /\
      sendMsg Tick
    s -> s /\ pure unit

  Pause -> modify_ case _ of
    Running n -> Paused n
    s -> s

  Tick -> modifyDo case _ of
    Running n -> Running (n + 1) /\ do
      liftAff $ reqAnimFram
      sendMsg Tick
    s -> s /\ pure unit

--------------------------------------------------------------------------------
--- View
--------------------------------------------------------------------------------

mkApp :: Component {}
mkApp = do

  component "App" \_ -> React.do
    state /\ act <- useMarionette
      { initialState: Paused 0
      , controller: Monadic.mkController $ control
      }

    let
      rotPerTick = 0.01

      rad =
        case state of
          Running n -> n
          Paused n -> n
          # toNumber >>> (_ * pi) >>> (_ * rotPerTick)

    pure $
      R.div'
        [ R.h1' [ text "AnimationFrame" ]
        , R.pre' [ text $ show state ]
        , R.div
            { style: css
                { border: "1px solid red"
                , width: "100px"
                , height: "100px"
                , margin: "40px"
                , transform: "rotate(" <> show rad <> "rad)"
                }
            }
            [ text "" ]
        , case state of
            Running _ -> R.button { onClick: handler_ $ act Pause }
              [ text "pause" ]
            Paused _ -> R.button { onClick: handler_ $ act Start }
              [ text "start" ]
        ]

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  elem <- elemById "root"
    >>= maybe (throw "Could not find container element") pure

  reactRoot <- createRoot elem
  app <- mkApp
  renderRoot reactRoot (app {})

--------------------------------------------------------------------------------
--- Util
--------------------------------------------------------------------------------

elemById :: String -> Effect (Maybe Element)
elemById id = do
  doc <- document =<< window
  getElementById id $ toNonElementParentNode doc

reqAnimFram :: Aff Unit
reqAnimFram = makeAff \cb -> do
  win <- window
  id <- requestAnimationFrame (cb $ Right unit) win
  pure $ effectCanceler $ cancelAnimationFrame id win

modifyDo :: forall s m a. MonadState s m => (s -> Tuple s (m a)) -> m a
modifyDo f = state (f >>> Tpl.swap) >>= identity
