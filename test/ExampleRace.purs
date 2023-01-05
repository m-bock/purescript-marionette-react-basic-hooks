module Test.ExampleRace where

import Prelude

import Control.Monad.State (get, modify, put)
import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Marionette.Controllers.Monadic (MarionetteT, sendMsg)
import Marionette.Controllers.Monadic as Monadic
import Marionette.ReactBasic (useMarionette)
import React.Basic.DOM (text)
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Simplified.Generated as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, (/\))
import React.Basic.Hooks as React
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type State = Int

data Msg = Tick | User

control :: Msg -> MarionetteT Msg State Aff Unit
control = case _ of
  Tick -> do
    log "start"
    ( get >>= \x -> do
        liftAff $ delay (Milliseconds 1.0)
        log $ show x
    )
      # replicate 10000
      # sequence_

  User -> put 100

mkApp :: Component {}
mkApp = component "App" \_ -> React.do
  state /\ act <- useMarionette
    { initialState: 0
    , controller: Monadic.mkController control
    }

  pure $ R.div {}
    [ R.div {}
        [ text $ show state
        ]
    , R.button { onClick: handler_ $ act Tick }
        [ text "tick"
        ]
    , R.button { onClick: handler_ $ act User }
        [ text "user"
        ]
    ]

main :: Effect Unit
main = do
  elem <- elemById "root"
    >>= maybe (throw "Could not find container element") pure

  reactRoot <- createRoot elem
  app <- mkApp
  renderRoot reactRoot (app {})

elemById :: String -> Effect (Maybe Element)
elemById id = do
  doc <- document =<< window
  getElementById id $ toNonElementParentNode doc