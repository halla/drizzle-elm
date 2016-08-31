port module Main exposing (..)

import Drizzle exposing (init, view, update, subscriptions)

import Html.App

main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
