module Controls exposing (..)

import Html exposing (div, text, Attribute, Html, button, textarea)
import Html.Attributes exposing (class )
import Html.Events exposing (onClick, onInput,  onWithOptions)

import Msgs exposing (..)

import Mouse
import Json.Decode as Json exposing ((:=))

-- VIEW


view model =
  let
    status =
      div [] [ text (if model.running then "Running" else "Stopped")]
    shuffleAll =
      button [ onClick ShuffleAll ] [ text "ShuffleAll"]
    insertButton =
      button [ onClick (Insert "some" True) ] [ text "Insert" ]
  in
    div [ class "controls" ] [ status, shuffleAll, insertButton, (importArea model.importing)]


importArea : Maybe String -> Html Msgs.Msg
importArea importing =
  div []
    [ textarea [ onInput Importing ] []
    , button [ onClick Import ] [ text "Import" ]
    ]


insertClick : Attribute Msgs.Msg
insertClick =
  onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.map InsertHereAndFocus Mouse.position)
