module Events exposing (..)

import Json.Decode as Json
import Html.Events exposing (on, keyCode)
-- See todo.demo


keys =
  { enter = 13
  , esc = 27
  }

onEnter msg =
  onKeyUp [ ( keys.enter, msg ) ]

onEsc msg =
  onKeyUp [ ( keys.esc, msg ) ]

onEscOrEnter esc enter =
  onKeyUp [ ( keys.esc, esc ), ( keys.enter, enter ) ]


onKeyUp options =
    let
        filter optionsToCheck code =
            case optionsToCheck of
                [] ->
                    Err "key code is not in the list"

                ( c, msg ) :: rest ->
                    if (c == code) then
                        Ok msg
                    else
                        filter rest code

        keyCodes =
            Json.customDecoder keyCode (filter options)
    in
      on "keyup" keyCodes
