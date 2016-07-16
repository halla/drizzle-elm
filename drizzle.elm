import Html exposing (Html, div, text, button)

import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


-- TODO
-- get window size
-- random color
-- random size
-- multiple words
-- import form
-- animations
-- single item input
-- return multiple commands
-- drag items on screen


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { text: String
  , x: Int
  , y: Int
  }

type Msg
  = Shuffle
  | SetRep (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle ->
      (model, Random.generate SetRep (Random.pair (Random.int 1 1600) (Random.int 1 1000)))
    SetRep (x, y) ->
      ({ model | x = x, y = y }, Cmd.none)
    --SetColor (r,g,b) ->



init : (Model, Cmd Msg)
init =
  ({ text = "moi", x = 50, y = 50
  }, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ button [onClick Shuffle] [text "Shuffle"]
    , div [ style [("position", "absolute"), ("left", (toString model.x) ++ "px"), ("top", (toString model.y) ++ "px")] ] [(text model.text)]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


rndRep text =
  {

 }
