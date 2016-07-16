import Html exposing (Html, div, text, button)

import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Color


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
  , color: Color.Color
  , size: Int
  }

type Msg
  = Shuffle
  | SetRep (Int, Int)
  | SetColor Color.Color
  | SetSize Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle ->
      (model ! [ genPosition, genColor, genSize ])
    SetRep (x, y) ->
      ({ model | x = x, y = y }, Cmd.none)
    SetColor color ->
      ({ model | color = color}, Cmd.none)
    SetSize size ->
      ({ model | size = size}, Cmd.none)



init : (Model, Cmd Msg)
init =
  ({ text = "moi", x = 50, y = 50, color = Color.black, size = 14
  }, Cmd.none)


renderStyle model =
  let
    rgba = Color.toRgb model.color
  in
    [ ("position", "absolute")
    , ("left", (toString model.x) ++ "px")
    , ("top", (toString model.y) ++ "px")
    , ("color", "rgb(" ++ toString rgba.red ++ "," ++ toString rgba.green ++ "," ++ toString rgba.blue ++ ")")
    , ("font-size", (toString model.size) ++ "px")
    ]



view : Model -> Html Msg
view model =
  div []
    [ button [onClick Shuffle] [text "Shuffle"]
    , div [ style (renderStyle model) ] [(text model.text)]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


rndRep text =
  {

 }

genSize : Cmd Msg
genSize =
  Random.generate SetSize (Random.int 10 40)

genPosition : Cmd Msg
genPosition =
  Random.generate SetRep (Random.pair (Random.int 1 1600) (Random.int 1 1000))

genColor : Cmd Msg
genColor =
  Random.generate SetColor rgb

rgb : Random.Generator Color.Color
rgb =
  Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)
