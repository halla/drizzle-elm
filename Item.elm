module Item exposing (..)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Color
import Random
-- import Window


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


-- UPDATE

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


--  VIEW


yMax = 1000
xMax = 1600
fontMin = 10
fontMax = 40

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
  div [style [("position", "absolute")]]
    [ button [onClick Shuffle] [text "Shuffle"]
    , div [ style (renderStyle model) ] [(text model.text)]
    ]

genSize : Cmd Msg
genSize =
  Random.generate SetSize (Random.int fontMin fontMax)

genPosition : Cmd Msg
genPosition =
  Random.generate SetRep (Random.pair (Random.int 1 xMax) (Random.int 1 yMax))

genColor : Cmd Msg
genColor =
  Random.generate SetColor rgb

rgb : Random.Generator Color.Color
rgb =
  Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)
