module Item exposing (..)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Color
import Random
-- import Window


type alias Model =
  { id: Int
  , text: String
  , x: Int
  , y: Int
  , color: Color.Color
  , size: Int
  }

type Msg
  = Shuffle Int
  | SetRep Int (Int, Int)
  | SetColor Int Color.Color
  | SetSize Int Int


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Shuffle id ->
      if id == model.id then
        (model ! [ (genPosition id), (genColor id), (genSize id) ])
      else
        (model, Cmd.none)
    SetRep id (x, y) ->
      if id == model.id then
        ({ model | x = x, y = y }, Cmd.none)
      else
        (model, Cmd.none)

    SetColor id color ->
      if id == model.id then
        ({ model | color = color}, Cmd.none)
      else
        (model, Cmd.none)
    SetSize id size ->
      if id == model.id then
        ({ model | size = size}, Cmd.none)
      else
        (model, Cmd.none)


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
  div []
    [ button [onClick (Shuffle model.id)] [text "Shuffle"]
    , div [ style (renderStyle model) ] [(text model.text)]
    ]

genSize : Int -> Cmd Msg
genSize id =
  Random.generate (SetSize id) (Random.int fontMin fontMax)

genPosition : Int -> Cmd Msg
genPosition id =
  Random.generate (SetRep id) (Random.pair (Random.int 1 xMax) (Random.int 1 yMax))

genColor : Int -> Cmd Msg
genColor id =
  Random.generate (SetColor id) rgb

rgb : Random.Generator Color.Color
rgb =
  Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)
