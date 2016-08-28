port module Item exposing (..)

import Html exposing (Html, div, text, button, Attribute, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, onBlur, onInput, onWithOptions)
import Mouse exposing (Position)
import Json.Decode as Json exposing ((:=))

import Color
import Random
-- import Window

port focus : String -> Cmd msg

type alias Model =
  { text: String
  , x: Int
  , y: Int
  , color: Color.Color
  , size: Int
  , position: Position
  , drag: Maybe Drag
  , editing: Bool
  , nextText: String
  }

type alias Drag =
  { start : Position
  , current : Position
  }

type Msg
  = Shuffle
  | SetRep (Int, Int)
  | SetColor Color.Color
  | SetSize Int
  | DragAt Position
  | DragStart Position
  | DragEnd Position
  | CommitEditing
  | StartEditing
  | NextText String
  | NoOp


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

    DragStart xy ->
      ({ model | drag = (Just (Drag xy xy)) }, Cmd.none)

    DragAt xy ->
      ({ model | drag = (Maybe.map (\{start} -> Drag start xy) model.drag) }, Cmd.none)

    DragEnd _ ->

      ({ model | position = (getPosition model), drag = Nothing }, Cmd.none)

    CommitEditing ->
      ({ model | editing = False, text = model.nextText, nextText = "" }, Cmd.none )
    StartEditing ->
      ({ model | editing = True, nextText = model.text}, focus ("#" ++ divId model ++ " input"))
    NextText text ->
      ({ model | nextText = text }, Cmd.none )
    NoOp ->
      ( model, Cmd.none )

divId model =
  "item-" ++ model.text


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt ]


--  VIEW


yMax = 1000
xMax = 1600
fontMin = 10
fontMax = 40

renderStyle model =
  let
    rgba = Color.toRgb model.color
    realPosition = getPosition model
  in
    [ ("position", "absolute")
    , ("left", px realPosition.x)
    , ("top", px realPosition.y)
    , ("padding", px 10)
    , ("color", "rgb(" ++ toString rgba.red ++ "," ++ toString rgba.green ++ "," ++ toString rgba.blue ++ ")")
    , ("font-size", (toString model.size) ++ "px")
    ]



view : Model -> Html Msg
view model =
  let
    itemView = div [ editClick ] [(text model.text)]
    itemEdit = input
      [ onInput NextText
      , onBlur CommitEditing
      , value model.nextText
      , autofocus True
      , editStopClick
      ] []
    item = if model.editing then itemEdit else itemView

  in
    div [ id (divId model), onMouseDown, onMouseUp, style (renderStyle model)]
      [
      item
      ]



editClick : Attribute Msg
editClick =
  onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed StartEditing)

editStopClick : Attribute Msg
editStopClick =
  onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed NoOp)

px : Int -> String
px number =
  toString number ++ "px"

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


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)

onMouseUp : Attribute Msg
onMouseUp =
  onWithOptions "mouseup" { stopPropagation = True, preventDefault = True } (Json.map DragEnd Mouse.position)
