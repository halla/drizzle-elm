module Drizzle exposing (init, view, update, subscriptions)

import Html.App as App
import Html exposing (Html, div, button, text, input, Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, onWithOptions)
import Item
import Item exposing (Msg(..))
import Color
import Time exposing (Time, second)
import Char
import Keyboard
import Debug
import Mouse exposing (Position)
import Json.Decode as Json exposing ((:=))

-- TODO
-- get window size
-- import form
-- single item input
-- drag items on screen
-- animations
-- add items
--
-- DONE
-- random color
-- random size
-- multiple words
-- return multiple commands



type alias Model =
  { items : List IndexedItem
  , uid : Int
  , running : Bool
  }

type alias IndexedItem =
  { id : Int
  , model : Item.Model
  }


init : (Model, Cmd Msg)
init =
  ({ items = []
  , uid = 0
  , running = False
  }, Cmd.none)
  --([ { id = 1, text = "moi", x = 50, y = 50, color = Color.black, size = 14
  --}, { id = 2, text = "fasdfe", x = 150, y = 150, color = Color.black, size = 14
  --}], Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
--..Sub.none
  --NothingHappened
  Sub.batch(
    [ Keyboard.downs (\c -> if (Char.fromCode c == ' ') then ToggleRunning else NoOp)
    , Keyboard.downs (\c -> if (Char.fromCode c == 'I') then (insertIfReady model) else NoOp)
    , Time.every second Tick
    ] ++
    List.map subHelp model.items)


subHelp : IndexedItem -> Sub Msg
subHelp {id, model} =
  Sub.map (Modify id) (Item.subscriptions model)



insertIfReady model =
  if List.any (\i -> i.model.editing == True ) model.items then NoOp else (Insert "item")

--  MODEL


type Msg
  = Insert String
  | InsertHere Position
  | Modify Int Item.Msg
  | ShuffleAll
  | Tick Time
  | ToggleRunning
  | NoOp

dummyItem uid content position' =
  { text = content ++ (toString uid), x = (50 + uid), y = (50 + uid), color = Color.black, size = 18, drag = Nothing, position = position', editing = False, nextText = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({items, uid, running } as model) =
  case msg of
    InsertHere position ->
      let
        m2 = { model
          | items = items ++ [ IndexedItem uid (dummyItem uid "content" position)]
          , uid = uid + 1
          }
      in
        update ((Modify uid) StartEditing) m2
    Insert content ->
      let
        m2 = { model
          | items = items ++ [ IndexedItem uid (dummyItem uid content (Position (200 + uid) 200))]
          , uid = uid + 1
          }
      in
        Debug.log(content)
        update ((Modify uid) StartEditing) m2

    Modify id msg ->
      let
        xs = List.map (updateHelp id msg) items
        items = List.map fst xs
        cmd = Cmd.batch (List.map snd xs)
      in
        ({ model | items = items }, cmd)
    ShuffleAll->
      update ((Modify 1) Shuffle) model
    Tick _ ->
      if
        running == True
      then
        update ShuffleAll model
      else
        (model, Cmd.none)
    ToggleRunning ->
      Debug.log "hip"
      ({ model | running = not running }, Cmd.none)
    NoOp ->
      Debug.log "asdf"
      (model, Cmd.none)

      -- Cmd.Batch List.map (Modify _.id Item.Msg.Shuffle) items


updateHelp : Int -> Item.Msg -> IndexedItem -> (IndexedItem, Cmd Msg)
updateHelp targetId msg {id, model} =
  let
    (model, cmd) = if (targetId == id) then (Item.update msg model) else (model, Cmd.none)
  in
    (IndexedItem id model, Cmd.map (Modify id) cmd)


  --combine (List.map (Item.update msg) model)

--combine : List (Item.Model, Cmd Item.Msg) -> (Model, Cmd Item.Msg)
--combine list =
  --((List.map fst list), Cmd.batch (List.map snd list))



view : Model -> Html Msg
view model =
  let
    status =
      div [] [ text (if model.running then "Running" else "Stopped")]
    shuffleAll =
      button [ onClick ShuffleAll ] [ text "ShuffleAll"]
    insertButton =
      button [ onClick (Insert "some") ] [ text "Insert" ]
    items =
      List.map viewIndexedItem model.items
  in
    div [ class "screen"
        , style [("height", "100vh")]
        , insertClick
        ]
      [ div [ class "controls" ] [ status, shuffleAll, insertButton ]
      , div [ class "canvas" ] items
      ]


insertClick : Attribute Msg
insertClick =
  onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.map InsertHere Mouse.position)

viewIndexedItem : IndexedItem -> Html Msg
viewIndexedItem {id, model} =
  App.map (Modify id) (Item.view model)
