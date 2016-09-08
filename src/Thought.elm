module Thought exposing (..)

import Msgs exposing (Msg(..))
import Item
import Item exposing (Msg(..))
import Mouse exposing (Position)
import Color
import Html exposing (Html)
import Update.Extra exposing (sequence)
import Html.App as App

--- MODEL
init =
  { items = []
  , uid = 0
  }

type alias Model =
  { items : List IndexedItem
  , uid : Int
  }

type alias IndexedItem =
  { id : Int
  , model : Item.Model
  }

baseCanvasFontSize = 18

lineHeight = 1.2


dummyItem uid content position' =
  { text = content ++ (toString uid), x = (50 + uid), y = (50 + uid), color = Color.black, size = baseCanvasFontSize, drag = Nothing, position = position', editing = Nothing }

--- VIEW

viewIndexedItem : IndexedItem -> Html Msgs.Msg
viewIndexedItem {id, model} =
  App.map (Modify id) (Item.view model id)



--- UPDATE
update : Msgs.Msg -> Model -> (Model, Cmd Msgs.Msg)
update msg ({items, uid} as model) =
  case msg of
    InsertHere position ->
      let
        m2 = { model
          | items = items ++ [ IndexedItem uid (dummyItem uid "content" position)]
          , uid = uid + 1
          }
      in
        --(updateIfReady model (update ((Modify uid) StartEditing) m2))
        update ((Modify uid) StartEditing) m2
    Insert content ->
      let
        m2 = { model
          | items = items ++ [ IndexedItem uid (dummyItem uid content (Position 200 (200 + uid * round(baseCanvasFontSize * lineHeight))))]
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
      model ! []
        |> sequence update (List.map (\i -> ((Modify i.id) Shuffle))  model.items)

    _ -> ( model, Cmd.none )
updateHelp : Int -> Item.Msg -> IndexedItem -> (IndexedItem, Cmd Msgs.Msg)
updateHelp targetId msg {id, model} =
  let
    (model, cmd) = if (targetId == id) then (Item.update msg model id) else (model, Cmd.none)
  in
    (IndexedItem id model, Cmd.map (Modify id) cmd)
