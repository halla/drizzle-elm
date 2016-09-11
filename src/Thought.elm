module Thought exposing (..)

import Random
import Msgs exposing (Msg(..))
import Item
import Item exposing (Msg(..))
import Mouse exposing (Position)
import Color
import Update.Extra exposing (sequence)
import Html.App as App
import Html exposing (Html, div)
import Html.Attributes exposing (class)

--- MODEL
init =
  { items = []
  , uid = 0
  , seed = (Random.initialSeed 4344362345)
  }

type alias Model =
  { items : List IndexedItem
  , uid : Int
  , seed : Random.Seed
  }

type alias IndexedItem =
  { id : Int
  , model : Item.Model
  }

baseCanvasFontSize = 18

lineHeight = 1.2

nItems = 10

dummyItem uid content position' =
  { text = content ++ (toString uid), x = (50 + uid), y = (50 + uid), color = Color.black, size = baseCanvasFontSize, drag = Nothing, position = position', editing = Nothing }

--- VIEW

view : Model -> Html Msgs.Msg
view model =
  let
    visibleItems = List.take nItems model.items
  in
    div [ class "thought" ] (List.map viewIndexedItem visibleItems)

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
      let
        (rndList, seed') = Random.step (Random.list (List.length model.items) (Random.int 1 100)) model.seed
        items' = List.map2 (,) rndList model.items |> List.sortBy fst |> List.unzip |> snd
      in
        { model | items = items', seed = seed' } ! []
          |> sequence update (List.map (\i -> ((Modify i.id) Shuffle))  model.items)

    _ -> ( model, Cmd.none )
    
updateHelp : Int -> Item.Msg -> IndexedItem -> (IndexedItem, Cmd Msgs.Msg)
updateHelp targetId msg {id, model} =
  let
    (model, cmd) = if (targetId == id) then (Item.update msg model id) else (model, Cmd.none)
  in
    (IndexedItem id model, Cmd.map (Modify id) cmd)
