
import Html.App as Html
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Item
import Item exposing (Msg(..))
import Color

-- TODO
-- get window size
-- import form
-- animations
-- single item input
-- drag items on screen
-- add items
--
-- DONE
-- random color
-- random size
-- multiple words
-- return multiple commands

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { items : List IndexedItem
  , uid : Int
  }

type alias IndexedItem =
  { id : Int
  , model : Item.Model
  }


init : (Model, Cmd Msg)
init =
  ({ items = []
  , uid = 0
  }, Cmd.none)
  --([ { id = 1, text = "moi", x = 50, y = 50, color = Color.black, size = 14
  --}, { id = 2, text = "fasdfe", x = 150, y = 150, color = Color.black, size = 14
  --}], Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


type Msg
  = Insert
  | Modify Int Item.Msg
  | ShuffleAll



update : Msg -> Model -> (Model, Cmd Msg)
update msg ({items, uid} as model) =
  case msg of
    Insert ->
      ( { model
        | items = items ++ [ IndexedItem uid { id = uid, text = "moi", x = 50, y = 50, color = Color.black, size = 14 }]
        , uid = uid + 1
        },
        Cmd.none
      )
    Modify id msg ->
      let
        xs = List.map (updateHelp id msg) items
        items = List.map fst xs
        cmd = Cmd.batch (List.map snd xs)
      in
        ({ model | items = items }, cmd)
    ShuffleAll->
      update ((Modify 1) (Shuffle 1)) model

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
    insert =
      button [ onClick Insert ] [ text "Insert" ]
    shuffleAll =
      button [ onClick ShuffleAll ] [ text "ShuffleAll"]

    items =
      List.map viewIndexedItem model.items
  in
    div [class "screen"]
      [ div [] ([ shuffleAll ] ++ [ insert ] ++ items)
      ]

viewIndexedItem : IndexedItem -> Html Msg
viewIndexedItem {id, model} =
  Html.map (Modify id) (Item.view model)
