module Drizzle exposing (init, view, update, subscriptions)

import Html.App as App
import Html exposing (Html, div, button, text, input, Attribute, textarea, footer, header)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, onWithOptions)


import Item
import Item exposing (Msg(..))
import Thought
import Controls
import Msgs exposing (Msg(..))

import Time exposing (Time, second)

import Debug

import Keyboard
import Char
import String
import Update.Extra exposing (sequence)

-- TODO
-- get window size
-- animations
-- multiple item sets
-- step fwd / backward
-- separate data from representation

-- DONE
-- drag items on screen
-- add items
-- import form
-- single item input
-- random color
-- random size
-- multiple words
-- return multiple commands


-- MODEL

type alias Model =
  { thought : Thought.Model
  , running : Bool
  , importing : Maybe String
  }



init : (Model, Cmd Msgs.Msg)
init = (
  { thought = Thought.init
  , running = False
  , importing = Nothing
  }, Cmd.none)


baseCanvasFontSize = 18

lineHeight = 1.2



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msgs.Msg
subscriptions model =
--..Sub.none
  --NothingHappened
  Sub.batch(
    [ Keyboard.downs (\c -> if (Char.fromCode c == ' ') then ToggleRunning else Msgs.NoOp)
    , Keyboard.downs (\c -> if (Char.fromCode c == 'I') then (insertIfReady model) else Msgs.NoOp)
    , Time.every second Tick
    ] ++
    List.map subHelp model.thought.items)


subHelp : Thought.IndexedItem -> Sub Msgs.Msg
subHelp {id, model} =
  Sub.map (Modify id) (Item.subscriptions model)



insertIfReady model =
  if Thought.isEditing model.thought then  Msgs.NoOp else (Insert "item" True)



--  UPDATE


update : Msgs.Msg -> Model -> (Model, Cmd Msgs.Msg)
update msg ({ running } as model) =
  case msg of
    Import ->
      case model.importing of
        Just txt ->
          { model | importing = Nothing } ! []
            |> sequence update (importUpdater (parseImportString txt))
        _ -> ( model, Cmd.none )

    Importing txt ->
      ( { model | importing = Just txt }, Cmd.none )

    InsertHereAndFocus position ->
      let
        (model', cmd') = Thought.update msg model.thought
      in
        ({ model | thought = model' }, cmd')

    InsertHere position ->
      let
        (model', cmd') = Thought.update msg model.thought
      in
        ({ model | thought = model' }, cmd')


    Insert content doFocus ->
      let
        (model', cmd') = Thought.update msg model.thought
      in
        ({ model | thought = model' }, cmd')

    Modify id msg' ->
      let
        (model', cmd') = Thought.update msg model.thought
      in
        ({ model | thought = model' }, cmd')

    ShuffleAll->
      let
        (model', cmd') = Thought.update msg model.thought
      in
        ({ model | thought = model' }, cmd')

    Tick _ ->
      if
        running == True
      then
        update ShuffleAll model
      else
        (model, Cmd.none)
    ToggleRunning ->
      updateIfReady model ({ model | running = not running }, Cmd.none)
    Msgs.NoOp ->
      (model, Cmd.none)

      -- Cmd.Batch List.map (Modify _.id Item.Msg.Shuffle) items

updateIfReady : Model -> (Model, Cmd Msgs.Msg) -> (Model, Cmd Msgs.Msg)
updateIfReady model modelMsg =
  if
    Thought.isEditing model.thought
  then
    ( model, Cmd.none )
  else
    modelMsg


importUpdater  =
  List.map (\txt -> Insert txt False)

parseImportString : String -> List String
parseImportString txt =
  String.split "\n" txt



--- VIEW

view : Model -> Html Msgs.Msg
view model =
  div [ class "screen" ]
    [ header [] [ Controls.view model ]
    , div [ class "canvas", Controls.insertClick] [ Thought.view model.thought ]
    , footer [] [ text "Drizzle" ]
    ]
