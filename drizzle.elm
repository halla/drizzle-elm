
import Html.App as Html
import Html exposing (div)

import Item
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


type alias Model = List Item.Model

init : (Model, Cmd Item.Msg)
init =
  ([ { id = 1, text = "moi", x = 50, y = 50, color = Color.black, size = 14
  }, { id = 2, text = "fasdfe", x = 150, y = 150, color = Color.black, size = 14
  }], Cmd.none)



subscriptions : Model -> Sub Item.Msg
subscriptions model =
  Sub.none


rndRep text =
  {

 }

update : Item.Msg -> Model -> (Model, Cmd Item.Msg)
update msg model =
  combine (List.map (Item.update msg) model)

combine : List (Item.Model, Cmd Item.Msg) -> (Model, Cmd Item.Msg)
combine list =
  ((List.map fst list), Cmd.batch (List.map snd list))

view model =
  div [] (List.map Item.view model)
