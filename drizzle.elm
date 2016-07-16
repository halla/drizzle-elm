
import Html.App as Html

import Item
import Color

-- TODO
-- get window size
-- multiple words
-- import form
-- animations
-- single item input
-- return multiple commands
-- drag items on screen
--
-- DONE
-- random color
-- random size

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model = Item.Model

init : (Model, Cmd Item.Msg)
init =
  ({ text = "moi", x = 50, y = 50, color = Color.black, size = 14
  }, Cmd.none)



subscriptions : Model -> Sub Item.Msg
subscriptions model =
  Sub.none


rndRep text =
  {

 }


update msg model =
  Item.update msg model


view model =
  Item.view model
