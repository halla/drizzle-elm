module Msgs exposing (..)
import Item exposing (Msg(..))
import Mouse exposing (Position)
import Time exposing (Time)

type Msg
  = Insert String
  | InsertHere Position
  | Import
  | Importing String
  | Modify Int Item.Msg
  | ShuffleAll
  | Tick Time
  | ToggleRunning
  | NoOp
