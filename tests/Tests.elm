module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import Item exposing (update)
import Item exposing (Msg(..))

import Mouse exposing (Position)
import Color

all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \() ->
                Expect.equal (3 + 7) 10
        , test "String.left" <|
            \() ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , testItemEdit

        ]


testItemEdit =

  let
    model = { text = "text", x = 50, y = 50, color = Color.black, size = 18, drag = Nothing, position = (Position 50 50), editing = False, nextText = "" }
    assert f expected (model, cmd) =
      Expect.equal (f model) expected
  in
    test "starts editing" <|
      \() ->
        (model
          |> update StartEditing
          |> assert .editing True
        )
