module Main exposing (..)

import List exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)

type alias Model = {}

init : ( Model, Cmd Msg )
init = ( {}, Cmd.none )

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

type Slot = X | O
type alias Row = List Slot
type alias Point = { x : Int, y : Int }
type alias Maze = List (Row)

carve : Int -> Int -> List Point -> List Point
carve x y state =
  if length state == 0 then
    [ { x = 1, y = 0 }
    , { x = x - 2, y = y - 1 }
    ]
  else
    state

generateMaze : Int -> Int -> List (Html Msg)
generateMaze x y =
  let
    spaces = carve x y []
    emptyMaze = repeat y (repeat x X)
  in
    map row (evaluateMaze emptyMaze spaces)

evaluateMaze : List Row -> List Point -> List Row
evaluateMaze maze paths = indexedMap (evaluateRows paths) maze

evaluateRows : List Point -> Int -> Row -> Row
evaluateRows paths y row = indexedMap (evaluateCol paths y) row

evaluateCol : List Point -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

view : Model -> Html Msg
view model = div [ class "maze" ] (generateMaze 5 5)

row : List (Slot) -> Html Msg
row els = div [class "maze__row"] (map renderSlot els)

renderSlot : Slot -> Html Msg
renderSlot slot = case slot of
  X -> div [class "maze__grid maze__grid--blocked"] []
  O -> div [class "maze__grid"] []

main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    }
