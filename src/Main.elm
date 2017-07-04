module Main exposing (..)

import Generator exposing (Point, carve)
import List exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)
import Random exposing (initialSeed)

type alias Model = { seed : Int }

init : Model -> ( Model, Cmd Msg )
init flags = ( flags, Cmd.none )

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

type Slot = X | O
type alias Row = List Slot
type alias Maze = List Row

generateMaze : Int -> Int -> Int -> List Row
generateMaze x y seed =
  let
    spaces = carve (initialSeed seed) x y { x = 1, y = 1 } []
    emptyMaze = repeat (y + 2) <| repeat (x + 2) X
  in
    evaluateMaze emptyMaze <| { x = 1, y = 0 } :: { x = x, y = y + 1 } :: spaces

evaluateMaze : List Row -> List Point -> List Row
evaluateMaze maze paths = indexedMap (evaluateRows paths) maze

evaluateRows : List Point -> Int -> Row -> Row
evaluateRows paths y row = indexedMap (evaluateCol paths y) row

evaluateCol : List Point -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

view : Model -> Html Msg
view model = div [ class "maze" ] <| map row <| generateMaze 20 20 model.seed

row : List (Slot) -> Html Msg
row els = div [class "maze__row"] <| map renderSlot els

renderSlot : Slot -> Html Msg
renderSlot slot = case slot of
  X -> div [class "maze__grid maze__grid--blocked"] []
  O -> div [class "maze__grid"] []

main : Program Model Model Msg
main =
  Html.programWithFlags
    { view = view
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    }
