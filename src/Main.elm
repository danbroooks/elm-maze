module Main exposing (..)

import Generator exposing (Point, carve)
import List exposing (..)
import List.Util exposing (..)
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

generateMaze : Int -> Int -> Int -> Maze
generateMaze x y seed =
  let
    prepop = [{ x = x, y = y + 1 }]
    spaces = flatten [prepop, carve (initialSeed seed) x y { x = 1, y = 0 } []]
    emptyMaze = repeat (y + 2) <| repeat (x + 2) X
  in
    indexedMap (evaluateRows spaces) emptyMaze

evaluateRows : List Point -> Int -> Row -> Row
evaluateRows paths y row = row |> indexedMap (evaluateCol paths y)

evaluateCol : List Point -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

view : Model -> Html Msg
view model = div [ class "maze" ] <| map row <| generateMaze 20 20 model.seed

row : Row -> Html Msg
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
