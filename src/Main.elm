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
type alias Maze = List Row

carve : Int -> Int -> Point -> List Point -> List Point
carve x y current state =
  let
    next = flatten [[current], state]
    rec = carve x y
    isValid = validSlot x y
  in
    if length state == 0 || isValid current then
      current
      |> availableSlots x y next
      |> foldr rec next
    else
      []

availableSlots : Int -> Int -> List Point -> Point -> List Point
availableSlots x y state current =
  let
    isValid = validSlot x y
    isMember = (\pt -> member pt state)
  in
    current
    |> neighbors
    |> reject isMember
    |> filter isValid

validSlot : Int -> Int -> Point -> Bool
validSlot x y pt = (pt.x > 0 && pt.x < (x - 1) && pt.y > 0 && pt.y < (y - 1))

reject : (a -> Bool) -> List a -> List a
reject f xs = filter (\x -> f x == False) xs

neighbors : Point -> List Point
neighbors pt =
  [ { x = pt.x - 1, y = pt.y }
  , { x = pt.x + 1, y = pt.y }
  , { x = pt.x,     y = pt.y - 1 }
  , { x = pt.x,     y = pt.y + 1 }
  ]

flatten : List (List a) -> List a
flatten xs =
  case xs of
    [] -> []
    h :: t -> concat [ h, (reject (\x -> member x h) <| flatten t) ]

generateMaze : Int -> Int -> List (Html Msg)
generateMaze x y =
  let
    spaces = carve x y { x = 1, y = 0 } []
    emptyMaze = repeat y <| repeat x X
  in
    map row <| evaluateMaze emptyMaze spaces

evaluateMaze : List Row -> List Point -> List Row
evaluateMaze maze paths = indexedMap (evaluateRows paths) maze

evaluateRows : List Point -> Int -> Row -> Row
evaluateRows paths y row = indexedMap (evaluateCol paths y) row

evaluateCol : List Point -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

view : Model -> Html Msg
view model = div [ class "maze" ] <| generateMaze 10 10

row : List (Slot) -> Html Msg
row els = div [class "maze__row"] <| map renderSlot els

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
