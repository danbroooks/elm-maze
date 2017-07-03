module Main exposing (..)

import List exposing (..)
import Random exposing (Seed, step, initialSeed)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)

type alias Model = { seed : Int }

init : Model -> ( Model, Cmd Msg )
init flags = ( flags, Cmd.none )

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

type Slot = X | O
type alias Row = List Slot
type alias Point = { x : Int, y : Int }
type alias Maze = List Row

carve : Seed -> Int -> Int -> Point -> List Point -> List Point
carve seed x y current state =
  let
    next = flatten [[current], state]
    rec = carve rnd x y
    (shf, rnd) = step (Random.int 1 10) seed
    isValid = validSlot x y state
  in
    if isValid current then
      current
      |> availableSlots x y next
      |> (shuffle shf)
      |> foldr rec next
    else
      state

shuffle : Int -> List a -> List a
shuffle n xs =
  if n < 1 then xs
  else
    case shuffle (n - 1) xs of
      [] -> []
      (h :: t) -> reverse (h :: (reverse t))

availableSlots : Int -> Int -> List Point -> Point -> List Point
availableSlots x y state current =
  let
    isValid = validSlot x y state
  in
    current
    |> neighbors
    |> reject (memberOf state)
    |> filter isValid

validSlot : Int -> Int -> List Point -> Point -> Bool
validSlot x y state pt =
  let
    inner = (pt.x > 0 && pt.x <= x && pt.y > 0 && pt.y <= y)
    sec = intersection (neighbors pt) state
    legal = member pt state /= True
  in
    (length state == 0) || (length sec < 2) && (inner && legal)

memberOf : List a -> a -> Bool
memberOf xs x = member x xs

reject : (a -> Bool) -> List a -> List a
reject f xs = filter (\x -> f x == False) xs

neighbors : Point -> List Point
neighbors pt =
  [ { x = pt.x - 1, y = pt.y }
  , { x = pt.x + 1, y = pt.y }
  , { x = pt.x,     y = pt.y - 1 }
  , { x = pt.x,     y = pt.y + 1 }
  ]

intersection : List Point -> List Point -> List Point
intersection a b = filter (memberOf b) a

flatten : List (List a) -> List a
flatten xs =
  case xs of
    [] -> []
    h :: t -> concat [ h, (reject (\x -> member x h) <| flatten t) ]

generateMaze : Int -> Int -> Int -> List (Html Msg)
generateMaze x y seed =
  let
    spaces = carve (initialSeed seed) x y { x = 1, y = 1 } []
    emptyMaze = repeat (y + 2) <| repeat (x + 2) X
  in
    map row <| evaluateMaze emptyMaze <| { x = 1, y = 0 } :: { x = x, y = y + 1 } :: spaces

evaluateMaze : List Row -> List Point -> List Row
evaluateMaze maze paths = indexedMap (evaluateRows paths) maze

evaluateRows : List Point -> Int -> Row -> Row
evaluateRows paths y row = indexedMap (evaluateCol paths y) row

evaluateCol : List Point -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

view : Model -> Html Msg
view model = div [ class "maze" ] <| generateMaze 20 20 model.seed

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
