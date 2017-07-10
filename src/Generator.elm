module Generator exposing (Slot, occupied, generateMaze)

import List exposing (..)
import List.Util exposing (..)
import Random exposing (Seed, step, initialSeed)

type alias Cell = { x : Int, y : Int }
type alias Grid = List Cell

type Slot = X | O
type alias Row = List Slot
type alias Maze = List Row

generateMaze : Int -> Int -> Int -> Maze
generateMaze x y seed =
  let
    prepop = [{ x = x, y = 1 }, { x = 1, y = y }, { x = x, y = y }, { x = x, y = y + 1 }]
    spaces = union prepop <| carve (initialSeed seed) x y { x = 1, y = 0 } []
  in
    emptyMaze x y |> indexedMap (evaluateRows spaces)

emptyMaze : Int -> Int -> Maze
emptyMaze x y = repeat (y + 2) <| repeat (x + 2) X

occupied : Slot -> Bool
occupied slot = case slot of
  X -> True
  _ -> False

evaluateRows : Grid -> Int -> Row -> Row
evaluateRows paths y row = row |> indexedMap (evaluateCol paths y)

evaluateCol : Grid -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

carve : Seed -> Int -> Int -> Cell -> Grid -> Grid
carve seed x y current state =
  let
    next = current :: state
    rec = carve rnd x y
    (shf, rnd) = step (Random.int 1 10) seed
    isValid = validSlot (pointContained x y) state
  in
    if isValid current then
      current
      |> availableSlots isValid next
      |> (shuffle shf)
      |> foldr rec next
    else
      state

availableSlots : (Cell -> Bool) -> Grid -> Cell -> Grid
availableSlots isValid state current =
  current
  |> neighbors
  |> reject (memberOf state)
  |> filter isValid

pointContained : Int -> Int -> Cell -> Bool
pointContained x y pt =
  (pt.x > 0 && pt.x <= x && pt.y > 0 && pt.y <= y)

validSlot : (Cell -> Bool) -> Grid -> Cell -> Bool
validSlot ptCont state pt =
  let
    sec = intersection (neighbors pt) state
    legal = member pt state /= True && (length sec < 2)
  in
    (length state == 0) || ((ptCont pt) && legal)

neighbors : Cell -> List Cell
neighbors pt =
  [ { x = pt.x - 1, y = pt.y }
  , { x = pt.x + 1, y = pt.y }
  , { x = pt.x,     y = pt.y - 1 }
  , { x = pt.x,     y = pt.y + 1 }
  ]
