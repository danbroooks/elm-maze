module Generator exposing (Slot, occupied, generateMaze)

import List exposing (..)
import List.Util exposing (..)
import Random exposing (Seed, step, initialSeed)

type alias Point = { x : Int, y : Int }

type Slot = X | O
type alias Row = List Slot
type alias Maze = List Row

generateMaze : Int -> Int -> Int -> Maze
generateMaze x y seed =
  let
    prepop = [{ x = x, y = 1 }, { x = 1, y = y }, { x = x, y = y }, { x = x, y = y + 1 }]
    spaces = flatten [prepop, carve (initialSeed seed) x y { x = 1, y = 0 } []]
    emptyMaze = repeat (y + 2) <| repeat (x + 2) X
  in
    indexedMap (evaluateRows spaces) emptyMaze

occupied : Slot -> Bool
occupied slot = case slot of
  X -> True
  _ -> False

evaluateRows : List Point -> Int -> Row -> Row
evaluateRows paths y row = row |> indexedMap (evaluateCol paths y)

evaluateCol : List Point -> Int -> Int -> Slot -> Slot
evaluateCol paths y x slot =
  if member { x = x, y = y } paths then O else slot

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
      |> availableSlots isValid next
      |> (shuffle shf)
      |> foldr rec next
    else
      state

availableSlots : (Point -> Bool) -> List Point -> Point -> List Point
availableSlots isValid state current =
  current
  |> neighbors
  |> reject (memberOf state)
  |> filter isValid

validSlot : Int -> Int -> List Point -> Point -> Bool
validSlot x y state pt =
  let
    inner = (pt.x > 0 && pt.x <= x && pt.y > 0 && pt.y <= y)
    sec = intersection (neighbors pt) state
    legal = member pt state /= True && (length sec < 2)
  in
    (length state == 0) || (inner && legal)

neighbors : Point -> List Point
neighbors pt =
  [ { x = pt.x - 1, y = pt.y }
  , { x = pt.x + 1, y = pt.y }
  , { x = pt.x,     y = pt.y - 1 }
  , { x = pt.x,     y = pt.y + 1 }
  ]
