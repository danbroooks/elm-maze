module Generator exposing (Point, carve)

import List exposing (..)
import List.Util exposing (..)
import Random exposing (Seed, step)

type alias Point = { x : Int, y : Int }

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

neighbors : Point -> List Point
neighbors pt =
  [ { x = pt.x - 1, y = pt.y }
  , { x = pt.x + 1, y = pt.y }
  , { x = pt.x,     y = pt.y - 1 }
  , { x = pt.x,     y = pt.y + 1 }
  ]
