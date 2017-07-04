module Main exposing (..)

import Generator exposing (Slot, occupied, generateMaze)
import List exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)

type alias Model = { seed : Int }

init : Model -> ( Model, Cmd Msg )
init flags = ( flags, Cmd.none )

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

view : Model -> Html Msg
view model =
  div [ class "maze" ]
  <| (map <| div [class "maze__row"])
  <| (map <| map (\x -> div [class <| gridClass x] []))
  <| generateMaze 12 12 model.seed

gridClass : Slot -> String
gridClass slot =
  if occupied slot then "maze__grid maze__grid--blocked"
  else "maze__grid"

main : Program Model Model Msg
main =
  Html.programWithFlags
    { view = view
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    }
