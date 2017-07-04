module Main exposing (..)

import Generator exposing (Slot, occupied, generateMaze)
import List exposing (..)
import List.Util exposing (..)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)

type alias Model = { seed : Int }

init : Model -> ( Model, Cmd Msg )
init flags = ( flags, Cmd.none )

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

view : Model -> Html Msg
view model = div [ class "maze" ] <| map row <| generateMaze 20 20 model.seed

row : List Slot -> Html Msg
row els = div [class "maze__row"] <| map renderSlot els

renderSlot : Slot -> Html Msg
renderSlot slot = case occupied slot of
  True -> div [class "maze__grid maze__grid--blocked"] []
  _ -> div [class "maze__grid"] []

main : Program Model Model Msg
main =
  Html.programWithFlags
    { view = view
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    }
