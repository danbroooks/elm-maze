module Main exposing (..)

import List exposing (map)
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)

type alias Model = {}

init : ( Model, Cmd Msg )
init = ( {}, Cmd.none )

type Msg = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

type Slot = X | O

renderSlot : Slot -> Html Msg
renderSlot slot = case slot of
    X -> div [class "maze__grid maze__grid--blocked"] []
    O -> div [class "maze__grid"] []

row : List (Slot) -> Html Msg
row els = div [class "maze__row"] (map renderSlot els)

view : Model -> Html Msg
view model =
    div [ class "maze" ]
        [ row [ X, O, X, X, X ]
        , row [ X, O, O, O, X ]
        , row [ X, O, X, O, X ]
        , row [ X, O, X, O, X ]
        , row [ X, X, X, O, X ]
        ]

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
