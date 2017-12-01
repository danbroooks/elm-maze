module Main exposing (..)

import Generator exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (..)


type alias Model =
    { maze : Maze }


init : ( Model, Cmd Msg )
init =
    ( { maze = [] }, Random.generate (initialSeed >> GenerateMaze 20 20) (Random.int 0 20000) )


type Msg
    = GenerateMaze Int Int Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateMaze w h seed ->
            ( { model | maze = generateMaze w h seed }, Cmd.none )


renderSlot : Slot -> Html msg
renderSlot slot =
    div [ class (gridClass slot) ] []


renderRow : List Slot -> Html msg
renderRow row =
    List.map renderSlot row
        |> div [ class "maze__row" ]


view : Model -> Html msg
view model =
    model.maze
        |> List.map renderRow
        |> div [ class "maze" ]


gridClass : Slot -> String
gridClass slot =
    if occupied slot then
        "maze__grid maze__grid--blocked"
    else
        "maze__grid"


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
