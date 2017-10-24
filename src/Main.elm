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


view : Model -> Html Msg
view model =
    model.maze
        |> List.map
            (div [ class "maze__row" ] << List.map (\x -> div [ class <| gridClass x ] []))
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
