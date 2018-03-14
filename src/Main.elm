module Main exposing (..)

import Generator exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import Task


type alias Model =
    { maze : Maze
    , seed : Int
    , height : Int
    , width : Int
    }


init : ( Model, Cmd Msg )
init =
    initialModel ! [ Task.succeed GenerateMaze |> Task.perform identity ]


initialModel : Model
initialModel =
    { maze = []
    , seed = 20000
    , height = 20
    , width = 20
    }


type Msg
    = NoOp
    | GenerateMaze
    | SetSeed Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateMaze ->
            ( { model | maze = generateMaze model.width model.height (initialSeed model.seed) }, Cmd.none )

        SetSeed seed ->
            { model | seed = seed } ! []

        _ ->
            model ! []


renderSlot : Slot -> Html msg
renderSlot slot =
    div [ class <| gridClass slot ] []


renderRow : List Slot -> Html msg
renderRow =
    List.map renderSlot >> div [ class "maze__row" ]


seedInput : Int -> Html Msg
seedInput seed =
    input [ type_ "number", value (toString seed), onInput setSeed ] []


setSeed : String -> Msg
setSeed val =
    String.toInt val
        |> Result.map SetSeed
        |> Result.withDefault NoOp


view : Model -> Html Msg
view model =
    let
        maze =
            model.maze
                |> List.map renderRow
                |> div [ class "maze" ]
    in
        div []
            [ maze
            , div []
                [ seedInput model.seed
                , button [ onClick GenerateMaze ] [ text "generate" ]
                ]
            ]


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
