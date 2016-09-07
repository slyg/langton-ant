module Main exposing (..)

import Html exposing (Html, div, span, text)
import Html.App as App
import Time exposing (Time, second)
import Matrix exposing (Matrix, matrix)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Color
    = White
    | Black


type alias Tile =
    { color : Color
    , current : Bool
    }


type alias TileMap =
    Matrix Tile


type alias Model =
    { time : Time
    , tileMap : TileMap
    }


initTile : Tile
initTile =
    { color = White
    , current = False
    }


init : ( Model, Cmd Msg )
init =
    ( { time = 0
      , tileMap = matrix 5 5 (\location -> initTile)
      }
    , Cmd.none
    )


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


viewTile : Tile -> Html Msg
viewTile tile =
    case tile.color of
        White ->
            span [] [ text "w" ]

        Black ->
            span [] [ text "b" ]


view : Model -> Html Msg
view model =
    let
        tiles =
            model
                |> .tileMap
                |> Matrix.flatten
                |> List.map (\tile -> viewTile tile)
    in
        div [] tiles
