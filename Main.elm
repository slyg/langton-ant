module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
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
      , tileMap = matrix 10 10 (\location -> initTile)
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
    let
        commonStyle =
            [ ( "width", "30px" )
            , ( "height", "30px" )
            , ( "padding", "0" )
            , ( "margin", "0" )
            , ( "display", "inline-block" )
            ]
    in
        case tile.color of
            White ->
                div [ style (( "backgroundColor", "lightgrey" ) :: commonStyle) ] []

            Black ->
                div [ style (( "backgroundColor", "black" ) :: commonStyle) ] []


viewTilesRow : List Tile -> Html Msg
viewTilesRow tilesRow =
    div [ style [ ( "lineHeight", "0" ) ] ] (List.map (\tile -> viewTile tile) tilesRow)


view : Model -> Html Msg
view model =
    let
        tiles =
            model
                |> .tileMap
                |> Matrix.toList
                |> List.map (\tileRow -> viewTilesRow tileRow)
    in
        div [] tiles
