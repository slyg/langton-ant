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


type Direction
    = Top
    | Left
    | Bottom
    | Right


type alias Tile =
    { color : Color
    , current : Bool
    }


type alias TileMap =
    Matrix Tile


type alias Model =
    { frame : Int
    , tileMap : TileMap
    , currentLocation : Matrix.Location
    , currentDirection : Direction
    }


initTile : Tile
initTile =
    { color = White
    , current = False
    }


initMatrix : TileMap
initMatrix =
    let
        initialMatrix =
            matrix 10 10 (\location -> initTile)
    in
        Matrix.update
            ( 4, 4 )
            (\tile -> { tile | current = True })
            initialMatrix


init : ( Model, Cmd Msg )
init =
    ( { frame = 0
      , tileMap = initMatrix
      , currentLocation = ( 4, 4 )
      , currentDirection = Top
      }
    , Cmd.none
    )


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | frame = model.frame + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


viewTile : Tile -> Html Msg
viewTile tile =
    let
        emphaseStyle =
            if tile.current then
                ( "borderRadius", "30px 30px" )
            else
                ( "borderRadius", "0px 0px" )

        commonStyle =
            emphaseStyle
                :: [ ( "width", "30px" )
                   , ( "height", "30px" )
                   , ( "padding", "0" )
                   , ( "margin", "1px" )
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

        frame =
            model
                |> .frame
                |> toString
    in
        div []
            [ div [] tiles
            , div [] [ text ("frame " ++ frame) ]
            ]
