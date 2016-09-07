module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.App as App
import Time exposing (Time, millisecond)
import AnimationFrame
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
    matrix 100 100 (\location -> initTile)


updateMatrix : TileMap -> Matrix.Location -> Color -> TileMap
updateMatrix matrix location color =
    matrix
        |> Matrix.map (\tile -> { tile | current = False })
        |> Matrix.update location (\tile -> { tile | current = True, color = color })


initLocation : Matrix.Location
initLocation =
    ( 49, 49 )


init : ( Model, Cmd Msg )
init =
    ( { frame = 0
      , tileMap = initMatrix
      , currentLocation = initLocation
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
            let
                currentXLocation =
                    Matrix.row model.currentLocation

                currentYLocation =
                    Matrix.col model.currentLocation

                currentColor =
                    let
                        currentColor =
                            Matrix.get model.currentLocation model.tileMap
                    in
                        case currentColor of
                            Just tile ->
                                case tile.color of
                                    Black ->
                                        White

                                    White ->
                                        Black

                            Nothing ->
                                White

                newXLocation =
                    case model.currentDirection of
                        Right ->
                            currentXLocation + 1

                        Left ->
                            currentXLocation - 1

                        _ ->
                            currentXLocation

                newYLocation =
                    case model.currentDirection of
                        Top ->
                            currentYLocation + 1

                        Bottom ->
                            currentYLocation - 1

                        _ ->
                            currentYLocation

                newLocation =
                    ( newXLocation
                    , newYLocation
                    )

                newColor =
                    let
                        currentColor =
                            Matrix.get newLocation model.tileMap
                    in
                        case currentColor of
                            Just tile ->
                                case tile.color of
                                    Black ->
                                        White

                                    White ->
                                        Black

                            Nothing ->
                                White

                newDirection =
                    case newColor of
                        White ->
                            case model.currentDirection of
                                Top ->
                                    Right

                                Right ->
                                    Bottom

                                Bottom ->
                                    Left

                                Left ->
                                    Top

                        Black ->
                            case model.currentDirection of
                                Top ->
                                    Left

                                Left ->
                                    Bottom

                                Bottom ->
                                    Right

                                Right ->
                                    Top
            in
                ( { model
                    | currentLocation = newLocation
                    , tileMap = updateMatrix model.tileMap newLocation newColor
                    , currentDirection = newDirection
                    , frame = model.frame + 1
                  }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick


viewTile : Tile -> Html Msg
viewTile tile =
    let
        emphaseStyle =
            if tile.current == True then
                ( "backgroundColor", "red" )
            else
                case tile.color of
                    White ->
                        ( "backgroundColor", "whitesmoke" )

                    Black ->
                        ( "backgroundColor", "black" )

        tileStyle =
            emphaseStyle
                :: [ ( "width", "5px" )
                   , ( "height", "5px" )
                   , ( "padding", "0" )
                   , ( "margin", "1px" )
                   , ( "display", "inline-block" )
                   ]
    in
        div [ style tileStyle ] []


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
            toString model.frame

        layoutStyle =
            [ ( "padding", "20px" ) ]

        tilesMapStyle =
            [ ( "width", "700px" )
            , ( "margin", "0 auto" )
            ]

        textStyle =
            [ ( "fontFamily", "Arial" )
            , ( "fontSize", "80%" )
            , ( "padding", "10px 0" )
            , ( "textAlign", "center" )
            ]
    in
        div [ style layoutStyle ]
            [ div [ style tilesMapStyle ] tiles
            , div [ style textStyle ] [ text ("frame " ++ frame) ]
            ]
