module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.App as App
import Time exposing (Time)
import AnimationFrame
import Matrix exposing (Matrix, matrix)


-- Types


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
    , isRunning : Bool
    }


type Msg
    = Tick Time
    | Pause



-- Inits


initTile : Tile
initTile =
    { color = White
    , current = False
    }


initMatrix : TileMap
initMatrix =
    matrix 70 60 (\location -> initTile)


initLocation : Matrix.Location
initLocation =
    ( 36, 29 )


init : ( Model, Cmd Msg )
init =
    ( { frame = 0
      , tileMap = initMatrix
      , currentLocation = initLocation
      , currentDirection = Top
      , isRunning = True
      }
    , Cmd.none
    )



-- Updates


updateMatrix : TileMap -> Matrix.Location -> Color -> TileMap
updateMatrix matrix location color =
    matrix
        |> Matrix.map (\tile -> { tile | current = False })
        |> Matrix.update location (\tile -> { tile | current = True, color = color })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pause ->
            ( { model
                | isRunning =
                    if model.isRunning then
                        False
                    else
                        True
              }
            , Cmd.none
            )

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
    if model.isRunning then
        AnimationFrame.diffs Tick
    else
        Sub.none



-- Views


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

        tilesMapWidth =
            (Matrix.rowCount model.tileMap) * 6

        tilesMapStyle =
            [ ( "width", (toString tilesMapWidth) ++ "px" )
            , ( "margin", "0 auto" )
            ]

        textStyle =
            [ ( "fontFamily", "Arial" )
            , ( "fontSize", "80%" )
            , ( "padding", "10px 0" )
            , ( "textAlign", "center" )
            ]

        playPauseText =
            if model.isRunning then
                "Pause"
            else
                "Play"
    in
        div [ style layoutStyle ]
            [ div [ style tilesMapStyle ] tiles
            , div [ style textStyle ]
                [ text ("frame " ++ frame ++ "  ")
                , button [ onClick Pause ] [ text playPauseText ]
                ]
            ]



-- Connect everything


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
