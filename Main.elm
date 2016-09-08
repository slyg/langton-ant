module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Html.App as App
import Time exposing (Time)
import AnimationFrame
import Matrix exposing (Matrix)


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


type alias TilesMatrix =
    Matrix Tile


type alias Model =
    { currentLocation : Matrix.Location
    , currentDirection : Direction
    , frame : Int
    , hasReachedEdges : Bool
    , isRunning : Bool
    , tilesMatrix : TilesMatrix
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


initMatrix : TilesMatrix
initMatrix =
    Matrix.square 70 (\location -> initTile)


initLocation : Matrix.Location
initLocation =
    ( 36, 36 )


init : ( Model, Cmd Msg )
init =
    ( { currentLocation = initLocation
      , currentDirection = Top
      , frame = 0
      , hasReachedEdges = False
      , isRunning = True
      , tilesMatrix = initMatrix
      }
    , Cmd.none
    )



-- Updates


getTileColor : Matrix.Location -> TilesMatrix -> Color
getTileColor location tilesMatrix =
    let
        c =
            Matrix.get location tilesMatrix
    in
        case c of
            Just tile ->
                case tile.color of
                    Black ->
                        White

                    White ->
                        Black

            Nothing ->
                White


updateMatrix : Matrix.Location -> Color -> TilesMatrix -> TilesMatrix
updateMatrix location color tilesMatrix =
    tilesMatrix
        |> Matrix.map (\tile -> { tile | current = False })
        |> Matrix.update location (\tile -> { tile | current = True, color = color })


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        Top ->
            Left

        Left ->
            Bottom

        Bottom ->
            Right

        Right ->
            Top


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        Top ->
            Right

        Right ->
            Bottom

        Bottom ->
            Left

        Left ->
            Top


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pause ->
            let
                isRunning =
                    if model.isRunning then
                        False
                    else
                        True
            in
                ( { model | isRunning = isRunning }
                , Cmd.none
                )

        Tick _ ->
            let
                ( currentXLocation, currentYLocation ) =
                    model.currentLocation

                currentColor =
                    getTileColor model.currentLocation model.tilesMatrix

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
                    getTileColor newLocation model.tilesMatrix

                newDirection =
                    case newColor of
                        White ->
                            turnRight model.currentDirection

                        Black ->
                            turnLeft model.currentDirection

                hasReachedEdges =
                    if newXLocation >= (Matrix.rowCount model.tilesMatrix) then
                        True
                    else if newXLocation < 1 then
                        True
                    else if newYLocation >= (Matrix.colCount model.tilesMatrix) then
                        True
                    else if newYLocation < 1 then
                        True
                    else
                        False
            in
                ( { model
                    | currentDirection = newDirection
                    , currentLocation = newLocation
                    , frame = model.frame + 1
                    , hasReachedEdges = hasReachedEdges
                    , tilesMatrix = updateMatrix newLocation newColor model.tilesMatrix
                  }
                , Cmd.none
                )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.hasReachedEdges then
        Sub.none
    else if model.isRunning then
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
                   , ( "margin", "1px 0 0 1px" )
                   , ( "display", "inline-block" )
                   ]
    in
        div [ style tileStyle ] []


viewTilesRow : List Tile -> Html Msg
viewTilesRow tilesRow =
    let
        lazyViewTile =
            lazy viewTile
    in
        div [ style [ ( "lineHeight", "0" ) ] ] (List.map lazyViewTile tilesRow)


viewPausePlayButton : Bool -> Html Msg
viewPausePlayButton isRunning =
    let
        label =
            if isRunning then
                "Pause"
            else
                "Play"
    in
        button [ onClick Pause ] [ text label ]


view : Model -> Html Msg
view model =
    let
        tiles =
            model
                |> .tilesMatrix
                |> Matrix.toList
                |> List.map (\tileRow -> viewTilesRow tileRow)

        frame =
            toString model.frame

        layoutStyle =
            [ ( "padding", "20px" ) ]

        tilesMatrixWidth =
            (Matrix.colCount model.tilesMatrix) * 6

        tilesMatrixStyle =
            [ ( "width", (toString tilesMatrixWidth) ++ "px" )
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
            [ div [ style tilesMatrixStyle ] tiles
            , div [ style textStyle ]
                [ text ("frame " ++ frame ++ "  ")
                , lazy viewPausePlayButton model.isRunning
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
