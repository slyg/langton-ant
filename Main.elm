module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Html.App as App
import Time exposing (Time)
import AnimationFrame
import Matrix exposing (Matrix)


-- Fancy tuples


(~>) : a -> b -> ( a, b )
(~>) a b =
    ( a, b )



-- Types


type Direction
    = North
    | West
    | South
    | East


type Color
    = White
    | Black


type alias TilesMatrix =
    Matrix Color


type alias Model =
    { direction : Direction
    , fps : Int
    , frame : Int
    , hasReachedEdges : Bool
    , isRunning : Bool
    , location : Matrix.Location
    , tilesMatrix : TilesMatrix
    }


type Msg
    = Tick Time



-- Inits


initMatrix : TilesMatrix
initMatrix =
    Matrix.matrix 66 50 (\location -> White)


initLocation : Matrix.Location
initLocation =
    ( 30, 24 )


init : ( Model, Cmd Msg )
init =
    ( { direction = North
      , fps = 0
      , frame = 0
      , hasReachedEdges = False
      , isRunning = True
      , location = initLocation
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
            Just color ->
                case color of
                    Black ->
                        White

                    White ->
                        Black

            Nothing ->
                White


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


getNextLocation : Matrix.Location -> Direction -> Matrix.Location
getNextLocation location direction =
    let
        ( x, y ) =
            location

        nextX =
            case direction of
                East ->
                    x + 1

                West ->
                    x - 1

                _ ->
                    x

        nextY =
            case direction of
                North ->
                    y + 1

                South ->
                    y - 1

                _ ->
                    y
    in
        ( nextX
        , nextY
        )


hasReachedEdged : Matrix.Location -> TilesMatrix -> Bool
hasReachedEdged location tilesMatrix =
    let
        ( x, y ) =
            location
    in
        if x >= (Matrix.rowCount tilesMatrix) then
            True
        else if x < 1 then
            True
        else if y >= (Matrix.colCount tilesMatrix) then
            True
        else if y < 1 then
            True
        else
            False


computeNextTilesMatrix : Matrix.Location -> Matrix.Location -> Color -> TilesMatrix -> TilesMatrix
computeNextTilesMatrix location nextLocation color tilesMatrix =
    Matrix.set nextLocation color tilesMatrix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                nextLocation =
                    getNextLocation model.location model.direction

                nextColor =
                    getTileColor nextLocation model.tilesMatrix

                nextDirection =
                    case nextColor of
                        White ->
                            turnRight model.direction

                        Black ->
                            turnLeft model.direction

                nextTilesMatrix =
                    computeNextTilesMatrix model.location nextLocation nextColor model.tilesMatrix
            in
                ( { model
                    | direction = nextDirection
                    , fps = round (1000 / dt)
                    , frame = model.frame + 1
                    , hasReachedEdges = hasReachedEdged model.location model.tilesMatrix
                    , location = nextLocation
                    , tilesMatrix = nextTilesMatrix
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


viewTile : Color -> Html Msg
viewTile color =
    let
        emphaseStyle =
            case color of
                White ->
                    ("backgroundColor" ~> "whitesmoke")

                Black ->
                    ("backgroundColor" ~> "black")

        tileStyle =
            emphaseStyle
                :: [ "width" ~> "5px"
                   , "height" ~> "5px"
                   , "padding" ~> "0"
                   , "margin" ~> "1px 0 0 1px"
                   , "display" ~> "inline-block"
                   ]
    in
        div [ style tileStyle ] []


viewTilesRow : List Color -> Html Msg
viewTilesRow tilesRow =
    div [ style [ "lineHeight" ~> "0" ] ] (List.map (lazy viewTile) tilesRow)


view : Model -> Html Msg
view model =
    let
        tiles =
            model
                |> .tilesMatrix
                |> Matrix.toList
                |> List.map (\tileRow -> viewTilesRow tileRow)

        fps =
            toString model.fps

        frame =
            toString model.frame

        layoutStyle =
            [ "padding" ~> "20px" ]

        tilesMatrixWidth =
            (Matrix.colCount model.tilesMatrix) * 6

        tilesMatrixStyle =
            [ "width" ~> ((toString tilesMatrixWidth) ++ "px")
            , "margin" ~> "0 auto"
            ]

        textStyle =
            [ "fontFamily" ~> "Arial"
            , "fontSize" ~> "80%"
            , "padding" ~> "10px 0"
            , "textAlign" ~> "center"
            ]
    in
        div [ style layoutStyle ]
            [ div [ style tilesMatrixStyle ] tiles
            , div [ style textStyle ]
                [ text (fps ++ " fps")
                , text " | "
                , text ("frame " ++ frame)
                ]
            ]



-- Wire everything


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
