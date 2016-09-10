module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Html.App as App
import Time exposing (Time)
import AnimationFrame
import Matrix exposing (Matrix)
import Collage
import Element
import Color


-- Fancy tuples


(~>) : a -> b -> ( a, b )
(~>) a b =
    ( a, b )



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
    , isCurrent : Bool
    }


type alias TilesMatrix =
    Matrix Tile


type alias Model =
    { direction : Direction
    , frame : Int
    , hasReachedEdges : Bool
    , isRunning : Bool
    , location : Matrix.Location
    , tilesMatrix : TilesMatrix
    }


type Msg
    = Tick Time
    | Pause



-- Inits


initTile : Tile
initTile =
    { color = White
    , isCurrent = False
    }


initMatrix : TilesMatrix
initMatrix =
    Matrix.matrix 66 50 (\location -> initTile)


initLocation : Matrix.Location
initLocation =
    ( 30, 24 )


init : ( Model, Cmd Msg )
init =
    ( { direction = Top
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
            Just tile ->
                case tile.color of
                    Black ->
                        White

                    White ->
                        Black

            Nothing ->
                White


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


getNextLocation : Matrix.Location -> Direction -> Matrix.Location
getNextLocation location direction =
    let
        ( x, y ) =
            location

        nextX =
            case direction of
                Right ->
                    x + 1

                Left ->
                    x - 1

                _ ->
                    x

        nextY =
            case direction of
                Top ->
                    y + 1

                Bottom ->
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
    tilesMatrix
        |> Matrix.update location (\tile -> { tile | isCurrent = False })
        |> Matrix.update nextLocation (\tile -> { tile | isCurrent = True, color = color })


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
        {-
           Intensive mode, might affect browser perfs
           use AnimationFrame.diffs Tick for respectful animations
        -}
        Time.every Time.millisecond Tick
    else
        Sub.none



-- Views


viewTile : Tile -> Html Msg
viewTile tile =
    let
        emphaseStyle =
            if tile.isCurrent == True then
                "backgroundColor" ~> "red"
            else
                case tile.color of
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


viewTileCanvas : Matrix.Location -> Tile -> Collage.Form
viewTileCanvas position tile =
    let
        ( x, y ) =
            position

        color =
            if tile.isCurrent == True then
                Color.rgb 255 0 0
            else
                case tile.color of
                    White ->
                        Color.rgb 222 222 222

                    Black ->
                        Color.rgb 0 0 0
    in
        Collage.rect 5 5
            |> Collage.filled color
            |> Collage.move ( toFloat ((6 * x) - 159), toFloat ((6 * y) - 147) )


viewTilesRow : List Tile -> Html Msg
viewTilesRow tilesRow =
    let
        lazyViewTile =
            lazy viewTile
    in
        div [ style [ "lineHeight" ~> "0" ] ] (List.map lazyViewTile tilesRow)


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
            Collage.collage tilesMatrixWidth
                tilesMatrixWidth
                (model
                    |> .tilesMatrix
                    |> Matrix.mapWithLocation viewTileCanvas
                    |> Matrix.flatten
                )
                |> Element.color (Color.rgb 240 240 240)
                |> Element.toHtml

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

        tilesMatrixView =
            div [ style tilesMatrixStyle ] [ tiles ]
    in
        div [ style layoutStyle ]
            [ tilesMatrixView
            , div [ style textStyle ]
                [ text ("frame " ++ frame ++ "  ")
                , lazy viewPausePlayButton model.isRunning
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
