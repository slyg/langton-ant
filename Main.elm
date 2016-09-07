module Main exposing (..)

import Html exposing (Html, text)
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
    { color : Color }


type alias TileMap =
    Matrix Tile


type alias Model =
    { time : Time
    , tileMap : TileMap
    }


init : ( Model, Cmd Msg )
init =
    ( { time = 0
      , tileMap = matrix 5 5 (\location -> { color = White })
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


view : Model -> Html Msg
view model =
    model
        |> .time
        |> toString
        |> text
