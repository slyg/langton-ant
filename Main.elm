module Main exposing (..)

import Html exposing (Html, text)
import Html.App as App
import Time exposing (Time, second)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { time : Time
    }


init : ( Model, Cmd Msg )
init =
    ( { time = 0 }, Cmd.none )


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
