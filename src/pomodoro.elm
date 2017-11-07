module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time exposing (Time, minute, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { remainingTime : Time
    , isRunning : Bool
    }


type Msg
    = Tick Time
    | Reset
    | Toggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newTime =
                    model.remainingTime - (1 * second)
            in
            if newTime <= 0 then
                ( { model | remainingTime = 0, isRunning = False }, Cmd.none )
            else
                ( { model | remainingTime = newTime }, Cmd.none )

        Reset ->
            init

        Toggle ->
            ( { model | isRunning = not model.isRunning }, Cmd.none )


init : ( Model, Cmd msg )
init =
    ( Model (25 * minute) True, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRunning then
        Time.every second Tick
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString <| Time.inMinutes model.remainingTime) ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]
