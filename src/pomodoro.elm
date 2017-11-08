module Main exposing (..)

import Html exposing (Html, button, div, label, text)
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
    { m : Int
    , s : Int
    , minutes : String
    , seconds : String
    }


type Msg
    = Tick Time
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newSecond =
                    if model.s - 1 == -1 then
                        59
                    else
                        model.s - 1

                newMinute =
                    if newSecond == 59 then
                        model.m - 1
                    else
                        model.m
            in
            ( { model | m = newMinute, s = newSecond, minutes = toString newMinute, seconds = toString newSecond }, Cmd.none )

        Reset ->
            init


init : ( Model, Cmd msg )
init =
    ( Model 1 30 "1" "30", Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text <| model.minutes ++ ":" ++ model.seconds ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]
