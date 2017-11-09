module Main exposing (..)

import Html exposing (Html, button, div, label, text)
import Html.Events exposing (onClick)
import Time exposing (Time, minute, second)


main : Program Never Model Msg
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
    , isRunning : Bool
    }


type Msg
    = Tick Time
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( newModel model, Cmd.none )

        Reset ->
            init


init : ( Model, Cmd msg )
init =
    ( Model 0 10 "00" "10" True, Cmd.none )


newModel : Model -> Model
newModel model =
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

        isRunning =
            newMinute + newSecond > 0
    in
    Model newMinute newSecond (padLeftWithZero newMinute) (padLeftWithZero newSecond) isRunning


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRunning then
        Time.every second Tick
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text <| model.minutes ++ ":" ++ model.seconds ]
        , button [ onClick Reset ] [ text "Restart" ]
        ]


padLeftWithZero : Int -> String
padLeftWithZero timeComponent =
    if timeComponent < 10 then
        "0" ++ toString timeComponent
    else
        toString timeComponent
