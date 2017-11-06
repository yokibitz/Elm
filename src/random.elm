module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dieFace1 : Int
    , dieFace2 : Int
    }


type Msg
    = Roll
    | NewFace (Int, Int)


imageDict : Dict Int String
imageDict =
    Dict.fromList
        [ ( 1, "one.png" )
        , ( 2, "two.png" )
        , ( 3, "three.png" )
        , ( 4, "four.png" )
        , ( 5, "five.png" )
        , ( 6, "six.png" )
        ]

dieGenerator : Random.Generator Int
dieGenerator =
    Random.int 1 6

diePairGenerator : Random.Generator (Int,Int)
diePairGenerator = 
    Random.pair dieGenerator dieGenerator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace diePairGenerator)

        NewFace (newFace1, newFace2) ->
            ( Model newFace1 newFace2, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.dieFace1) ]
        , img [ src <| "images\\" ++ (Maybe.withDefault "" <| Dict.get model.dieFace1 imageDict) ] []
        , h1 [] [ text (toString model.dieFace2) ]
        , img [ src <| "images\\" ++ (Maybe.withDefault "" <| Dict.get model.dieFace2 imageDict) ] []
        , div []
            [ button [ onClick Roll ] [ text "Roll" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Model 1 1, Cmd.none )
