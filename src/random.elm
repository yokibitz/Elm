module Main exposing (..)

import Html exposing (Html, button, div, h1, text, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Dict exposing (Dict)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dieFace : Int
    }


type Msg
    = Roll
    | NewFace Int

imageDict : Dict Int String
imageDict = 
  Dict.fromList 
    [
      (1, "one.png")
      , (2, "two.png")
      , (3, "three.png")
      , (4, "four.png")
      , (5, "five.png")
      , (6, "six.png")
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace newFace ->
            ( Model newFace , Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.dieFace) ]
        -- , img [src ] 
        , button [ onClick Roll ] [ text "Roll" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Model 1, Cmd.none )
