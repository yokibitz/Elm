import Html exposing (Html, Attribute, div, text, button, h2, img, br, input, select, option)
import Http
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode as Decode
  
main = 
  Html.program
  {
    init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions         
  }

type alias Model =
  {
    topic : String,
    gifUrl : String,
    errorMessage : String
  }

type Msg = 
  MorePlease
  | NewTopic String
  | NewGif (Result Http.Error String)

init : String -> (Model, Cmd Msg)
init topic =
  (Model topic "waiting.gif" "", getRandomGif topic)

view : Model -> Html Msg
view model =
    div []
    [
      h2 [] [text model.topic]
      , select [onChange NewTopic, selected True] 
      [
        option [] [text "cats"] 
        , option [] [text "dogs"] 
        , option [] [text "cakes"] 
      ]
      , button [onClick MorePlease] [text "More please!!!"]
      , br [] []
      , img [src model.gifUrl] []
      , div [style [("color", "red")]] [text model.errorMessage]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( {model | errorMessage = ""}, getRandomGif model.topic )

        NewTopic newTopic ->
            ( {model | topic = newTopic}, Cmd.none )    
        
        NewGif (Ok value) ->
            ( {model | gifUrl = value, errorMessage = ""}, Cmd.none )
            
        NewGif (Err err) ->
            ( {model | errorMessage = toString err} , Cmd.none )

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url = 
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    request = 
      Http.get url decodeGifUrl
  in
    Http.send NewGif request

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at["data", "image_url"] Decode.string  

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Decode.map tagger targetValue)