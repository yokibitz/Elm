import Html exposing (Html, div, button, input, text)
import Html.Events exposing (..)
import WebSocket

--uncomment if needed -- import Html.App as App

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Model = 
  {
    input : String,
    messages : List String
  }

init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)

type Msg = 
  Input String
  | Send
  | NewMessage String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newString ->
          ( {model | input = newString}, Cmd.none )

        Send ->
          ( model, WebSocket.send "ws://echo.websocket.org" model.input)

        NewMessage str ->
          ( {model | messages = str :: model.messages}, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://echo.websocket.org" NewMessage

view : Model -> Html Msg
view model =
    div []
    [ div [] (List.map viewMessage model.messages)
      , input [onInput Input] []
      , button [onClick Send] [text "Send"]
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [text msg]
            
