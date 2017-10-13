import Char exposing (isDigit, isLower, isUpper)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , validate : Bool
    }


model : Model
model =
    Model "" "" "" "" False


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit



--| Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name newName ->
            { model | name = newName }

        Password newPassword ->
            { model | password = newPassword }

        PasswordAgain newPasswordAgain ->
            { model | passwordAgain = newPasswordAgain }

        Age newAge ->
            { model | age = newAge }

        Submit ->
            { model | validate = True }



-- Submit ->
--   viewValidation model
--   model


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "name", onInput Name ] []
        , input [ type_ "text", placeholder "age", onInput Age ] []
        , input [ type_ "password", placeholder "password", onInput Password ] []
        , input [ type_ "password", placeholder "password again", onInput PasswordAgain ] []
        , viewValidation model
        , div [] [ button [ onClick Submit ] [ text "Submit" ] ]
        ]


type ValidationResult
    = OK String
    | PasswordsDontMatch String
    | AgeNotANumber String
    | MinimumLengthNotSatisfied String
    | CharacterVariationNotSatisfied String



-- inputValidation : Model -> ( Model, ValidationResult )
-- inputValidation model =
--     if not <| String.all isDigit model.age then
--         ( { model | isValid = False }, AgeNotANumber "age must be a number" )
--     else if model.password /= model.passwordAgain then
--         ( { model | isValid = False }, PasswordsDontMatch "passwords don't match" )
--     else if String.length model.password < 8 then
--         ( { model | isValid = False }, MinimumLengthNotSatisfied "password must be at least 8 characters" )
--     else if not (String.any isLower model.password && String.any isUpper model.password && String.any isDigit model.password) then
--         ( { model | isValid = False }, CharacterVariationNotSatisfied "password must be contain upper case, lower case, and numeric characters" )
--     else
--         ( { model | isValid = True }, OK "OK" )


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message ) =
            if not <| String.all isDigit model.age then
                ( "red", "age must be a number" )
            else if model.password /= model.passwordAgain then
                ( "red", "passwords don't match" )
            else if String.length model.password < 8 then
                ( "red", "password must be at least 8 characters" )
            else if not (String.any isLower model.password && String.any isUpper model.password && String.any isDigit model.password) then
                ( "red", "password must be contain upper case, lower case, and numeric characters" )
            else
                ( "green", "OK" )
    in
    div [ style [ ( "color", color ) ], hidden <| not model.validate ] [ text message ]
