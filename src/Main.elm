module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Field exposing (..)
import Form exposing (..)
import Html exposing (Html)


myForm :
    { init : State ( FieldState String, ( FieldState String, ( FieldState String, ( FieldState String, () ) ) ) )
    , state : State state -> state
    , submit :
        State ( FieldState String, ( FieldState String, ( FieldState String, ( FieldState String, () ) ) ) )
        ->
            Result
                (State ( FieldState String, ( FieldState String, ( FieldState String, ( FieldState String, () ) ) ) ))
                ( String, ( Float, ( Int, ( Int, () ) ) ) )
    , set : ((( FieldState input, rest ) -> ( FieldState input, rest )) -> e -> f) -> input -> State e -> State f
    , toEls : State ( FieldState String, ( FieldState String, ( FieldState String, ( FieldState String, () ) ) ) ) -> ( Element Msg, ( Element Msg, ( Element Msg, ( Element Msg, () ) ) ) )
    }
myForm =
    form f4
        (field
            (string "hello"
                [ stringMustNotContain "hell"
                , stringMustBeLongerThan 10
                ]
                Set0
            )
        )
        (field
            (float "2"
                [ floatMustBeGreaterThan 1 ]
                Set1
            )
        )
        (field
            (int "wow"
                [ intMustBeGreaterThan 0 ]
                Set2
            )
        )
        (field
            (int "gosh"
                [ intMustBeLessThan 0 ]
                Set3
            )
        )
        end


type alias Model =
    State
        ( FieldState String
        , ( FieldState String
          , ( FieldState String
            , ( FieldState String
              , ()
              )
            )
          )
        )


initialModel : Model
initialModel =
    myForm.init


type Msg
    = Set0 String
    | Set1 String
    | Set2 String
    | Set3 String
    | SubmitClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        Set0 s ->
            myForm.set i0 s model

        Set1 s ->
            myForm.set i1 s model

        Set2 s ->
            myForm.set i2 s model

        Set3 s ->
            myForm.set i3 s model

        SubmitClicked ->
            case myForm.submit model of
                Ok _ ->
                    model

                Err newModel ->
                    newModel


view : Model -> Html Msg
view model =
    let
        ( s0, ( s1, ( s2, ( s3, () ) ) ) ) =
            myForm.toEls model
    in
    layout [] <|
        column [ padding 30, spacing 10 ]
            [ s0
            , s1
            , s2
            , s3
            , Input.button [ padding 10, Border.width 1 ]
                { onPress = Just SubmitClicked
                , label = text "Submit"
                }
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
