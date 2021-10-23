module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Form exposing (..)
import Html exposing (Html)


myForm =
    form f4
        (stringToString "hello"
            [ stringMustNotContain "hell"
            , stringMustBeLongerThan 10
            ]
        )
        (stringToFloat "2"
            [ floatMustBeGreaterThan 1
            ]
        )
        (stringToInt "wow"
            [ intMustBeGreaterThan 0
            ]
        )
        (stringToInt "gosh"
            [ intMustBeLessThan 0
            ]
        )
        end


type alias Model =
    State ( String, ( String, ( String, ( String, () ) ) ) )


initialModel : Model
initialModel =
    myForm.init


type Msg
    = Set0 String
    | Set1 String
    | Set2 String
    | Set3 String


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


view : Model -> Html Msg
view model =
    let
        (State ( zero, ( one, ( two, ( three, () ) ) ) )) =
            model

        ( r0, ( r1, ( r2, ( r3, () ) ) ) ) =
            myForm.submit model
    in
    layout [] <|
        column [ padding 30, spacing 10 ]
            [ input "one" zero Set0 r0
            , input "two" one Set1 r1
            , input "three" two Set2 r2
            , input "four" three Set3 r3
            ]


input : String -> String -> (String -> msg) -> Result error value -> Element msg
input l t msg r =
    Input.text
        [ Background.color
            (case r of
                Ok _ ->
                    rgb255 255 255 255

                Err _ ->
                    rgb255 255 100 100
            )
        ]
        { label = Input.labelLeft [ width <| px 50 ] (text l)
        , text = t
        , placeholder = Nothing
        , onChange = msg
        }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
