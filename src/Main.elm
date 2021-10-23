module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Field exposing (..)
import Form exposing (..)
import Html exposing (Html)


myForm =
    form f4
        (field
            (stringToString "hello"
                [ stringMustNotContain "hell"
                , stringMustBeLongerThan 10
                ]
            )
        )
        (field
            (stringToFloat "2"
                [ floatMustBeGreaterThan 1
                ]
            )
        )
        (field
            (stringToInt "wow"
                [ intMustBeGreaterThan 0
                ]
            )
        )
        (field
            (stringToInt "gosh"
                [ intMustBeLessThan 0
                ]
            )
        )
        end


type alias Model =
    State
        ( { data : String, touched : Bool }
        , ( { data : String, touched : Bool }
          , ( { data : String, touched : Bool }
            , ( { data : String, touched : Bool }, () )
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
            let
                ( newModel, _ ) =
                    myForm.submit model
            in
            newModel


view : Model -> Html Msg
view model =
    let
        ( s0, ( s1, ( s2, ( s3, () ) ) ) ) =
            myForm.state model

        ( _, results ) =
            myForm.submit model

        ( r0, ( r1, ( r2, ( r3, () ) ) ) ) =
            results
    in
    layout [] <|
        column [ padding 30, spacing 10 ]
            [ input "one" s0 Set0 r0
            , input "two" s1 Set1 r1
            , input "three" s2 Set2 r2
            , input "four" s3 Set3 r3
            , Input.button [ padding 10, Border.width 1 ]
                { onPress = Just SubmitClicked
                , label = text "Submit"
                }
            ]


input : String -> { data : String, touched : Bool } -> (String -> msg) -> Result error value -> Element msg
input l { touched, data } msg r =
    Input.text
        [ Background.color
            (case ( touched, r ) of
                ( True, Ok _ ) ->
                    rgb255 100 255 100

                ( True, Err _ ) ->
                    rgb255 255 100 100

                _ ->
                    rgb255 255 255 255
            )
        ]
        { label = Input.labelLeft [ width <| px 50 ] (text l)
        , text = data
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
