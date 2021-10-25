module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Field
import Form
import Html exposing (Html)


myForm :
    { init : Form.State ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) )
    , submit : Form.State ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) ) -> Result (Form.State ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) )) ( Int, ( Float, ( Int, ( Int, () ) ) ) )
    , update :
        ((( Field.Field input delta output msg, restOfForm )
          -> ( Field.State input, restOfState )
          -> ( Field.State input, restOfState )
         )
         -> ( Field.Field Int Int Int Msg, ( Field.Field String String Float Msg, ( Field.Field String String Int Msg, ( Field.Field String String Int Msg, () ) ) ) )
         -> ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) )
         -> ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) )
        )
        -> delta
        -> Form.State ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) )
        -> Form.State ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) )
    , toEls : Form.State ( Field.State Int, ( Field.State String, ( Field.State String, ( Field.State String, () ) ) ) ) -> ( Element Msg, ( Element Msg, ( Element Msg, ( Element Msg, () ) ) ) )
    }
myForm =
    Form.form Form.f4
        (Form.field
            (Field.custom
                { id = "1"
                , init = 1
                , msg = Set0
                , updater = \delta input -> delta + input
                , parser = Ok
                , renderer = intInput
                }
            )
        )
        (Form.field
            (Field.float "age" Set1
                |> Field.withLabel "How old are you?"
                |> Field.withValidator (Field.floatMustBeGreaterThan 1)
            )
        )
        (Form.field
            (Field.int "shoe-size" Set2
                |> Field.withLabel "What is your shoe size?"
                |> Field.withValidator (Field.intMustBeGreaterThan 0)
            )
        )
        (Form.field
            (Field.int "neg-number" Set3
                |> Field.withLabel "What is your favourite negative number?"
                |> Field.withValidator (Field.intMustBeLessThan 0)
            )
        )
        Form.end


type alias FormState =
    Form.State
        ( Field.State Int
        , ( Field.State String
          , ( Field.State String
            , ( Field.State String
              , ()
              )
            )
          )
        )


type alias Model =
    FormState


initialModel : Model
initialModel =
    myForm.init


type Msg
    = Set0 Int
    | Set1 String
    | Set2 String
    | Set3 String
    | SubmitClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        Set0 s ->
            myForm.update Form.i0 s model

        Set1 s ->
            myForm.update Form.i1 s model

        Set2 s ->
            myForm.update Form.i2 s model

        Set3 s ->
            myForm.update Form.i3 s model

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


intInput : { a | input : Int, msg : number -> msg } -> Element msg
intInput { input, msg } =
    row
        [ spacing 10 ]
        [ Input.button [ Border.width 1, padding 10 ]
            { label = text "-"
            , onPress = Just (msg -1)
            }
        , text <| String.fromInt input
        , Input.button [ Border.width 1, padding 10 ]
            { label = text "+"
            , onPress = Just (msg 1)
            }
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
