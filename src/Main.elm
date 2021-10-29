module Main exposing (main, myForm)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Field
import Form
import Html


type Answer
    = X String
    | O String


myForm =
    Form.form
        (Form.defaultConfig
            |> Form.withSubmit SubmitClicked
        )
        Form.f3
        (q "how can you avoid wheelspin when driving on an icy road?"
            [ O "drive slow in the highest gear possible"
            , X "use the parking brake if the wheels start slipping"
            , X "brake gently and repeatedly"
            , X "drive in a low gear at all times"
            ]
            Set0
        )
        (q "what will help you to move off on a snowy surface?"
            [ X "using the lowest gear"
            , O "using a higher gear than usual"
            , X "using a high engine speed"
            , X "using the brakes"
            ]
            Set1
        )
        (Form.field
            (Field.int "How many smarties can you eat?" Set2)
        )
        Form.end


initialModel =
    myForm.init


type Msg
    = Set0 Answer
    | Set1 Answer
    | Set2 String
    | SubmitClicked


update msg model =
    case msg of
        Set0 s ->
            myForm.update Form.idx0 s model

        Set1 s ->
            myForm.update Form.idx1 s model

        Set2 s ->
            myForm.update (Form.idx1 >> Form.idx1) s model

        SubmitClicked ->
            case myForm.submit model of
                Ok _ ->
                    model

                Err newModel ->
                    newModel


view model =
    layout [] <|
        myForm.view model


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


q label opts msg =
    Field.custom
        { id = ""
        , init = Nothing
        , msg = msg
        , parser =
            \opt ->
                case opt of
                    Just o ->
                        if List.member o opts then
                            Ok opt

                        else
                            Err (Field.IntTooLow 1)

                    Nothing ->
                        Ok opt
        , renderer = questionInput opts
        , updater = \new _ -> Just new
        }
        |> Field.withLabel label
        |> Field.withValidator
            (Maybe.andThen
                (\opt ->
                    case opt of
                        O _ ->
                            Nothing

                        X _ ->
                            Just (Field.IntTooLow 1)
                )
            )
        |> Form.field


questionInput opts { input, parsed, msg, label } =
    column
        [ spacing 10
        , width fill
        , Background.color
            (case parsed of
                Err _ ->
                    rgb255 255 200 200

                Ok (Just (O _)) ->
                    rgb255 200 255 200

                Ok _ ->
                    rgb255 255 255 255
            )
        ]
        [ text (label |> Maybe.withDefault "no label!")
        , column [ spacing 5, width fill ]
            (List.map
                (\opt ->
                    let
                        optString =
                            case opt of
                                X x ->
                                    x

                                O o ->
                                    o
                    in
                    Input.button
                        [ Border.width 1
                        , padding 10
                        , width fill
                        , Background.color
                            (if input == Just opt || input == Nothing then
                                rgb255 220 220 220

                             else
                                rgb255 255 255 255
                            )
                        ]
                        { label = text optString, onPress = Just (msg opt) }
                )
                opts
            )
        ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
