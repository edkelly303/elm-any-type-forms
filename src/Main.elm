module Main exposing (main, myForm)

import Browser
import Element exposing (..)
import Field
import Form


myForm =
    Form.form
        (Form.defaultConfig FormMsg
            |> Form.withSubmit SubmitClicked
        )
        Form.f3
        (Form.field (Field.string "What is your name?" Set0))
        (Form.field (Field.float "What is your favourite non-whole number?" Set1))
        (Form.field (Field.int "How many smarties can you eat?" Set2))
        Form.end


initialModel =
    myForm.init


type Msg
    = Set0 String
    | Set1 String
    | Set2 String
    | SubmitClicked
    | FormMsg Form.InternalMsg


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

        FormMsg formMsg ->
            myForm.internalUpdate formMsg model


view model =
    layout [ padding 20 ] <|
        myForm.view model


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
