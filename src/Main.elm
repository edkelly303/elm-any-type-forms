module Main exposing (main)

import Browser
import Element exposing (..)
import Field
import Form
import Html


type Page
    = Form
    | Submitted { name : String, number : Float, smarties : Int }


myForm2 =
    Form.form FormMsg
        |> Form.withField (Field.string "What is your name?" Set0)
        |> Form.withField (Field.float "What is your favourite non-whole number?" Set1)
        |> Form.withField (Field.int "How many smarties can you eat?" Set2)
        |> Form.withSubmit SubmitClicked
        |> Form.done


type alias Model =
    { page : Page
    , form : Form.State ( Field.State String, ( Field.State String, ( Field.State String, () ) ) )
    }


initialModel : Model
initialModel =
    { page = Form
    , form = myForm2.init
    }


type Msg
    = Set0 String
    | Set1 String
    | Set2 String
    | SubmitClicked
    | FormMsg Form.InternalMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Set0 s ->
            { model | form = myForm2.updateField Form.i0 s model.form }

        Set1 s ->
            { model | form = myForm2.updateField Form.i1 s model.form }

        Set2 s ->
            { model | form = myForm2.updateField (Form.i1 >> Form.i1) s model.form }

        SubmitClicked ->
            case myForm2.submit model.form of
                Ok ( str, ( flt, ( int, () ) ) ) ->
                    { model | page = Submitted { name = str, number = flt, smarties = int } }

                Err newForm ->
                    { model | form = newForm }

        FormMsg formMsg ->
            { model | form = myForm2.update formMsg model.form }


view model =
    layout [ padding 20 ] <|
        case model.page of
            Form ->
                myForm2.view model.form

            Submitted { name, number, smarties } ->
                column []
                    [ text "Form submitted!"
                    , text <| "Hello " ++ name ++ "!"
                    , text <|
                        "Your favourite non-whole number is "
                            ++ String.fromFloat number
                            ++ " and you can eat "
                            ++ String.fromInt smarties
                            ++ " smarties."
                    ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
