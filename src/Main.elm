module Main exposing (main)

import Browser
import Date exposing (Date)
import Element exposing (..)
import Element.Border
import Element.Font
import Field
import Form


type Page
    = Form
    | Submitted { name : String, shoeSize : Float, dogCount : Int, dob : Date.Date }


myForm =
    Form.form FormMsg
        |> Form.withField (Field.string "What is your name?" Set0)
        |> Form.withField (Field.float "What shoe size do you wear?" Set1)
        |> Form.withField (Field.int "How many dogs have you seen today?" Set2)
        |> Form.withField (Field.date "What is your date of birth?" Set3)
        |> Form.withSubmit SubmitClicked
        |> Form.done


type alias Model =
    { page : Page
    , form : Form.State ( Field.State String, ( Field.State String, ( Field.State String, ( Field.State { page : Date, selected : Maybe Date }, () ) ) ) )
    }


initialModel : Model
initialModel =
    { page = Form
    , form = myForm.init
    }


type Msg
    = Set0 String
    | Set1 String
    | Set2 String
    | Set3 Field.DateDelta
    | SubmitClicked
    | FormMsg Form.InternalMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        wrap =
            Tuple.mapFirst (\form -> { model | form = form })
    in
    case msg of
        FormMsg formMsg ->
            myForm.update formMsg model.form
                |> wrap

        Set0 s ->
            myForm.updateField Form.i0 s model.form
                |> wrap

        Set1 s ->
            myForm.updateField Form.i1 s model.form
                |> wrap

        Set2 s ->
            myForm.updateField (Form.i1 >> Form.i1) s model.form
                |> wrap

        Set3 s ->
            myForm.updateField (Form.i1 >> Form.i1 >> Form.i1) s model.form
                |> wrap

        SubmitClicked ->
            case myForm.submit model.form of
                Ok ( str, ( flt, ( int, ( date, () ) ) ) ) ->
                    ( { model | page = Submitted { name = str, shoeSize = flt, dogCount = int, dob = date } }, Cmd.none )

                Err newForm ->
                    ( { model | form = newForm }, Cmd.none )


view model =
    layout [ padding 20 ] <|
        case model.page of
            Form ->
                myForm.view model.form

            Submitted { name, shoeSize, dogCount, dob } ->
                let
                    bold x =
                        el [ Element.Font.bold ] (text x)
                in
                column
                    [ centerX
                    , centerY
                    , width <| px 300
                    , Element.Border.width 1
                    , Element.Border.rounded 5
                    , padding 20
                    , spacing 10
                    ]
                    [ text "Form submitted!"
                    , paragraph []
                        [ text "Hello "
                        , bold name
                        , text "!"
                        ]
                    , paragraph []
                        [ text "You wear size "
                        , bold (String.fromFloat shoeSize)
                        , text " shoes, and you have seen "
                        , bold (String.fromInt dogCount)
                        , text " dogs today."
                        ]
                    , paragraph []
                        [ text "Your date of birth is "
                        , bold (Date.format "d MMMM yyyy" dob)
                        ]
                    ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
