module Main exposing (main, myForm)

import Browser
import Date
import Element exposing (..)
import Element.Border
import Element.Font
import Element.Input
import Field
import Form
import Html
import Http
import Json.Decode as JSD



-- TYPES


type Msg
    = Set0 String
    | Set1 String
    | Set2 String
    | Set3 Field.DateDelta
    | Set4 Field.TimeDelta
    | Set5 (Field.SearchDelta String)
    | SubmitClicked
    | FormMsg Form.InternalMsg
    | BackClicked


type alias Model =
    { page : Page
    , form :
        Form.State
            ( Field.State String
            , ( Field.State String
              , ( Field.State String
                , ( Field.State Field.DateState
                  , ( Field.State Field.TimeState
                    , ( Field.State (Field.SearchState String)
                      , ()
                      )
                    )
                  )
                )
              )
            )
            Msg
    }


type Page
    = Form
    | Submitted
        { name : String
        , shoeSize : Float
        , dogCount : Int
        , dob : Date.Date
        , time : Field.TimeState
        , favouriteCharacter : String
        }



-- CORE LOGIC


myForm =
    Form.form FormMsg
        |> Form.withField
            (Field.string "What is your name?" Set0
                |> Field.withValidator (Field.stringMustBeLongerThan 0)
            )
        |> Form.withField (Field.float "What shoe size do you wear?" Set1)
        |> Form.withField (Field.int "How many dogs have you seen today?" Set2)
        |> Form.withField (Field.date "What is your date of birth?" Set3)
        |> Form.withField (Field.time "What time is it?" Set4)
        |> Form.withField (Field.search "Who is your favourite Star Wars character?" Set5 identity getStarWarsNames)
        |> Form.withSubmit SubmitClicked
        |> Form.done


initialModel : Model
initialModel =
    { page = Form
    , form = myForm.init
    }


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
            myForm.updateField Form.i2 s model.form
                |> wrap

        Set3 s ->
            myForm.updateField Form.i3 s model.form
                |> wrap

        Set4 s ->
            myForm.updateField Form.i4 s model.form
                |> wrap

        Set5 s ->
            myForm.updateField Form.i5 s model.form
                |> wrap

        SubmitClicked ->
            case myForm.submit model.form of
                Ok ( str, ( flt, ( int, ( date, ( time, ( starwars, () ) ) ) ) ) ) ->
                    ( { model
                        | page =
                            Submitted
                                { name = str
                                , shoeSize = flt
                                , dogCount = int
                                , dob = date
                                , time = time
                                , favouriteCharacter = starwars
                                }
                      }
                    , Cmd.none
                    )

                Err newForm ->
                    ( { model | form = newForm }, Cmd.none )

        BackClicked ->
            ( { model | page = Form, form = myForm.init }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    layout [ padding 20 ] <|
        case model.page of
            Form ->
                myForm.view model.form

            Submitted { name, shoeSize, dogCount, dob, time, favouriteCharacter } ->
                submittedView name shoeSize dogCount dob time favouriteCharacter



-- PROGRAM BOILERPLATE


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- HELPER CRAP


submittedView : String -> Float -> Int -> Date.Date -> Field.TimeState -> String -> Element Msg
submittedView name shoeSize dogCount dob time favouriteCharacter =
    let
        bold x =
            el [ Element.Font.bold ] (text x)
    in
    column
        [ centerX
        , centerY
        , width <| px 400
        , Element.Border.width 1
        , Element.Border.rounded 5
        , padding 30
        , spacing 20
        ]
        [ paragraph []
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
            , bold (Date.format "ddd MMMM yyyy" dob)
            , text ", and the time is currently "
            , bold (String.fromInt time.hours |> String.padLeft 2 '0')
            , bold ":"
            , bold (String.fromInt time.minutes |> String.padLeft 2 '0')
            ]
        , paragraph []
            [ text "Perhaps most important of all, your favourite Star Wars character is "
            , bold favouriteCharacter
            ]
        , Element.Input.button
            [ centerX
            , padding 10
            , Element.Border.rounded 3
            , Element.Border.width 1
            ]
            { label = text "Go back", onPress = Just BackClicked }
        ]


getStarWarsNames : { input | search : String } -> Cmd (List String)
getStarWarsNames { search } =
    Http.get
        { url = "https://swapi.dev/api/people?search=" ++ search
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Ok list ->
                            list

                        Err _ ->
                            []
                )
                starWarsDecoder
        }


starWarsDecoder : JSD.Decoder (List String)
starWarsDecoder =
    JSD.field "results" (JSD.list starWarsPerson)


starWarsPerson : JSD.Decoder String
starWarsPerson =
    JSD.field "name" JSD.string
