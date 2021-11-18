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
import Widgets



-- TYPES


type Msg
    = Field0Changed (Field.Delta String)
    | Field1Changed (Field.Delta String)
    | Field2Changed (Field.Delta String)
    | Field3Changed (Field.Delta Widgets.DateDelta)
    | Field4Changed (Field.Delta Widgets.TimeDelta)
    | Field5Changed (Field.Delta (Widgets.SearchDelta String))
    | Field6Changed (Field.Delta ())
    | SubmitClicked
    | FormChanged (Form.InternalMsg Msg)
    | BackClicked


type alias Model =
    { page : Page
    , form :
        Form.State
            ( Field.State String String
            , ( Field.State String Float
              , ( Field.State String Int
                , ( Field.State Widgets.DateState Date.Date
                  , ( Field.State Widgets.TimeState Widgets.TimeState
                    , ( Field.State (Widgets.SearchState String) String
                      , ( Field.State Int Int
                        , ()
                        )
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
        , time : Widgets.TimeState
        , favouriteCharacter : String
        }



-- CORE LOGIC


expensive : (Field.Delta () -> msg) -> Field.Field Int () Int (Element msg) msg
expensive deltaMsg =
    Field.custom
        { init = 40
        , deltaMsg = deltaMsg
        , updater = \() input -> input
        , parser = \input -> Ok (Debug.log "Parsing..." fib input)
        , renderer = \{ delta } -> Element.Input.button [] { label = text "fib", onPress = Just (delta ()) }
        , label = "Now let's do some hard work"
        }


fib : Int -> Int
fib x =
    case x of
        0 ->
            1

        1 ->
            1

        _ ->
            fib (x - 1) + fib (x - 2)


myForm =
    Form.form FormChanged
        |> Form.withField (Widgets.string "What is your name?" Field0Changed |> Field.withValidator (Field.stringMustBeLongerThan 0))
        |> Form.withField (Widgets.float "What shoe size do you wear?" Field1Changed)
        |> Form.withField (Widgets.int "How many dogs have you seen today?" Field2Changed)
        |> Form.withField (Widgets.date "What is today's date?" Field3Changed)
        |> Form.withField (Widgets.time "What time is it?" Field4Changed)
        |> Form.withField (Widgets.search "Who is your favourite Star Wars character?" Field5Changed identity getStarWarsNames)
        |> Form.withField (expensive Field6Changed)
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
        FormChanged delta ->
            myForm.updateForm delta model.form
                |> wrap

        Field0Changed delta ->
            myForm.updateField Form.i0 delta model.form
                |> wrap

        Field1Changed delta ->
            myForm.updateField Form.i1 delta model.form
                |> wrap

        Field2Changed delta ->
            myForm.updateField Form.i2 delta model.form
                |> wrap

        Field3Changed delta ->
            myForm.updateField Form.i3 delta model.form
                |> wrap

        Field4Changed delta ->
            myForm.updateField Form.i4 delta model.form
                |> wrap

        Field5Changed delta ->
            myForm.updateField Form.i5 delta model.form
                |> wrap

        Field6Changed delta ->
            myForm.updateField Form.i6 delta model.form
                |> wrap

        SubmitClicked ->
            case myForm.submit model.form of
                Ok ( str, ( flt, ( int, ( date, ( time, ( starwars, ( fibonacci, () ) ) ) ) ) ) ) ->
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



-- HELPERS


submittedView : String -> Float -> Int -> Date.Date -> Widgets.TimeState -> String -> Element Msg
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
            [ text "Today's date is "
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
                            List.take 5 list

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
