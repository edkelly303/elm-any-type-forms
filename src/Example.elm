module Example exposing (main)

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
    | Field6Changed (Field.Delta String)
    | Field7Changed (Field.Delta Option)
    | SubmitClicked
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
                      , ( Field.State String Int
                        , ( Field.State (Maybe Option) Option
                          , ()
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
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
        , fibonacci : Int
        }


type Option
    = Cats
    | Dogs
    | GuineaPigs
    | Sharks



-- CORE LOGIC


myForm =
    Form.form
        |> Form.withField
            (Widgets.string "What is your name?" Field0Changed
                |> Field.failIf (\x -> String.length x < 1) "Must be at least 1 character"
                |> Field.warnIf
                    (\x ->
                        String.uncons x
                            |> Maybe.map Tuple.first
                            |> Maybe.map Char.isLower
                            |> Maybe.withDefault True
                    )
                    "Names usually start with a capital letter"
                |> Field.infoIf (\x -> String.length x > 20) "Hey, that's a pretty long name you have there!"
                |> Field.infoIf (\x -> String.toLower x == "rebecca") "Let me guess, last night you dreamt you went to Manderley again?"
            )
        |> Form.withField
            (Widgets.float "What shoe size do you wear?" Field1Changed
                |> Field.warnIf (\x -> x > 15) "Gosh those are big feet!"
                |> Field.warnIf (\x -> x < 4) "Gosh those are tiny feet!"
                |> Field.failIf (\x -> x < 0) "You can't have a negative shoe size."
            )
        |> Form.withField
            (Widgets.int "How many dogs have you seen today?" Field2Changed
                |> Field.failIf (\x -> x < 0) "Really? What does a negative dog look like?"
                |> Field.infoIf (\x -> x == 101) "Hello Cruella!"
            )
        |> Form.withField (Widgets.date "What is today's date?" Field3Changed)
        |> Form.withField (Widgets.time "What time is it?" Field4Changed)
        |> Form.withField
            (Widgets.search "Who is your favourite Star Wars character?" Field5Changed identity getStarWarsNames
                |> Field.failIf (\x -> x == "Jar Jar Binks") "Jar Jar Binks cannot be your favourite character."
                |> Field.infoIf (\x -> x == "Lando Calrissian") "You are a person of taste!"
            )
        |> Form.withField (Widgets.fibonacci Field6Changed)
        |> Form.withField
            (Widgets.radioColumn "Which are your preferred animals?"
                Field7Changed
                [ ( Cats, text "Cats" )
                , ( Dogs, text "Dogs" )
                , ( GuineaPigs, text "Guinea pigs" )
                , ( Sharks, text "Sharks" )
                ]
            )
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
        updateField fieldIndex delta =
            let
                ( form, cmd ) =
                    myForm.updateField fieldIndex delta model.form
            in
            ( { model | form = form }, cmd )
    in
    case msg of
        Field0Changed delta ->
            updateField Form.i0 delta

        Field1Changed delta ->
            updateField Form.i1 delta

        Field2Changed delta ->
            updateField Form.i2 delta

        Field3Changed delta ->
            updateField Form.i3 delta

        Field4Changed delta ->
            updateField Form.i4 delta

        Field5Changed delta ->
            updateField Form.i5 delta

        Field6Changed delta ->
            updateField Form.i6 delta

        Field7Changed delta ->
            updateField Form.i7 delta

        SubmitClicked ->
            case myForm.submit model.form of
                Ok ( str, ( flt, ( int, ( date, ( time, ( starwars, ( fibonacci, ( _, () ) ) ) ) ) ) ) ) ->
                    ( { model
                        | page =
                            Submitted
                                { name = str
                                , shoeSize = flt
                                , dogCount = int
                                , dob = date
                                , time = time
                                , favouriteCharacter = starwars
                                , fibonacci = fibonacci
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

            Submitted { name, shoeSize, dogCount, dob, time, favouriteCharacter, fibonacci } ->
                submittedView name shoeSize dogCount dob time favouriteCharacter fibonacci



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


submittedView : String -> Float -> Int -> Date.Date -> Widgets.TimeState -> String -> Int -> Element Msg
submittedView name shoeSize dogCount dob time favouriteCharacter fibonacci =
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
            [ text "Your favourite Star Wars character is "
            , bold favouriteCharacter
            ]
        , paragraph []
            [ text "And your favourite number in the Fibonacci series is "
            , bold (String.fromInt fibonacci)
            ]
        , Element.Input.button
            [ centerX
            , padding 10
            , Element.Border.rounded 3
            , Element.Border.width 1
            ]
            { label = text "Go back", onPress = Just BackClicked }
        ]


getStarWarsNames : { input | search : String } -> Cmd (Result (List String) (List String))
getStarWarsNames { search } =
    Http.get
        { url = "https://swapi.dev/api/people?search=" ++ search
        , expect =
            Http.expectJson
                (Result.map (List.take 5) >> Result.mapError (\_ -> [ "Lando Calrissian" ]))
                starWarsDecoder
        }


starWarsDecoder : JSD.Decoder (List String)
starWarsDecoder =
    JSD.field "results" (JSD.list starWarsPerson)


starWarsPerson : JSD.Decoder String
starWarsPerson =
    JSD.field "name" JSD.string
