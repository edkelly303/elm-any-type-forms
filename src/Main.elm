module Main exposing (main)

import Browser
import Date
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
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
        , fibonacci : Int
        }



-- CORE LOGIC


expensive : (Field.Delta () -> msg) -> Field.Field Int () Int (Element msg) msg
expensive deltaMsg =
    Field.custom
        { init = 41
        , deltaMsg = deltaMsg
        , updater = \() input -> input
        , parser = \input -> Ok (fib input)
        , renderer =
            \{ input, delta, parsed, focusMsg, focused } ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 20
                    , Element.padding 20
                    , Element.Border.width 1
                    , Element.Border.rounded 5
                    , Element.Border.color Widgets.paleGrey
                    , Element.Events.onClick focusMsg
                    , Element.Background.color
                        (if focused then
                            Widgets.focusedBlue

                         else
                            Widgets.white
                        )
                    ]
                    [ Element.Input.button
                        [ Element.padding 10
                        , Element.Border.width 1
                        , Element.Border.rounded 3
                        , Element.Border.color Widgets.midGrey
                        , Element.width Element.fill
                        , Element.Background.color Widgets.white
                        , Element.Font.center
                        ]
                        { label = text ("Number " ++ String.fromInt input ++ " in the Fibonacci sequence is...")
                        , onPress = Just (delta ())
                        }
                    , Element.el [ Element.centerX, Element.Font.size 50 ]
                        (text
                            (case parsed of
                                Just n ->
                                    String.fromInt n

                                Nothing ->
                                    "?"
                            )
                        )
                    ]
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
            [ text "Perhaps most important of all, your favourite Star Wars character is "
            , bold favouriteCharacter
            ]
        , paragraph []
            [ text "Finally, did you know that "
            , bold (String.fromInt fibonacci)
            , text " is a number in the Fibonacci sequence?"
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
