module Field exposing
    ( DateDelta
    , DateState
    , Error(..)
    , Field(..)
    , State
    , TimeDelta
    , TimeState
    , custom
    , date
    , float
    , floatMustBeGreaterThan
    , floatMustBeLessThan
    , initialize
    , int
    , intMustBeGreaterThan
    , intMustBeLessThan
    , string
    , stringMustBeLongerThan
    , stringMustBeShorterThan
    , stringMustNotContain
    , time
    , withInitialState
    , withLabel
    , withRenderer
    , withValidator
    )

import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Internals exposing (..)
import Time


type Field input delta output element msg
    = Field
        { index : Int
        , init : input
        , msg : delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool

            -- , onBlurMsg : msg
            , onFocusMsg : msg
            , msg : delta -> msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
        , label : Maybe String
        }


type alias State input =
    { input : input }


type Error
    = NotValidInt
    | NotValidFloat
    | IntTooLow Int
    | IntTooHigh Int
    | FloatTooLow Float
    | FloatTooHigh Float
    | StringTooShort Int
    | StringTooLong Int
    | StringMustNotContain String
    | NoDateSelected
    | Custom String


errorToString : Error -> String
errorToString e =
    case e of
        NotValidInt ->
            "Must be a whole number"

        NotValidFloat ->
            "Must be a number"

        IntTooLow i ->
            "Must be a whole number higher than " ++ String.fromInt i

        IntTooHigh i ->
            "Must be a whole number lower than " ++ String.fromInt i

        FloatTooLow f ->
            "Must be a number higher than " ++ String.fromFloat f

        FloatTooHigh f ->
            "Must be a number lower than " ++ String.fromFloat f

        StringTooShort i ->
            "Must be longer than " ++ String.fromInt i ++ " characters"

        StringTooLong i ->
            "Must be shorter than " ++ String.fromInt i ++ " characters"

        StringMustNotContain str ->
            "Must not contain '" ++ str ++ "'"

        NoDateSelected ->
            "No date selected"

        Custom str ->
            str



-- CREATING FIELDS


custom :
    { a
        | init : input
        , msg : delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool

            -- , onBlurMsg : msg
            , onFocusMsg : msg
            , msg : delta -> msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
    }
    -> Field input delta output element msg
custom { init, msg, updater, parser, renderer, id } =
    Field
        { index = 0
        , init = init
        , msg = msg
        , updater = updater
        , parser = parser
        , validators = []
        , renderer = renderer
        , id = id
        , label = Nothing
        }


string : String -> (String -> msg) -> Field String String String (Element msg) msg
string id msg =
    custom
        { init = ""
        , msg = msg
        , updater = \delta _ -> delta
        , parser = Ok
        , renderer = renderTextField
        , id = id
        }


float : String -> (String -> msg) -> Field String String Float (Element msg) msg
float id msg =
    custom
        { init = ""
        , msg = msg
        , updater = \delta _ -> delta
        , parser = String.toFloat >> Result.fromMaybe NotValidFloat
        , renderer = renderTextField
        , id = id
        }


int : String -> (String -> msg) -> Field String String Int (Element msg) msg
int id msg =
    custom
        { init = ""
        , msg = msg
        , updater = \delta _ -> delta
        , parser = String.toInt >> Result.fromMaybe NotValidInt
        , renderer = renderTextField
        , id = id
        }


type DateDelta
    = PageChanged Date.Unit Int
    | DateSelected Date.Date


type alias DateState =
    { page : Date.Date, selected : Maybe Date.Date }


date : String -> (DateDelta -> msg) -> Field DateState DateDelta Date.Date (Element msg) msg
date id msg =
    custom
        { init = { page = Date.fromCalendarDate 2020 Time.Feb 1, selected = Nothing }
        , msg = msg
        , updater =
            \delta state ->
                case delta of
                    PageChanged interval amount ->
                        { state | page = Date.add interval amount state.page }

                    DateSelected d ->
                        { state
                            | selected =
                                case state.selected of
                                    Nothing ->
                                        Just d

                                    Just s ->
                                        if s == d then
                                            Nothing

                                        else
                                            Just d
                        }
        , parser = \state -> Result.fromMaybe NoDateSelected state.selected
        , renderer = renderDatePicker
        , id = id
        }


type alias TimeState =
    { hours : Int, minutes : Int }


type TimeDelta
    = MinutesChanged Int
    | HoursChanged Int


time : String -> (TimeDelta -> msg) -> Field TimeState TimeDelta TimeState (Element msg) msg
time id msg =
    custom
        { init = { hours = 12, minutes = 0 }
        , msg = msg
        , updater =
            \delta state ->
                case delta of
                    HoursChanged diff ->
                        { state | hours = state.hours + diff |> modBy 24 }

                    MinutesChanged diff ->
                        { state | minutes = state.minutes + diff |> modBy 60 }
        , parser =
            \state ->
                if state.hours >= 0 && state.hours < 24 && state.minutes >= 0 && state.minutes < 60 then
                    Ok state

                else
                    Err (Custom "")
        , renderer = renderTimePicker
        , id = id
        }



-- WORKING WITH FIELDS


initialize : Field input delta output element msg -> State input
initialize (Field { init }) =
    { input = init }


withLabel : String -> Field input delta output element msg -> Field input delta output element msg
withLabel l (Field f) =
    Field { f | label = Just l }


withValidator : (output -> Maybe Error) -> Field input delta output element msg -> Field input delta output element msg
withValidator v (Field f) =
    Field { f | validators = v :: f.validators }


withInitialState : input -> Field input delta output element msg -> Field input delta output element msg
withInitialState input (Field f) =
    Field { f | init = input }


withRenderer :
    ({ input : input
     , touched : Bool
     , focused : Bool
     , typing : Bool

     --  , onBlurMsg : msg
     , onFocusMsg : msg
     , msg : delta -> msg
     , parsed : Result (List Error) output
     , label : Maybe String
     , id : String
     }
     -> element2
    )
    -> Field input delta output element msg
    -> Field input delta output element2 msg
withRenderer r (Field f) =
    Field
        { index = f.index
        , init = f.init
        , msg = f.msg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , renderer = r
        , id = f.id
        , label = f.label
        }



-- CREATING VALIDATORS


ifTrueThenJust : a -> Bool -> Maybe a
ifTrueThenJust a bool =
    if bool then
        Just a

    else
        Nothing


stringMustBeLongerThan : Int -> String -> Maybe Error
stringMustBeLongerThan numberOfChars str =
    (String.length str <= numberOfChars)
        |> ifTrueThenJust (StringTooShort numberOfChars)


stringMustBeShorterThan : Int -> String -> Maybe Error
stringMustBeShorterThan numberOfChars str =
    (String.length str >= numberOfChars)
        |> ifTrueThenJust (StringTooLong numberOfChars)


stringMustNotContain : String -> String -> Maybe Error
stringMustNotContain content str =
    String.contains content str
        |> ifTrueThenJust (StringMustNotContain content)


intMustBeGreaterThan : Int -> Int -> Maybe Error
intMustBeGreaterThan tooLow int_ =
    (int_ <= tooLow)
        |> ifTrueThenJust (IntTooLow tooLow)


intMustBeLessThan : Int -> Int -> Maybe Error
intMustBeLessThan tooHigh int_ =
    (int_ >= tooHigh)
        |> ifTrueThenJust (IntTooHigh tooHigh)


floatMustBeGreaterThan : Float -> Float -> Maybe Error
floatMustBeGreaterThan tooLow float_ =
    (float_ <= tooLow)
        |> ifTrueThenJust (FloatTooLow tooLow)


floatMustBeLessThan : Float -> Float -> Maybe Error
floatMustBeLessThan tooHigh float_ =
    (float_ >= tooHigh)
        |> ifTrueThenJust (FloatTooHigh tooHigh)



-- RENDERERS


renderTextField :
    { onFocusMsg : msg

    -- , onBlurMsg : msg
    , input : String
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , msg : String -> msg
    , parsed : Result (List Error) output
    , id : String
    , label : Maybe String
    }
    -> Element msg
renderTextField { input, touched, typing, onFocusMsg, focused, msg, parsed, id, label } =
    column
        [ spacing 10
        , padding 20
        , width fill
        , Background.color
            (if focused then
                focusedBlue

             else
                white
            )
        , Border.rounded 5
        , Events.onClick onFocusMsg
        ]
        [ Input.text
            [ Background.color
                (case ( touched, parsed, typing ) of
                    ( True, Ok _, False ) ->
                        green

                    ( True, Err _, False ) ->
                        red

                    _ ->
                        white
                )
            , htmlAttribute (Html.Attributes.id id)
            , Events.onFocus onFocusMsg

            -- , Events.onLoseFocus onBlurMsg
            ]
            { label = Input.labelAbove [ Font.size 18 ] (text (label |> Maybe.withDefault id))
            , text = input
            , placeholder = Nothing
            , onChange = msg
            }
        , if typing then
            text "..."

          else
            none
        , case ( touched, parsed, typing ) of
            ( True, Err errs, False ) ->
                column [ Font.size 12, spacing 5 ] (List.map (errorToString >> text) errs)

            _ ->
                none
        ]


red : Color
red =
    rgb255 255 100 100


green : Color
green =
    rgb255 100 255 100


focusedBlue : Color
focusedBlue =
    rgb255 220 255 220


type CalendarBlock
    = Blank
    | Day Date.Date


renderDatePicker :
    { onFocusMsg : msg

    -- , onBlurMsg : msg
    , input : { page : Date.Date, selected : Maybe Date.Date }
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , msg : DateDelta -> msg
    , parsed : Result (List Error) output
    , id : String
    , label : Maybe String
    }
    -> Element msg
renderDatePicker { input, msg, label, id, focused, onFocusMsg, touched, parsed, typing } =
    let
        firstDayOfNextMonth =
            Date.add Date.Months 1 firstDayOfMonth

        firstDayOfMonth =
            Date.floor Date.Month input.page

        days =
            Date.range Date.Day 1 firstDayOfMonth firstDayOfNextMonth

        daysOffsetAtStart =
            List.head days
                |> Maybe.map Date.weekdayNumber
                |> Maybe.withDefault 0

        blanksAtStart =
            List.repeat (daysOffsetAtStart - 1) Blank

        daysOffsetAtEnd =
            List.reverse days
                |> List.head
                |> Maybe.map Date.weekdayNumber
                |> Maybe.withDefault 0

        blanksAtEnd =
            List.repeat (7 - daysOffsetAtEnd) Blank

        blocks =
            blanksAtStart ++ List.map Day days ++ blanksAtEnd

        weeks =
            List.foldl
                (\b ( idx, out ) ->
                    ( idx + 1
                    , case out of
                        [] ->
                            [ [ b ] ]

                        fst :: rest ->
                            if modBy 7 idx == 0 then
                                [ b ] :: out

                            else
                                (b :: fst) :: rest
                    )
                )
                ( 0, [] )
                blocks
                |> Tuple.second
                |> List.map List.reverse
                |> List.reverse

        button txt m =
            Input.button
                [ width <| px maxButtonSize
                , Events.onFocus onFocusMsg
                ]
                { label = box transparent midGrey (text txt)
                , onPress = Just (msg m)
                }
    in
    column
        [ width fill
        , spacing 10
        , padding 20
        , width fill
        , Events.onClick onFocusMsg
        , Background.color
            (if focused then
                rgb255 220 255 220

             else
                rgb255 255 255 255
            )
        , Border.rounded 5
        ]
        [ el [ Font.size 18 ] (text (label |> Maybe.withDefault id))
        , row [ spaceEvenly, width <| maximum maxCalendarWidth <| fill, centerX ]
            [ button "<" (PageChanged Date.Months -1)
            , text (Date.format "MMM" input.page)
            , button ">" (PageChanged Date.Months 1)
            , button "<" (PageChanged Date.Years -1)
            , text (Date.format "yyyy" input.page)
            , button ">" (PageChanged Date.Years 1)
            ]
        , row [ spacing 8, width <| maximum maxCalendarWidth <| fill, centerX ] headerRow
        , column [ spacing 8, width <| maximum maxCalendarWidth <| fill, centerX ]
            (List.map
                (\w ->
                    row [ spacing 8, width fill ]
                        (List.map (viewBlock msg onFocusMsg input.selected) w)
                )
                weeks
            )
        , case ( touched, parsed, typing ) of
            ( True, Err errs, False ) ->
                column [ Font.size 12, spacing 5 ] (List.map (errorToString >> text) errs)

            _ ->
                none
        ]


viewBlock : (DateDelta -> a) -> a -> Maybe Date.Date -> CalendarBlock -> Element a
viewBlock msg onFocusMsg selected block =
    let
        colour d =
            if Just d == selected then
                { bg = green, bd = midGrey }

            else
                { bg = transparent, bd = midGrey }
    in
    case block of
        Blank ->
            box transparent transparent none

        Day d ->
            let
                { bg, bd } =
                    colour d
            in
            Input.button
                [ width <| maximum maxButtonSize <| fill
                , Events.onFocus onFocusMsg

                -- , Events.onLoseFocus onBlurMsg
                ]
                { label = box bg bd (text <| String.fromInt <| Date.day d)
                , onPress = Just (msg (DateSelected d))
                }


maxCalendarWidth : Int
maxCalendarWidth =
    (7 * maxButtonSize) + (6 * buttonSpacing)


maxButtonSize : Int
maxButtonSize =
    44


buttonSpacing : Int
buttonSpacing =
    8


midGrey : Color
midGrey =
    rgb255 150 150 150


paleGrey : Color
paleGrey =
    rgb255 225 225 225


white : Color
white =
    rgb255 255 255 255


transparent : Color
transparent =
    rgba255 255 255 255 0


headerRow : List (Element msg)
headerRow =
    [ box transparent transparent (text "Mo")
    , box transparent transparent (text "Tu")
    , box transparent transparent (text "We")
    , box transparent transparent (text "Th")
    , box transparent transparent (text "Fr")
    , box transparent transparent (text "Sa")
    , box transparent transparent (text "Su")
    ]


box : Color -> Color -> Element msg -> Element msg
box bgColour borderColour content =
    el
        [ width <| px maxButtonSize
        , height <| px maxButtonSize
        , Background.color bgColour
        , alignLeft
        , Border.color borderColour
        , Border.width 1
        , Border.rounded 3
        ]
        (el [ centerX, centerY ] content)


renderTimePicker { input, id, label, msg, onFocusMsg, focused } =
    let
        button txt m =
            Input.button
                [ width <| px 40
                , height <| px 40
                , Border.rounded 3
                , Border.width 1
                , Border.color midGrey
                ]
                { label = text txt, onPress = Just (msg m) }

        hours =
            String.fromInt input.hours
                |> String.padLeft 2 '0'
                |> String.split ""
                |> List.map (\x -> el [ width <| px 40 ] <| text x)

        minutes =
            String.fromInt input.minutes
                |> String.padLeft 2 '0'
                |> String.split ""
                |> List.map (\x -> el [ width <| px 40 ] <| text x)
    in
    column
        [ width fill
        , spacing 10
        , padding 20
        , width fill
        , Events.onClick onFocusMsg
        , Background.color
            (if focused then
                focusedBlue

             else
                white
            )
        , Border.rounded 5
        , Font.center
        ]
        [ text (Maybe.withDefault id label)
        , row [ spacing 5, centerX ]
            [ button "+" (HoursChanged 10)
            , button "+" (HoursChanged 1)
            , el [ width <| px 40 ] none
            , button "+" (MinutesChanged 10)
            , button "+" (MinutesChanged 1)
            ]
        , row [ spacing 5, centerX ]
            (hours
                ++ [ el [ width <| px 40 ] <| text ":" ]
                ++ minutes
            )
        , row [ spacing 5, centerX ]
            [ button "-" (HoursChanged -10)
            , button "-" (HoursChanged -1)
            , el [ width <| px 40 ] none
            , button "-" (MinutesChanged -10)
            , button "-" (MinutesChanged -1)
            ]
        ]
