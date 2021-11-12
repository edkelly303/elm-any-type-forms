module Field exposing
    ( DateDelta
    , DateState
    , Error(..)
    , Field(..)
    , SearchDelta(..)
    , SearchState
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
    , search
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
        , deltaMsg : delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool
            , deltaMsg : delta -> msg
            , focusMsg : msg
            , requestCmdMsg : msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
        , label : Maybe String
        , loadCmd : input -> Cmd msg
        , typingTimeout : Float
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
        , deltaMsg : delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool
            , requestCmdMsg : msg
            , focusMsg : msg
            , deltaMsg : delta -> msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
        , loadCmd : input -> Cmd msg
        , typingTimeout : Float
    }
    -> Field input delta output element msg
custom { init, deltaMsg, updater, parser, renderer, id, loadCmd, typingTimeout } =
    Field
        { index = 0
        , init = init
        , deltaMsg = deltaMsg
        , updater = updater
        , parser = parser
        , validators = []
        , renderer = renderer
        , id = id
        , label = Nothing
        , loadCmd = loadCmd
        , typingTimeout = typingTimeout
        }


string : String -> (String -> msg) -> Field String String String (Element msg) msg
string id msg =
    custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = Ok
        , renderer = renderTextField
        , id = id
        , loadCmd = always Cmd.none
        , typingTimeout = 500
        }


float : String -> (String -> msg) -> Field String String Float (Element msg) msg
float id msg =
    custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = String.toFloat >> Result.fromMaybe NotValidFloat
        , renderer = renderTextField
        , id = id
        , loadCmd = always Cmd.none
        , typingTimeout = 500
        }


int : String -> (String -> msg) -> Field String String Int (Element msg) msg
int id msg =
    custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = String.toInt >> Result.fromMaybe NotValidInt
        , renderer = renderTextField
        , id = id
        , loadCmd = always Cmd.none
        , typingTimeout = 500
        }


type DateDelta
    = PageChanged Date.Unit Int
    | DateSelected Date.Date


type alias DateState =
    { page : Date.Date, selected : Maybe Date.Date }


date : String -> (DateDelta -> msg) -> Field DateState DateDelta Date.Date (Element msg) msg
date id msg =
    custom
        { init = { page = Date.fromCalendarDate 2021 Time.Nov 1, selected = Nothing }
        , deltaMsg = msg
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
        , loadCmd = always Cmd.none
        , typingTimeout = 500
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
        , deltaMsg = msg
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
        , loadCmd = always Cmd.none
        , typingTimeout = 500
        }


type SearchDelta a
    = SearchChanged String
    | ResultsLoaded (List a)
    | ResultSelected a


type alias SearchState a =
    { search : String
    , options : List a
    , selected : Maybe a
    }


search :
    String
    -> (SearchDelta a -> msg)
    -> (a -> String)
    -> (SearchState a -> Cmd (List a))
    -> Field (SearchState a) (SearchDelta a) a (Element msg) msg
search id msg toString loadCmd =
    custom
        { init = { search = "", options = [], selected = Nothing }
        , deltaMsg = msg
        , updater =
            \delta state ->
                case delta of
                    SearchChanged s ->
                        { state | search = s }

                    ResultsLoaded l ->
                        { state | options = l }

                    ResultSelected r ->
                        { state | selected = Just r }
        , parser = \state -> Result.fromMaybe (Custom "Must select something") state.selected
        , renderer = renderSearchField toString
        , id = id
        , loadCmd = loadCmd >> Cmd.map (ResultsLoaded >> msg)
        , typingTimeout = 1000
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
     , requestCmdMsg : msg
     , focusMsg : msg
     , deltaMsg : delta -> msg
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
        , deltaMsg = f.deltaMsg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , renderer = r
        , id = f.id
        , label = f.label
        , loadCmd = f.loadCmd
        , typingTimeout = f.typingTimeout
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
    { focusMsg : msg
    , requestCmdMsg : msg
    , deltaMsg : String -> msg
    , input : String
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , parsed : Result (List Error) output
    , id : String
    , label : Maybe String
    }
    -> Element msg
renderTextField { input, touched, typing, focusMsg, focused, deltaMsg, parsed, id, label } =
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
        , Events.onClick focusMsg
        ]
        [ Input.text
            [ htmlAttribute (Html.Attributes.id id)
            , Events.onFocus focusMsg
            ]
            { label = Input.labelAbove [ Font.size 18 ] (text (label |> Maybe.withDefault id))
            , text = input
            , placeholder = Nothing
            , onChange = deltaMsg
            }
        , if typing then
            text "..."

          else
            none
        , viewErrors touched parsed typing
        ]


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
    { focusMsg : msg
    , requestCmdMsg : msg
    , input : { page : Date.Date, selected : Maybe Date.Date }
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , deltaMsg : DateDelta -> msg
    , parsed : Result (List Error) output
    , id : String
    , label : Maybe String
    }
    -> Element msg
renderDatePicker { input, deltaMsg, label, id, focused, focusMsg, touched, parsed, typing } =
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
                , Events.onFocus focusMsg
                ]
                { label = box transparent midGrey (text txt)
                , onPress = Just (deltaMsg m)
                }
    in
    column
        [ width fill
        , spacing 10
        , padding 20
        , width fill
        , Events.onClick focusMsg
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
                        (List.map (viewBlock deltaMsg focusMsg input.selected) w)
                )
                weeks
            )
        , viewErrors touched parsed typing
        ]


viewBlock : (DateDelta -> a) -> a -> Maybe Date.Date -> CalendarBlock -> Element a
viewBlock msg focusMsg selected block =
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
                , Events.onFocus focusMsg
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


renderTimePicker { input, id, label, deltaMsg, focusMsg, focused } =
    let
        button txt m =
            Input.button
                [ width <| px 40
                , height <| px 40
                , Border.rounded 3
                , Border.width 1
                , Border.color midGrey
                ]
                { label = text txt, onPress = Just (deltaMsg m) }

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
        , Events.onClick focusMsg
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
            (hours ++ (el [ width <| px 40 ] (text ":") :: minutes))
        , row [ spacing 5, centerX ]
            [ button "-" (HoursChanged -10)
            , button "-" (HoursChanged -1)
            , el [ width <| px 40 ] none
            , button "-" (MinutesChanged -10)
            , button "-" (MinutesChanged -1)
            ]
        ]


renderSearchField :
    (a -> String)
    ->
        { focusMsg : msg
        , requestCmdMsg : msg
        , input : SearchState a
        , touched : Bool
        , focused : Bool
        , typing : Bool
        , deltaMsg : SearchDelta a -> msg
        , parsed : Result (List Error) output
        , id : String
        , label : Maybe String
        }
    -> Element msg
renderSearchField toString { label, id, input, deltaMsg, requestCmdMsg, focusMsg, focused, touched, parsed, typing } =
    column
        [ spacing 10
        , padding 20
        , Events.onClick focusMsg
        , Background.color
            (if focused then
                focusedBlue

             else
                white
            )
        , Border.rounded 5
        ]
        [ row [ spacing 10 ]
            [ Input.text []
                { onChange = deltaMsg << SearchChanged
                , placeholder = Nothing
                , label = Input.labelAbove [] (text (Maybe.withDefault id label))
                , text = input.search
                }
            , Input.button [ padding 10, Border.width 1, Border.rounded 3, Border.color midGrey ] { label = text "search", onPress = Just requestCmdMsg }
            ]
        , if typing then
            text "..."

          else
            none
        , column [ spacing 10, width fill ] <|
            List.map
                (\item ->
                    Input.button
                        [ Background.color
                            (if Just item == input.selected then
                                green

                             else
                                transparent
                            )
                        , Border.width 1
                        , Border.rounded 3
                        , Border.color midGrey
                        , padding 10
                        , width fill
                        ]
                        { label = text (toString item)
                        , onPress = Just (deltaMsg <| ResultSelected item)
                        }
                )
                input.options
        , viewErrors touched parsed typing
        ]


viewErrors : Bool -> Result (List Error) output -> Bool -> Element msg
viewErrors touched parsed typing =
    case ( touched, parsed, typing ) of
        ( True, Err errs, False ) ->
            column [ Font.size 12, spacing 5, Font.color (rgb255 220 0 150) ] (List.map (errorToString >> text) errs)

        _ ->
            none
