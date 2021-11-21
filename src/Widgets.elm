module Widgets exposing (..)

import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import FeatherIcons as FI
import Field exposing (RendererConfig)
import Html.Attributes
import Time



-- d888888b d88888b db    db d888888b
-- `~~88~~' 88'     `8b  d8' `~~88~~'
--    88    88ooooo  `8bd8'     88
--    88    88~~~~~  .dPYb.     88
--    88    88.     .8P  Y8.    88
--    YP    Y88888P YP    YP    YP
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


string : String -> (Field.Delta String -> msg) -> Field.Field String String String (Element msg) msg
string label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = Ok
        , renderer = renderTextField
        , label = label
        }


float : String -> (Field.Delta String -> msg) -> Field.Field String String Float (Element msg) msg
float label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = String.toFloat >> Result.fromMaybe (Field.fail "Must be a number")
        , renderer = renderTextField
        , label = label
        }


int : String -> (Field.Delta String -> msg) -> Field.Field String String Int (Element msg) msg
int label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = String.toInt >> Result.fromMaybe (Field.fail "Must be a whole number")
        , renderer = renderTextField
        , label = label
        }


renderTextField : RendererConfig String String output msg -> Element msg
renderTextField { input, status, focusMsg, focused, debouncedDelta, parsed, feedback, id, label } =
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
        , Border.width 1
        , Border.color paleGrey
        , Events.onClick focusMsg
        ]
        [ row [ spacing 10, width fill ]
            [ Input.text
                [ htmlAttribute (Html.Attributes.id id)
                , Events.onFocus focusMsg
                ]
                { label = Input.labelAbove [ Font.size 18 ] (text label)
                , text = input
                , placeholder = Nothing
                , onChange = debouncedDelta 500
                }
            , statusToIcon status parsed
            ]
        , viewFeedback status feedback
        ]



-- d8888b.  .d8b.  d888888b d88888b
-- 88  `8D d8' `8b `~~88~~' 88'
-- 88   88 88ooo88    88    88ooooo
-- 88   88 88~~~88    88    88~~~~~
-- 88  .8D 88   88    88    88.
-- Y8888D' YP   YP    YP    Y88888P
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


type DateDelta
    = MonthsPageOpened
    | MonthSelected Time.Month
    | YearsPageOpened
    | DecadeSelected Int
    | YearSelected Int
    | DateSelected Date.Date
    | CalendarPageChanged Int


type alias DateState =
    { page : Page
    , viewing : Date.Date
    , selected : Maybe Date.Date
    }


type Page
    = CalendarPage
    | MonthsPage
    | YearsPage { decade : Int }


type CalendarBlock
    = Blank
    | Day Date.Date


date : String -> (Field.Delta DateDelta -> msg) -> Field.Field DateState DateDelta Date.Date (Element msg) msg
date label deltaMsg =
    Field.custom
        { init = { page = CalendarPage, viewing = Date.fromCalendarDate 2021 Time.Nov 1, selected = Nothing }
        , deltaMsg = deltaMsg
        , updater =
            \delta state ->
                case delta of
                    MonthsPageOpened ->
                        { state | page = MonthsPage }

                    MonthSelected month ->
                        { state
                            | page = CalendarPage
                            , viewing =
                                Date.fromCalendarDate
                                    (Date.year state.viewing)
                                    month
                                    (Date.day state.viewing)
                            , selected =
                                state.selected
                                    |> Maybe.map
                                        (\d ->
                                            Date.fromCalendarDate
                                                (Date.year d)
                                                month
                                                (Date.day d)
                                        )
                        }

                    YearsPageOpened ->
                        { state | page = YearsPage { decade = Date.year state.viewing // 10 } }

                    DecadeSelected decade ->
                        { state | page = YearsPage { decade = decade } }

                    YearSelected year ->
                        case state.page of
                            YearsPage { decade } ->
                                { state
                                    | page = CalendarPage
                                    , viewing =
                                        Date.fromCalendarDate
                                            (decade * 10 + year)
                                            (Date.month state.viewing)
                                            (Date.day state.viewing)
                                    , selected =
                                        state.selected
                                            |> Maybe.map
                                                (\d ->
                                                    Date.fromCalendarDate
                                                        (decade * 10 + year)
                                                        (Date.month d)
                                                        (Date.day d)
                                                )
                                }

                            _ ->
                                state

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

                    CalendarPageChanged i ->
                        { state | viewing = Date.add Date.Months i state.viewing }
        , parser = \state -> Result.fromMaybe (Field.fail "Must select a date") state.selected
        , renderer = renderDatePicker
        , label = label
        }


renderDatePicker : RendererConfig DateState DateDelta Date.Date msg -> Element msg
renderDatePicker { input, delta, label, focused, focusMsg, status, parsed, feedback } =
    let
        firstDayOfNextMonth =
            Date.add Date.Months 1 firstDayOfMonth

        firstDayOfMonth =
            Date.floor Date.Month input.viewing

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

        button attrs txt m =
            Input.button
                ([ Events.onFocus focusMsg
                 , width fill
                 , height <| px 44
                 , Border.width 1
                 , Border.rounded 3
                 , Border.color midGrey
                 , Background.color white
                 , padding 10
                 , Font.center
                 ]
                    ++ attrs
                )
                { label = text txt
                , onPress = Just (delta m)
                }

        selectedButton txt m =
            Input.button
                [ Events.onFocus focusMsg
                , width fill
                , height <| px 44
                , Border.width 1
                , Border.rounded 3
                , Border.color primaryColor
                , Background.color primaryColor
                , Font.color white
                , padding 10
                , Font.center
                ]
                { label = text txt
                , onPress = Just (delta m)
                }

        iconButton attrs i m =
            Input.button
                ([ Events.onFocus focusMsg
                 , width fill
                 , height <| px 44
                 , Border.width 1
                 , Border.rounded 3
                 , Border.color midGrey
                 , Background.color white
                 , padding 10
                 , Font.center
                 ]
                    ++ attrs
                )
                { label = i
                , onPress = Just (delta m)
                }
    in
    column
        [ width fill
        , height <| px 500
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
        , Border.width 1
        , Border.color paleGrey
        ]
        [ row [ width fill, spaceEvenly ] [ el [ Font.size 18 ] (text label), statusToIcon status parsed ]
        , case input.page of
            CalendarPage ->
                column [ spacing 10, width fill ]
                    [ row [ spacing 10, width <| maximum maxCalendarWidth <| fill, centerX ]
                        [ iconButton [ width <| px 44 ] (icon FI.arrowLeft) (CalendarPageChanged -1)
                        , iconButton [ width <| fillPortion 3 ] (row [ spacing 10, width fill ] [ el [] (text (Date.format "MMMM" input.viewing)), el [ alignRight ] (icon FI.chevronDown) ]) MonthsPageOpened
                        , iconButton [ width <| fillPortion 2 ] (row [ spacing 10, width fill ] [ el [] (text (Date.format "yyyy" input.viewing)), el [ alignRight ] (icon FI.chevronDown) ]) YearsPageOpened
                        , iconButton [ width <| px 44 ] (icon FI.arrowRight) (CalendarPageChanged 1)
                        ]
                    , row [ spacing 8, width <| maximum maxCalendarWidth <| fill, centerX ] headerRow
                    , column [ spacing 8, width <| maximum maxCalendarWidth <| fill, centerX ]
                        (List.map
                            (\w ->
                                row [ spacing 8, width fill ]
                                    (List.map (viewBlock delta focusMsg input.selected) w)
                            )
                            weeks
                        )
                    , viewFeedback status feedback
                    ]

            YearsPage { decade } ->
                column [ spacing 10, width fill ]
                    [ Element.Keyed.row [ width <| fillPortion 1, spacing 10 ]
                        (List.range (decade - 2) (decade + 2)
                            |> List.map
                                (\dec ->
                                    if dec == decade then
                                        ( String.fromInt dec, selectedButton (String.fromInt (dec * 10) ++ "s") (DecadeSelected dec) )

                                    else
                                        ( String.fromInt dec, button [] (String.fromInt (dec * 10) ++ "s") (DecadeSelected dec) )
                                )
                        )
                    , row [ width fill, spacing 10 ]
                        [ Element.Keyed.column [ width <| fillPortion 2, spacing 10 ]
                            (List.range 0 4 |> List.map (\yr -> ( String.fromInt decade, button [] (String.fromInt decade ++ String.fromInt yr) (YearSelected yr) )))
                        , Element.Keyed.column [ width <| fillPortion 2, spacing 10 ]
                            (List.range 5 9 |> List.map (\yr -> ( String.fromInt decade, button [] (String.fromInt decade ++ String.fromInt yr) (YearSelected yr) )))
                        ]
                    ]

            MonthsPage ->
                let
                    monthToString i =
                        case i of
                            1 ->
                                "January"

                            2 ->
                                "February"

                            3 ->
                                "March"

                            4 ->
                                "April"

                            5 ->
                                "May"

                            6 ->
                                "June"

                            7 ->
                                "July"

                            8 ->
                                "August"

                            9 ->
                                "September"

                            10 ->
                                "October"

                            11 ->
                                "November"

                            _ ->
                                "December"
                in
                row [ spacing 10, width fill ]
                    [ column [ width fill, spacing 10 ] (List.range 1 6 |> List.map (\m -> button [] (monthToString m) (MonthSelected (Date.numberToMonth m))))
                    , column [ width fill, spacing 10 ] (List.range 7 12 |> List.map (\m -> button [] (monthToString m) (MonthSelected (Date.numberToMonth m))))
                    ]
        ]


viewBlock : (DateDelta -> msg) -> msg -> Maybe Date.Date -> CalendarBlock -> Element msg
viewBlock msg focusMsg selected block =
    let
        colour d =
            if Just d == selected then
                { bg = primaryColor, bd = primaryColor, font = white }

            else
                { bg = white, bd = midGrey, font = black }
    in
    case block of
        Blank ->
            box transparent transparent transparent none

        Day d ->
            let
                { bg, bd, font } =
                    colour d
            in
            Input.button
                [ width <| maximum maxButtonSize <| fill
                , Events.onFocus focusMsg
                ]
                { label = box bg font bd (text <| String.fromInt <| Date.day d)
                , onPress = Just (msg (DateSelected d))
                }


headerRow : List (Element msg)
headerRow =
    [ box transparent black transparent (text "Mo")
    , box transparent black transparent (text "Tu")
    , box transparent black transparent (text "We")
    , box transparent black transparent (text "Th")
    , box transparent black transparent (text "Fr")
    , box transparent black transparent (text "Sa")
    , box transparent black transparent (text "Su")
    ]


box : Color -> Color -> Color -> Element msg -> Element msg
box bgColour fontColour borderColor content =
    el
        [ width <| px maxButtonSize
        , height <| px maxButtonSize
        , Background.color bgColour
        , alignLeft
        , Font.color fontColour
        , Border.color borderColor
        , Border.width 1
        , Border.rounded 3
        ]
        (el [ centerX, centerY ] content)


maxCalendarWidth : Int
maxCalendarWidth =
    (7 * maxButtonSize) + (6 * buttonSpacing)


maxButtonSize : Int
maxButtonSize =
    44


buttonSpacing : Int
buttonSpacing =
    8



-- d888888b d888888b .88b  d88. d88888b
-- `~~88~~'   `88'   88'YbdP`88 88'
--    88       88    88  88  88 88ooooo
--    88       88    88  88  88 88~~~~~
--    88      .88.   88  88  88 88.
--    YP    Y888888P YP  YP  YP Y88888P
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


type alias TimeState =
    { hours : Int, minutes : Int }


type TimeDelta
    = MinutesChanged Int
    | HoursChanged Int


time : String -> (Field.Delta TimeDelta -> msg) -> Field.Field TimeState TimeDelta TimeState (Element msg) msg
time label deltaMsg =
    Field.custom
        { init = { hours = 12, minutes = 0 }
        , deltaMsg = deltaMsg
        , updater =
            \delta state ->
                case delta of
                    HoursChanged diff ->
                        { state | hours = state.hours + diff |> modBy 24 }

                    MinutesChanged diff ->
                        { state | minutes = state.minutes + diff |> modBy 60 }
        , parser =
            \state ->
                if state.hours < 0 || state.hours > 23 then
                    Err (Field.fail "Hours must be between 0 & 23")

                else if state.minutes < 0 || state.minutes > 59 then
                    Err (Field.fail "Minutes must be between 0 & 59")

                else
                    Ok state
        , renderer = renderTimePicker
        , label = label
        }


renderTimePicker : RendererConfig TimeState TimeDelta TimeState msg -> Element msg
renderTimePicker { input, label, delta, focusMsg, focused, status, parsed } =
    let
        button txt m =
            Input.button
                [ width <| px 40
                , height <| px 40
                , Border.rounded 3
                , Border.width 1
                , Border.color midGrey
                , Background.color white
                ]
                { label = text txt, onPress = Just (delta m) }

        hours =
            String.fromInt input.hours
                |> String.padLeft 2 '0'
                |> String.split ""
                |> List.map (\x -> el [ width <| px 40, Font.size 30 ] <| text x)

        minutes =
            String.fromInt input.minutes
                |> String.padLeft 2 '0'
                |> String.split ""
                |> List.map (\x -> el [ width <| px 40, Font.size 30 ] <| text x)
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
        , Border.width 1
        , Border.color paleGrey
        , Font.center
        ]
        [ row [ width fill, spaceEvenly ] [ el [ Font.size 18 ] (text label), statusToIcon status parsed ]
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



-- .d8888. d88888b  .d8b.  d8888b.  .o88b. db   db
-- 88'  YP 88'     d8' `8b 88  `8D d8P  Y8 88   88
-- `8bo.   88ooooo 88ooo88 88oobY' 8P      88ooo88
--   `Y8b. 88~~~~~ 88~~~88 88`8b   8b      88~~~88
-- db   8D 88.     88   88 88 `88. Y8b  d8 88   88
-- `8888Y' Y88888P YP   YP 88   YD  `Y88P' YP   YP
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


type SearchDelta a
    = SearchChanged String
    | ItemsLoaded (List a)
    | ResultSelected a


type alias SearchState a =
    { search : String
    , options : List a
    , selected : Maybe a
    }


search :
    String
    -> (Field.Delta (SearchDelta a) -> msg)
    -> (a -> String)
    -> (SearchState a -> Cmd (List a))
    -> Field.Field (SearchState a) (SearchDelta a) a (Element msg) msg
search label msg toString loadItemsCmd =
    Field.custom
        { init = { search = "", options = [], selected = Nothing }
        , deltaMsg = msg
        , updater =
            \delta state ->
                case delta of
                    SearchChanged str ->
                        { state | search = str }

                    ItemsLoaded list ->
                        { state
                            | options = list
                            , selected =
                                state.selected
                                    |> Maybe.andThen
                                        (\sel ->
                                            if List.member sel list then
                                                Just sel

                                            else
                                                Nothing
                                        )
                        }

                    ResultSelected item ->
                        { state | selected = Just item }
        , parser =
            \state ->
                Result.fromMaybe (Field.fail "Must select something") state.selected
        , renderer = renderSearchField toString
        , label = label
        }
        |> Field.withCmd "loadItems" ItemsLoaded loadItemsCmd


renderSearchField :
    (a -> String)
    -> Field.RendererConfig (SearchState a) (SearchDelta a) a msg
    -> Element msg
renderSearchField toString { label, input, delta, debouncedEffectfulDelta, focusMsg, focused, parsed, status, feedback } =
    column
        [ height <| px 400
        , spacing 10
        , padding 20
        , Events.onClick focusMsg
        , Background.color
            (if focused then
                focusedBlue

             else
                white
            )
        , Border.rounded 5
        , Border.width 1
        , Border.color paleGrey
        ]
        [ row [ spacing 10 ]
            [ Input.text []
                { onChange = debouncedEffectfulDelta 1000 "loadItems" << SearchChanged
                , placeholder = Nothing
                , label = Input.labelAbove [] (text label)
                , text = input.search
                }
            , statusToIcon status parsed
            ]
        , case input.options of
            [] ->
                el [ centerX, centerY, Font.color midGrey ]
                    (text
                        (if status /= Field.Intact then
                            "[ no results found ]"

                         else
                            "[ start typing to search ]"
                        )
                    )

            _ ->
                column [ spacing 10, width fill ] <|
                    List.map
                        (\item ->
                            Input.button
                                [ Background.color
                                    (if Just item == input.selected then
                                        primaryColor

                                     else
                                        transparent
                                    )
                                , Font.color
                                    (if Just item == input.selected then
                                        white

                                     else
                                        black
                                    )
                                , Border.width 1
                                , Border.rounded 3
                                , Border.color
                                    (if Just item == input.selected then
                                        primaryColor

                                     else
                                        midGrey
                                    )
                                , padding 10
                                , width fill
                                ]
                                { label = text (toString item)
                                , onPress = Just (delta (ResultSelected item))
                                }
                        )
                        input.options
        , viewFeedback status feedback
        ]



-- d88888b d888888b d8888b.  .d88b.  d8b   db  .d8b.   .o88b.  .o88b. d888888b
-- 88'       `88'   88  `8D .8P  Y8. 888o  88 d8' `8b d8P  Y8 d8P  Y8   `88'
-- 88ooo      88    88oooY' 88    88 88V8o 88 88ooo88 8P      8P         88
-- 88~~~      88    88~~~b. 88    88 88 V8o88 88~~~88 8b      8b         88
-- 88        .88.   88   8D `8b  d8' 88  V888 88   88 Y8b  d8 Y8b  d8   .88.
-- YP      Y888888P Y8888P'  `Y88P'  VP   V8P YP   YP  `Y88P'  `Y88P' Y888888P
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


fibonacci : (Field.Delta String -> msg) -> Field.Field String String Int (Element msg) msg
fibonacci deltaMsg =
    Field.custom
        { init = ""
        , deltaMsg = deltaMsg
        , updater = \delta _ -> delta
        , parser = \input -> String.toInt input |> Result.fromMaybe (Field.fail "Must be a whole number") |> Result.map fib
        , renderer =
            renderFibonacci
        , label = "What's your favourite Fibonacci number?"
        }


renderFibonacci : { input : String, status : Field.Status, parsed : Maybe Int, feedback : List Field.Feedback, delta : String -> msg, debouncedDelta : Float -> String -> msg, effectfulDelta : String -> String -> msg, debouncedEffectfulDelta : Float -> String -> String -> msg, focusMsg : msg, focused : Bool, label : String, id : String } -> Element msg
renderFibonacci { label, input, debouncedDelta, parsed, status, focusMsg, focused, feedback } =
    column
        [ width Element.fill
        , spacing 20
        , padding 20
        , Border.width 1
        , Border.rounded 5
        , Border.color paleGrey
        , Events.onClick focusMsg
        , Background.color
            (if focused then
                focusedBlue

             else
                white
            )
        ]
        [ row [ spaceEvenly, width fill ]
            [ Input.text
                [ padding 10
                , Border.width 1
                , Border.rounded 3
                , Border.color midGrey
                , width Element.fill
                , Background.color white
                ]
                { label = Input.labelAbove [] (paragraph [] [ text label ])
                , onChange = debouncedDelta 1000
                , placeholder = Nothing
                , text = input
                }
            , statusToIcon status parsed
            ]
        , el
            [ centerX
            , Font.size 50
            ]
            (text
                (case parsed of
                    Just n ->
                        String.fromInt n

                    Nothing ->
                        "?"
                )
            )
        , viewFeedback status feedback
        ]


fib : Int -> Int
fib x =
    case x of
        0 ->
            1

        1 ->
            1

        _ ->
            fib (x - 1) + fib (x - 2)



-- db   db d88888b db      d8888b. d88888b d8888b. .d8888.
-- 88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
-- 88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
-- 88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
-- 88   88 88.     88booo. 88      88.     88 `88. db   8D
-- YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


viewFeedback : Field.Status -> List Field.Feedback -> Element msg
viewFeedback status feedback =
    case status of
        Field.Idle ->
            case feedback of
                [] ->
                    none

                _ ->
                    column
                        [ Font.size 12
                        , spacing 5
                        , width fill
                        ]
                        (feedback
                            |> List.sortWith
                                (\f1 f2 ->
                                    let
                                        tagToInt t =
                                            case t of
                                                Field.Fail ->
                                                    0

                                                Field.Warn ->
                                                    1

                                                Field.Pass ->
                                                    2

                                                Field.Info ->
                                                    3
                                    in
                                    Basics.compare (tagToInt f1.tag) (tagToInt f2.tag)
                                )
                            |> List.map
                                (\f ->
                                    row [ spacing 5, width fill ]
                                        [ viewFeedbackIcon f.tag
                                        , el [] (text f.text)
                                        ]
                                )
                        )

        _ ->
            none


viewFeedbackIcon : Field.FeedbackTag -> Element msg
viewFeedbackIcon tag =
    case tag of
        Field.Info ->
            el [ Font.color infoColor ] (smallIcon FI.info)

        Field.Pass ->
            el [ Font.color passColor ] (smallIcon FI.checkCircle)

        Field.Fail ->
            el [ Font.color failColor ] (smallIcon FI.xCircle)

        Field.Warn ->
            el [ Font.color warnColor ] (smallIcon FI.alertTriangle)


statusToIcon : Field.Status -> Maybe value -> Element msg
statusToIcon status parsed =
    let
        ( c, i ) =
            case ( status, parsed ) of
                ( Field.Changing, _ ) ->
                    ( midGrey, FI.moreHorizontal )

                ( Field.Idle, Just _ ) ->
                    ( passColor, FI.checkCircle )

                ( Field.Idle, Nothing ) ->
                    ( failColor, FI.xCircle )

                ( Field.Intact, _ ) ->
                    ( midGrey, FI.helpCircle )

                ( Field.Loading, _ ) ->
                    ( midGrey, FI.save )

                ( Field.Parsing, _ ) ->
                    ( midGrey, FI.cpu )
    in
    el
        [ Font.color c
        , alignTop
        ]
        (icon i)


icon : FI.Icon -> Element msg
icon i =
    i
        |> FI.toHtml []
        |> html


smallIcon : FI.Icon -> Element msg
smallIcon i =
    i
        |> FI.withSize 16
        |> icon


passColor : Color
passColor =
    rgb255 92 184 92


failColor : Color
failColor =
    rgb255 217 83 79


infoColor : Color
infoColor =
    rgb255 91 192 222


warnColor : Color
warnColor =
    rgb255 255 193 7


primaryColor : Color
primaryColor =
    rgb255 2 117 216


focusedBlue : Color
focusedBlue =
    rgb255 245 245 245


midGrey : Color
midGrey =
    rgb255 150 150 150


paleGrey : Color
paleGrey =
    rgb255 200 200 200


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


transparent : Color
transparent =
    rgba255 255 255 255 0
