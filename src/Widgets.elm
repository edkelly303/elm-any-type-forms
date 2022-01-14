module Widgets exposing
    ( DateDelta
    , DateState
    , SearchDelta
    , SearchState
    , TimeDelta
    , TimeState
    , date
    , fibonacci
    , float
    , int
    , radioColumn
    , radioRow
    , search
    , string
    , time
    )

import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import FeatherIcons as FI
import Field exposing (ViewConfig)
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
        , updater = \delta _ -> ( delta, Cmd.none, [ Field.debounce 500 ] )
        , parser = Ok
        , renderer = renderTextField
        , label = label
        }


float : String -> (Field.Delta String -> msg) -> Field.Field String String Float (Element msg) msg
float label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> ( delta, Cmd.none, [ Field.debounce 500 ] )
        , parser = String.toFloat >> Result.fromMaybe (Field.fail "Must be a number")
        , renderer = renderTextField
        , label = label
        }


int : String -> (Field.Delta String -> msg) -> Field.Field String String Int (Element msg) msg
int label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> ( delta, Cmd.none, [ Field.debounce 500 ] )
        , parser = String.toInt >> Result.fromMaybe (Field.fail "Must be a whole number")
        , renderer = renderTextField
        , label = label
        }


renderTextField : Field.ViewConfig String String output msg -> Element msg
renderTextField { input, status, focusMsg, focused, delta, parsed, id, label } =
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
                , onChange = delta
                }
            , statusToIcon status parsed
            ]
        , viewFeedback status parsed
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
                        ( { state | page = MonthsPage }
                        , Cmd.none
                        , []
                        )

                    MonthSelected month ->
                        ( { state
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
                        , Cmd.none
                        , []
                        )

                    YearsPageOpened ->
                        ( { state | page = YearsPage { decade = Date.year state.viewing // 10 } }
                        , Cmd.none
                        , []
                        )

                    DecadeSelected decade ->
                        ( { state | page = YearsPage { decade = decade } }
                        , Cmd.none
                        , []
                        )

                    YearSelected year ->
                        case state.page of
                            YearsPage { decade } ->
                                ( { state
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
                                , Cmd.none
                                , []
                                )

                            _ ->
                                ( state
                                , Cmd.none
                                , []
                                )

                    DateSelected d ->
                        ( { state
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
                        , Cmd.none
                        , []
                        )

                    CalendarPageChanged i ->
                        ( { state | viewing = Date.add Date.Months i state.viewing }, Cmd.none, [] )
        , parser = \state -> Result.fromMaybe (Field.fail "Must select a date") state.selected
        , renderer = renderDatePicker
        , label = label
        }


renderDatePicker : Field.ViewConfig DateState DateDelta Date.Date msg -> Element msg
renderDatePicker { input, delta, label, focused, focusMsg, status, parsed } =
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
                    [ row [ spacing 10, width fill, centerX ]
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
                    , viewFeedback status parsed
                    ]

            YearsPage { decade } ->
                column [ spacing 10, width fill ]
                    [ row [ spacing 10, width fill, centerX ]
                        [ iconButton [ width <| px 44 ] (icon FI.arrowLeft) (CalendarPageChanged -1)
                        , iconButton [ width <| fillPortion 3 ] (row [ spacing 10, width fill ] [ el [] (text (Date.format "MMMM" input.viewing)), el [ alignRight ] (icon FI.chevronDown) ]) MonthsPageOpened
                        , iconButton [ width <| fillPortion 2 ] (row [ spacing 10, width fill ] [ el [] (text (Date.format "yyyy" input.viewing)), el [ alignRight ] (icon FI.chevronDown) ]) YearsPageOpened
                        , iconButton [ width <| px 44 ] (icon FI.arrowRight) (CalendarPageChanged 1)
                        ]
                    , Element.Keyed.row
                        [ width <| fillPortion 1, spacing 10, Font.color midGrey, Font.size 16 ]
                        (List.range (decade - 2) (decade + 2)
                            |> List.map
                                (\dec ->
                                    if dec == decade then
                                        ( String.fromInt dec, selectedButton (String.fromInt (dec * 10) ++ "s") (DecadeSelected dec) )

                                    else
                                        ( String.fromInt dec, button [ width fill, padding 10 ] (text <| String.fromInt (dec * 10) ++ "s") (delta <| DecadeSelected dec) )
                                )
                        )
                    , row [ width fill, spacing 10 ]
                        [ Element.Keyed.column [ width <| fillPortion 2, spacing 10 ]
                            (List.range 0 4 |> List.map (\yr -> ( String.fromInt decade, button [ width fill, padding 10 ] (text <| String.fromInt decade ++ String.fromInt yr) (delta <| YearSelected yr) )))
                        , Element.Keyed.column [ width <| fillPortion 2, spacing 10 ]
                            (List.range 5 9 |> List.map (\yr -> ( String.fromInt decade, button [ width fill, padding 10 ] (text <| String.fromInt decade ++ String.fromInt yr) (delta <| YearSelected yr) )))
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
                column [ width fill, spacing 10 ]
                    [ row [ spacing 10, width fill, centerX ]
                        [ iconButton [ width <| px 44 ] (icon FI.arrowLeft) (CalendarPageChanged -1)
                        , iconButton [ width <| fillPortion 3 ] (row [ spacing 10, width fill ] [ el [] (text (Date.format "MMMM" input.viewing)), el [ alignRight ] (icon FI.chevronDown) ]) MonthsPageOpened
                        , iconButton [ width <| fillPortion 2 ] (row [ spacing 10, width fill ] [ el [] (text (Date.format "yyyy" input.viewing)), el [ alignRight ] (icon FI.chevronDown) ]) YearsPageOpened
                        , iconButton [ width <| px 44 ] (icon FI.arrowRight) (CalendarPageChanged 1)
                        ]
                    , row [ spacing 10, width fill ]
                        [ column [ width fill, spacing 10 ] (List.range 1 6 |> List.map (\m -> button [ width fill, padding 10 ] (text <| monthToString m) (delta <| MonthSelected (Date.numberToMonth m))))
                        , column [ width fill, spacing 10 ] (List.range 7 12 |> List.map (\m -> button [ width fill, padding 10 ] (text <| monthToString m) (delta <| MonthSelected (Date.numberToMonth m))))
                        ]
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



-- d8888b.  .d8b.  d8888b. d888888b  .d88b.
-- 88  `8D d8' `8b 88  `8D   `88'   .8P  Y8.
-- 88oobY' 88ooo88 88   88    88    88    88
-- 88`8b   88~~~88 88   88    88    88    88
-- 88 `88. 88   88 88  .8D   .88.   `8b  d8'
-- 88   YD YP   YP Y8888D' Y888888P  `Y88P'
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


radioRow : String -> (Field.Delta delta -> msg) -> List ( delta, Element msg ) -> Field.Field (Maybe delta) delta delta (Element msg) msg
radioRow label deltaMsg options =
    Field.custom
        { init = Nothing
        , deltaMsg = deltaMsg
        , label = label
        , parser = Result.fromMaybe (Field.fail "Must select something")
        , renderer = radioRenderer wrappedRow options
        , updater =
            \delta input ->
                ( case input of
                    Nothing ->
                        Just delta

                    Just prev ->
                        if delta == prev then
                            Nothing

                        else
                            Just delta
                , Cmd.none
                , []
                )
        }


radioColumn :
    String
    -> (Field.Delta item -> msg)
    -> List ( item, Element msg )
    -> Field.Field (Maybe item) item item (Element msg) msg
radioColumn label deltaMsg options =
    radioRow label deltaMsg options
        |> Field.withView (radioRenderer column options)


radioRenderer :
    (List (Attribute msg) -> List (Element msg) -> Element msg)
    -> List ( item, Element msg )
    -> ViewConfig (Maybe item) item item msg
    -> Element msg
radioRenderer layout options { parsed, delta, label, status, focusMsg, focused } =
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
        [ row [ width fill, spaceEvenly ]
            [ el [ Font.size 18 ] (text label)
            , statusToIcon status parsed
            ]
        , layout [ spacing 10 ]
            (List.map
                (\( item, lbl ) ->
                    let
                        attrs =
                            case parsed of
                                Field.Passed output _ ->
                                    if output == item then
                                        [ Border.color primaryColor
                                        , Background.color primaryColor
                                        , Font.color white
                                        ]

                                    else
                                        []

                                _ ->
                                    []
                    in
                    button ([ width shrink, padding 10 ] ++ attrs) lbl (delta item)
                )
                options
            )
        , viewFeedback status parsed
        ]



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
                        ( { state | hours = state.hours + diff |> modBy 24 }, Cmd.none, [] )

                    MinutesChanged diff ->
                        ( { state | minutes = state.minutes + diff |> modBy 60 }, Cmd.none, [] )
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


button : List (Attribute msg) -> Element msg -> msg -> Element msg
button attrs label msg =
    Input.button
        ([ width <| px 40
         , height <| px 40
         , Border.rounded 3
         , Border.width 1
         , Border.color midGrey
         , Background.color white
         , Font.center
         , padding 10
         ]
            ++ attrs
        )
        { label = label, onPress = Just msg }


renderTimePicker : Field.ViewConfig TimeState TimeDelta TimeState msg -> Element msg
renderTimePicker { input, label, delta, focusMsg, focused, status, parsed } =
    let
        btn txt m =
            button [] (text txt) (delta m)

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
            [ btn "+" (HoursChanged 10)
            , btn "+" (HoursChanged 1)
            , el [ width <| px 40 ] none
            , btn "+" (MinutesChanged 10)
            , btn "+" (MinutesChanged 1)
            ]
        , row [ spacing 5, centerX ]
            (hours ++ (el [ width <| px 40 ] (text ":") :: minutes))
        , row [ spacing 5, centerX ]
            [ btn "-" (HoursChanged -10)
            , btn "-" (HoursChanged -1)
            , el [ width <| px 40 ] none
            , btn "-" (MinutesChanged -10)
            , btn "-" (MinutesChanged -1)
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
    | ItemsLoaded (Result (List a) (List a))
    | ResultSelected a


type alias SearchState a =
    { search : String
    , options : RemoteData a
    , selected : Maybe a
    , error : String
    }


type RemoteData a
    = NotAsked
    | Results (List a)


search :
    String
    -> (Field.Delta (SearchDelta a) -> msg)
    -> (a -> String)
    -> (SearchState a -> Cmd (Result (List a) (List a)))
    -> Field.Field (SearchState a) (SearchDelta a) a (Element msg) msg
search label msg toString loadItemsCmd =
    Field.custom
        { init = { search = "", options = NotAsked, selected = Nothing, error = "" }
        , deltaMsg = msg
        , updater =
            \delta state ->
                case delta of
                    SearchChanged str ->
                        let
                            newState =
                                { state | search = str }
                        in
                        ( newState
                        , Cmd.map ItemsLoaded (loadItemsCmd newState)
                        , [ Field.debounce 1000, Field.doNotValidate ]
                        )

                    ItemsLoaded (Ok list) ->
                        ( { state
                            | error = ""
                            , options = Results list
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
                        , Cmd.none
                        , []
                        )

                    ItemsLoaded (Err list) ->
                        ( { state
                            | error = "Loading failed!"
                            , options = Results list
                          }
                        , Cmd.none
                        , []
                        )

                    ResultSelected item ->
                        ( { state
                            | selected =
                                if state.selected == Just item then
                                    Nothing

                                else
                                    Just item
                          }
                        , Cmd.none
                        , []
                        )
        , parser =
            \state ->
                Result.fromMaybe (Field.fail "Must select something") state.selected
        , renderer = renderSearchField toString
        , label = label
        }


renderSearchField :
    (a -> String)
    -> Field.ViewConfig (SearchState a) (SearchDelta a) a msg
    -> Element msg
renderSearchField toString { label, input, delta, focusMsg, focused, parsed, status } =
    column
        [ height <| px 400
        , width fill
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
                { onChange = delta << SearchChanged
                , placeholder = Nothing
                , label = Input.labelAbove [] (text label)
                , text = input.search
                }
            , statusToIcon status parsed
            ]
        , if input.error /= "" then
            el [ centerX ] (text input.error)

          else
            none
        , case input.options of
            NotAsked ->
                el [ centerX, centerY, Font.color midGrey ]
                    (text "[ start typing to search ]")

            Results [] ->
                el [ centerX, centerY, Font.color midGrey ]
                    (text "[ no results found ]")

            Results list ->
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
                        list
        , viewFeedback status parsed
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
        , updater =
            \delta _ ->
                ( delta
                , Cmd.none
                , [ Field.debounce 500 ]
                )
        , parser = \input -> String.toInt input |> Result.fromMaybe (Field.fail "Must be a whole number") |> Result.map fib
        , renderer =
            renderFibonacci
        , label = "What's your favourite Fibonacci number?"
        }


renderFibonacci : Field.ViewConfig String String Int msg -> Element msg
renderFibonacci { label, input, delta, parsed, status, focusMsg, focused } =
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
                , onChange = delta
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
                    Field.Passed n _ ->
                        String.fromInt n

                    _ ->
                        "?"
                )
            )
        , viewFeedback status parsed
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


viewFeedback : Field.Status -> Field.ValidationStatus output -> Element msg
viewFeedback status parsed =
    case status of
        Field.Idle ->
            case parsed of
                Field.Intact ->
                    none

                Field.Passed _ [] ->
                    none

                Field.Passed _ feedback ->
                    viewFeedback2 feedback

                Field.Failed [] ->
                    none

                Field.Failed feedback ->
                    viewFeedback2 feedback

        _ ->
            none


viewFeedback2 : List Field.Feedback -> Element msg
viewFeedback2 feedback =
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


statusToIcon : Field.Status -> Field.ValidationStatus output -> Element msg
statusToIcon status parsed =
    let
        ( c, i ) =
            case ( status, parsed ) of
                ( Field.Debouncing, _ ) ->
                    ( midGrey, FI.moreHorizontal )

                ( Field.Idle, Field.Intact ) ->
                    ( midGrey, FI.helpCircle )

                ( Field.Idle, Field.Passed _ _ ) ->
                    ( passColor, FI.checkCircle )

                ( Field.Idle, Field.Failed _ ) ->
                    ( failColor, FI.xCircle )

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
