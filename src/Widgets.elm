module Widgets exposing (..)

import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Field
import Html.Attributes
import Internals exposing (..)
import Time



-- d888888b d88888b db    db d888888b
-- `~~88~~' 88'     `8b  d8' `~~88~~'
--    88    88ooooo  `8bd8'     88
--    88    88~~~~~  .dPYb.     88
--    88    88.     .8P  Y8.    88
--    YP    Y88888P YP    YP    YP
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


string : String -> (Field.Delta String String msg -> msg) -> Field.Field String String String (Element msg) msg
string label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = Ok
        , renderer = renderTextField
        , label = label
        }
        |> Field.withDebounce 500


float : String -> (Field.Delta String String msg -> msg) -> Field.Field String String Float (Element msg) msg
float label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = String.toFloat >> Result.fromMaybe Field.NotValidFloat
        , renderer = renderTextField
        , label = label
        }
        |> Field.withDebounce 500


int : String -> (Field.Delta String String msg -> msg) -> Field.Field String String Int (Element msg) msg
int label msg =
    Field.custom
        { init = ""
        , deltaMsg = msg
        , updater = \delta _ -> delta
        , parser = String.toInt >> Result.fromMaybe Field.NotValidInt
        , renderer = renderTextField
        , label = label
        }
        |> Field.withDebounce 500


renderTextField :
    { focusMsg : msg
    , requestCmdMsg : msg
    , deltaMsg : Field.Delta String String msg -> msg
    , input : String
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , parsed : Result (List Field.Error) output
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
            , onChange = Field.Delta Field.Pure >> deltaMsg
            }
        , if typing then
            text "..."

          else
            none
        , viewErrors touched parsed typing
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
    = PageChanged Date.Unit Int
    | DateSelected Date.Date


type alias DateState =
    { page : Date.Date, selected : Maybe Date.Date }


type CalendarBlock
    = Blank
    | Day Date.Date


date : String -> (Field.Delta DateState DateDelta msg -> msg) -> Field.Field DateState DateDelta Date.Date (Element msg) msg
date label deltaMsg =
    Field.custom
        { init = { page = Date.fromCalendarDate 2021 Time.Nov 1, selected = Nothing }
        , deltaMsg = deltaMsg
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
        , parser = \state -> Result.fromMaybe Field.NoDateSelected state.selected
        , renderer = renderDatePicker
        , label = label
        }


renderDatePicker :
    { focusMsg : msg
    , requestCmdMsg : msg
    , input : { page : Date.Date, selected : Maybe Date.Date }
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , deltaMsg : Field.Delta DateState DateDelta msg -> msg
    , parsed : Result (List Field.Error) output
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
                , onPress = Just (deltaMsg (Field.Delta Field.Pure m))
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


viewBlock : (Field.Delta DateState DateDelta a -> a) -> a -> Maybe Date.Date -> CalendarBlock -> Element a
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
                , onPress = Just (msg (Field.Delta Field.Pure (DateSelected d)))
                }


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


time : String -> (Field.Delta String TimeDelta msg -> msg) -> Field.Field TimeState TimeDelta TimeState (Element msg) msg
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
                if state.hours >= 0 && state.hours < 24 && state.minutes >= 0 && state.minutes < 60 then
                    Ok state

                else
                    Err (Field.Custom "")
        , renderer = renderTimePicker
        , label = label
        }


renderTimePicker :
    { a
        | input : TimeState
        , id : String
        , label : Maybe String
        , deltaMsg : Field.Delta TimeState TimeDelta msg -> msg
        , focusMsg : msg
        , focused : Bool
    }
    -> Element msg
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
                { label = text txt, onPress = Just (deltaMsg (Field.Delta Field.Pure m)) }

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
    | ResultsLoaded (List a)
    | ResultSelected a


type alias SearchState a =
    { search : String
    , options : List a
    , selected : Maybe a
    }


search :
    String
    -> (Field.Delta (SearchState a) (SearchDelta a) msg -> msg)
    -> (a -> String)
    -> (SearchState a -> Cmd (List a))
    -> Field.Field (SearchState a) (SearchDelta a) a (Element msg) msg
search label msg toString loadCmd =
    Field.custom
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
        , parser =
            \state ->
                Result.fromMaybe (Field.Custom "Must select something") state.selected
        , renderer = renderSearchField toString
        , label = label
        }
        |> Field.withLoadCmd (loadCmd >> Cmd.map (ResultsLoaded >> Field.Delta Field.Housekeeping >> msg))
        |> Field.withDebounce 1000


renderSearchField :
    (a -> String)
    ->
        { focusMsg : msg
        , requestCmdMsg : msg
        , input : SearchState a
        , touched : Bool
        , focused : Bool
        , typing : Bool
        , deltaMsg : Field.Delta (SearchState a) (SearchDelta a) msg -> msg
        , parsed : Result (List Field.Error) output
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
                { onChange = deltaMsg << Field.Delta Field.Effectful << SearchChanged
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
                        , onPress = Just (deltaMsg <| Field.Delta Field.Pure <| ResultSelected item)
                        }
                )
                input.options
        , viewErrors touched parsed typing
        ]



-- db   db d88888b db      d8888b. d88888b d8888b. .d8888.
-- 88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
-- 88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
-- 88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
-- 88   88 88.     88booo. 88      88.     88 `88. db   8D
-- YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'
--
-- https://www.coolgenerator.com/ascii-text-generator / font = Basic


viewErrors : Bool -> Result (List Field.Error) output -> Bool -> Element msg
viewErrors touched parsed typing =
    case ( touched, parsed, typing ) of
        ( True, Err errs, False ) ->
            column [ Font.size 12, spacing 5, Font.color (rgb255 220 0 150) ] (List.map (Field.errorToString >> text) errs)

        _ ->
            none


green : Color
green =
    rgb255 100 255 100


focusedBlue : Color
focusedBlue =
    rgb255 220 255 220


midGrey : Color
midGrey =
    rgb255 150 150 150


white : Color
white =
    rgb255 255 255 255


transparent : Color
transparent =
    rgba255 255 255 255 0
