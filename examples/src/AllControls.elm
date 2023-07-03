module AllControls exposing (main)

import Array
import Control
import Dict
import Html
import Html.Attributes
import Html.Events
import Process
import Set
import Task
import Time
import Time.Extra



-- Here's how we define a form for a complex Elm type


type alias Example =
    { time : Time.Posix
    , int : Int
    , float : Float
    , string : String
    , char : Char
    , bool : Bool
    , enum : Enum
    , maybe : Maybe Example2
    }


exampleControl =
    Control.record Example
        |> Control.field .time (timeControl Time.utc)
        |> Control.field .int Control.int
        |> Control.field .float Control.float
        |> Control.field .string
            (Control.string
                |> Control.failIf (String.isEmpty) "Mustn't be blank"
                |> Control.noteIf (not << String.isEmpty) "Nice work!"
            )
        |> Control.field .char Control.char
        |> Control.field .bool Control.bool
        |> Control.field .enum
            (Control.enum ( "Red", Red ) ( "Green", Green ) [ ( "Blue", Blue ) ]
                |> Control.htmlBefore (Html.h3 [] [ Html.text "This is a heading before a control" ])
                |> Control.htmlAfter (Html.p [] [ Html.text "This is some text after a primitive control" ])
            )
        |> Control.field .maybe
            (Control.maybe example2Control
                |> Control.htmlBefore (Html.h3 [] [ Html.text "This is a heading before a custom type" ])
                |> Control.htmlAfter (Html.p [] [ Html.text "This is some text after a custom type" ])
            )
        |> Control.end
        |> Control.htmlBefore (Html.h3 [] [ Html.text "This is a heading before a record" ])
        |> Control.htmlAfter (Html.p [] [ Html.text "This is some text after a record" ])


type Enum
    = Red
    | Green
    | Blue


type alias Example2 =
    { time : Time.Posix
    , id : Id Int
    , tuple : ( Int, String )
    , triple : ( Int, String, Float )
    , result : Result Int String
    , list : List Time.Posix
    , dict : Dict.Dict String Int
    , set : Set.Set String
    , array : Array.Array Int
    , counter : Int
    }


example2Control =
    Control.record Example2
        |> Control.field .time (timeControl Time.utc)
        |> Control.field .id (Control.wrapper { wrap = Id, unwrap = \(Id x) -> x } (Control.int |> Control.label "Id"))
        |> Control.field .tuple (Control.tuple Control.int Control.string)
        |> Control.field .triple (Control.triple Control.int Control.string Control.float)
        |> Control.field .result (Control.result Control.int Control.string)
        |> Control.field .list
            (Control.list (timeControl Time.utc)
                |> Control.htmlBefore (Html.h3 [] [ Html.text "This is a heading before a list" ])
                |> Control.htmlAfter (Html.p [] [ Html.text "This is some text after a list" ])
            )
        |> Control.field .dict (Control.dict Control.string Control.int)
        |> Control.field .set (Control.set Control.string)
        |> Control.field .array (Control.array Control.int)
        |> Control.field .counter counterControl
        |> Control.end


type Id a
    = Id a



-- The package includes basic controls for all the core Elm types (Control.int,
-- Control.float, Control.string, Control.list, Control.maybe, Control.tuple, etc.)
--
-- You can also make your own custom controls, with arbitrary state (i.e. model)
-- and delta (i.e. msg) types. Here's an example that may look a bit familiar:


type CounterDelta
    = Increment
    | Decrement


counterControl =
    Control.create
        { initEmpty = ( 0, Cmd.none )
        , initWith = \int -> ( int, Cmd.none )
        , update =
            \delta state ->
                ( case delta of
                    Increment ->
                        state + 1

                    Decrement ->
                        state - 1
                , Cmd.none
                )
        , view =
            \{ state, label, id, name } ->
                Html.div
                    [ Html.Attributes.id id
                    , Html.Attributes.name name
                    ]
                    [ Html.label [] [ Html.text label ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Decrement
                        ]
                        [ Html.text "-1" ]
                    , Html.text <| String.fromInt state
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Increment
                        ]
                        [ Html.text "+1" ]
                    ]
        , parse = Ok
        , subscriptions = \_ -> Sub.none
        , label = "Counter"
        }


type TimeDelta
    = ClockTicked Time.Posix
    | InputUpdated String


timeControl tz =
    Control.create
        { initEmpty =
            ( { input = "", now = Time.millisToPosix 0 }
            , Task.perform ClockTicked Time.now
            )
        , initWith =
            \posix ->
                ( { input = String.fromInt (Time.toHour tz posix) ++ ":" ++ String.fromInt (Time.toMinute tz posix)
                  , now = posix
                  }
                , Task.perform ClockTicked Time.now
                )
        , update =
            \delta state ->
                case delta of
                    ClockTicked posix ->
                        ( { state | now = posix }
                        , Cmd.none
                        )

                    InputUpdated str ->
                        ( { state | input = str }
                        , Cmd.none
                        )
        , view =
            \{ state, label, id, name, class } ->
                let
                    showTime toPart =
                        state.now
                            |> toPart tz
                            |> String.fromInt
                            |> String.padLeft 2 '0'
                in
                Html.div []
                    [ Html.label [ Html.Attributes.for id ] [ Html.text label ]
                    , Html.input
                        [ Html.Attributes.id id
                        , Html.Attributes.name name
                        , Html.Attributes.class class
                        , Html.Events.onInput InputUpdated
                        ]
                        [ Html.text state.input ]
                    , Html.small []
                        [ Html.text
                            ("(Current time is "
                                ++ showTime Time.toHour
                                ++ ":"
                                ++ showTime Time.toMinute
                                ++ ")"
                            )
                        ]
                    ]
        , parse =
            \{ input, now } ->
                case String.split ":" input of
                    [ h, m ] ->
                        case String.toInt h of
                            Nothing ->
                                Err [ "hours must be a number" ]

                            Just h_ ->
                                if h_ < 0 || h_ > 23 then
                                    Err [ "hours must be between 0 and 23" ]

                                else
                                    case String.toInt m of
                                        Nothing ->
                                            Err [ "minutes must be a number" ]

                                        Just m_ ->
                                            if m_ < 0 || m_ > 59 then
                                                Err [ "minutes must be between 0 and 59" ]

                                            else
                                                Ok
                                                    (Time.Extra.partsToPosix tz
                                                        { day = Time.toDay tz now
                                                        , month = Time.toMonth tz now
                                                        , year = Time.toYear tz now
                                                        , hour = h_
                                                        , minute = m_
                                                        , second = 0
                                                        , millisecond = 0
                                                        }
                                                    )

                    _ ->
                        Err [ "format must be HH:MM" ]
        , subscriptions = \_ -> Time.every 1000 ClockTicked
        , label = "Time"
        }



-- And here's how we can test it out:


main =
    Control.sandbox
        { control = exampleControl
        , outputToString = Debug.toString
        }
