module Example exposing (main)

import Array
import Browser
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


main =
    Control.toProgram "My Lovely Form" exampleControl


type alias Example =
    { int : Int
    , float : Float
    , string : String
    , char : Char
    , bool : Bool
    , checkbox : Bool
    , enum : Enum
    , maybe : Maybe Example2
    }


exampleControl =
    Control.record Example
        |> Control.field "int" .int Control.int
        |> Control.field "float" .float Control.float
        |> Control.field "string" .string Control.string
        |> Control.field "char" .char Control.char
        |> Control.field "bool" .bool (Control.bool "true" "false")
        |> Control.field "checkbox" .checkbox Control.checkbox
        |> Control.field "enum" .enum (Control.enum ( "Red", Red ) ( "Green", Green ) [ ( "Blue", Blue ) ])
        |> Control.field "maybe" .maybe (Control.maybe example2Control)
        |> Control.end


type Enum
    = Red
    | Green
    | Blue


type alias Example2 =
    { time : Time.Posix
    , wrapper : Wrapper Int
    , tuple : ( Int, String )
    , triple : ( Int, String, Float )
    , result : Result Int String
    , list : List Int
    , dict : Dict.Dict String Int
    , set : Set.Set String
    , array : Array.Array Int
    , counter : Int
    }


example2Control =
    Control.record Example2
        |> Control.field "time" .time timeControl
        |> Control.field "wrapper" .wrapper (Control.wrapper { label = "wrapper", wrap = Wrapper, unwrap = \(Wrapper x) -> x } Control.int)
        |> Control.field "tuple" .tuple (Control.tuple ( "int", Control.int ) ( "string", Control.string ))
        |> Control.field "triple" .triple (Control.triple ( "int", Control.int ) ( "string", Control.string ) ( "float", Control.float ))
        |> Control.field "result" .result (Control.result Control.int Control.string)
        |> Control.field "list" .list (Control.list Control.int)
        |> Control.field "dict" .dict (Control.dict ( "key", Control.string ) ( "value", Control.int ))
        |> Control.field "set" .set (Control.set Control.string)
        |> Control.field "array" .array (Control.array Control.int)
        |> Control.field "counter" .counter counterControl
        |> Control.end


type Wrapper a
    = Wrapper a



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
                        , Html.Events.onClick Increment
                        ]
                        [ Html.text "+1" ]
                    , Html.text <| String.fromInt state
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Decrement
                        ]
                        [ Html.text "-1" ]
                    ]
        , parse = Ok
        }


type TimeDelta
    = ClockTicked Time.Posix
    | InputUpdated String


timeControl =
    Control.create
        { initEmpty =
            ( { input = "", now = Time.millisToPosix 0 }
            , Task.perform ClockTicked Time.now
            )
        , initWith =
            \posix ->
                ( { input = String.fromInt (Time.toHour Time.utc posix) ++ ":" ++ String.fromInt (Time.toMinute Time.utc posix)
                  , now = posix
                  }
                , Task.perform ClockTicked Time.now
                )
        , update =
            \delta state ->
                case delta of
                    ClockTicked posix ->
                        ( { state | now = posix }
                        , Task.perform ClockTicked (Process.sleep 1000 |> Task.andThen (\() -> Time.now))
                        )

                    InputUpdated str ->
                        ( { state | input = str }
                        , Task.perform ClockTicked (Process.sleep 1000 |> Task.andThen (\() -> Time.now))
                        )
        , view =
            \{ state, label, id, name, class } ->
                let
                    showTime toPart =
                        state.now
                            |> toPart Time.utc
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
                                ++ ":"
                                ++ showTime Time.toSecond
                                ++ ")"
                            )
                        ]
                    ]
        , parse =
            \{ input, now } ->
                case String.split ":" input of
                    [ h, m ] ->
                        Maybe.map2
                            (\h_ m_ ->
                                Time.Extra.partsToPosix Time.utc
                                    { day = Time.toDay Time.utc now
                                    , month = Time.toMonth Time.utc now
                                    , year = Time.toYear Time.utc now
                                    , hour = h_
                                    , minute = m_
                                    , second = 0
                                    , millisecond = 0
                                    }
                            )
                            (String.toInt h)
                            (String.toInt m)
                            |> Result.fromMaybe [ "Not a valid time" ]

                    _ ->
                        Err [ "Not a valid time" ]
        }
