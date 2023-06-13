module Example exposing (main)

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
    ,    int : Int
    , float : Float
    , string : String
    , char : Char
    , bool : Bool
    , enum : Enum
    , maybe : Maybe Example2
    }


exampleControl =
    Control.record Example
            |> Control.field "Time" .time (timeControl Time.utc)
        |> Control.field "Int" .int Control.int
        |> Control.field "Float" .float Control.float
        |> Control.field "String" .string Control.string
        |> Control.field "Char" .char Control.char
        |> Control.field "Bool" .bool Control.bool
        |> Control.field "Enum" .enum (Control.enum ( "Red", Red ) ( "Green", Green ) [ ( "Blue", Blue ) ])
        |> Control.field "Maybe" .maybe (Control.maybe example2Control)
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
    , list : List Time.Posix
    , dict : Dict.Dict String Int
    , set : Set.Set String
    , array : Array.Array Int
    , counter : Int
    }


example2Control =
    Control.record Example2
        |> Control.field "Time" .time (timeControl Time.utc)
        |> Control.field "Wrapper" .wrapper (Control.wrapper { wrap = Wrapper, unwrap = \(Wrapper x) -> x } Control.int)
        |> Control.field "Tuple" .tuple (Control.tuple ( "Int", Control.int ) ( "String", Control.string ))
        |> Control.field "Triple" .triple (Control.triple ( "Int", Control.int ) ( "String", Control.string ) ( "Float", Control.float ))
        |> Control.field "Result" .result (Control.result Control.int Control.string)
        |> Control.field "List" .list (Control.list (timeControl Time.utc))
        |> Control.field "Dict" .dict (Control.dict ( "Key", Control.string ) ( "Value", Control.int ))
        |> Control.field "Set" .set (Control.set Control.string)
        |> Control.field "Array" .array (Control.array Control.int)
        |> Control.field "Counter" .counter counterControl
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
        , subscriptions = \_ -> Sub.none
        }


type TimeDelta
    = ClockTicked Time.Posix
    | InputUpdated String


timeControl tz =
    Control.create
        { initEmpty =
            ( { input = "", now = Time.millisToPosix 0 }
            , Cmd.none
            )
        , initWith =
            \posix ->
                ( { input = String.fromInt (Time.toHour tz posix) ++ ":" ++ String.fromInt (Time.toMinute tz posix)
                  , now = posix
                  }
                , Cmd.none
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
                                Err [ "Hours must be a number" ]

                            Just h_ ->
                                if h_ < 0 || h_ > 23 then
                                    Err [ "Hours must be between 0 and 23" ]

                                else
                                    case String.toInt m of
                                        Nothing ->
                                            Err [ "Minutes must be a number" ]

                                        Just m_ ->
                                            if m_ < 0 || m_ > 59 then
                                                Err [ "Minutes must be between 0 and 59" ]

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
                        Err [ "Format must be HH:MM" ]
        , subscriptions = \_ -> Time.every 1000 ClockTicked
        }



-- And here's how we can test it out:


main =
    Control.debug
        { control = exampleControl
        , title = "My Lovely Form"
        , outputToString = Debug.toString
        }
