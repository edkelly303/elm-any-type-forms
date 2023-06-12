module Example exposing (main)

import Array
import Browser
import Control
import Dict
import Html
import Html.Attributes
import Html.Events
import Set



-- Here's how we define a form for a complex Elm type


main :
    Program
        ()
        (Control.State
            ( Control.State String
            , ( Control.State String
              , ( Control.State String
                , ( Control.State String
                  , ( Control.State Bool
                    , ( Control.State Enum
                      , ( Control.State
                            ( Control.State ()
                            , ( Control.State
                                    ( Control.State
                                        ( Control.State ( Control.State String, Control.End )
                                        , ( Control.State ( Control.State String, ( Control.State String, Control.End ) )
                                          , ( Control.State ( Control.State String, ( Control.State String, ( Control.State String, Control.End ) ) )
                                            , ( Control.State ( Control.State ( Control.State String, Control.End ), ( Control.State ( Control.State String, Control.End ), Control.End ) )
                                              , ( Control.State (List (Control.State String))
                                                , ( Control.State ( Control.State (List (Control.State ( Control.State String, ( Control.State String, Control.End ) ))), Control.End )
                                                  , ( Control.State ( Control.State (List (Control.State String)), Control.End )
                                                    , ( Control.State ( Control.State (List (Control.State String)), Control.End )
                                                      , ( Control.State Int
                                                        , Control.End
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                    , Control.End
                                    )
                              , Control.End
                              )
                            )
                        , Control.End
                        )
                      )
                    )
                  )
                )
              )
            )
        )
        (Control.Delta
            ( Control.Delta String
            , ( Control.Delta String
              , ( Control.Delta String
                , ( Control.Delta String
                  , ( Control.Delta Bool
                    , ( Control.Delta Enum
                      , ( Control.Delta
                            ( Control.Delta ()
                            , ( Control.Delta
                                    ( Control.Delta
                                        ( Control.Delta ( Control.Delta String, Control.End )
                                        , ( Control.Delta ( Control.Delta String, ( Control.Delta String, Control.End ) )
                                          , ( Control.Delta ( Control.Delta String, ( Control.Delta String, ( Control.Delta String, Control.End ) ) )
                                            , ( Control.Delta ( Control.Delta ( Control.Delta String, Control.End ), ( Control.Delta ( Control.Delta String, Control.End ), Control.End ) )
                                              , ( Control.Delta (Control.ListDelta String)
                                                , ( Control.Delta ( Control.Delta (Control.ListDelta ( Control.Delta String, ( Control.Delta String, Control.End ) )), Control.End )
                                                  , ( Control.Delta ( Control.Delta (Control.ListDelta String), Control.End )
                                                    , ( Control.Delta ( Control.Delta (Control.ListDelta String), Control.End )
                                                      , ( Control.Delta CounterDelta
                                                        , Control.End
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                    , Control.End
                                    )
                              , Control.End
                              )
                            )
                        , Control.End
                        )
                      )
                    )
                  )
                )
              )
            )
        )
main =
    Control.toProgram "My Lovely Form" exampleControl


type alias Example =
    { int : Int
    , float : Float
    , string : String
    , char : Char
    , bool : Bool
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
        |> Control.field "enum" .enum (Control.enum ( "Red", Red ) ( "Green", Green ) [ ( "Blue", Blue ) ])
        |> Control.field "maybe" .maybe (Control.maybe example2Control)
        |> Control.end


type Enum
    = Red
    | Green
    | Blue


type alias Example2 =
    { wrapper : Wrapper Int
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
        { initEmpty = 0
        , initWith = identity
        , update =
            \delta state ->
                case delta of
                    Increment ->
                        state + 1

                    Decrement ->
                        state - 1
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
