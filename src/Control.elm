module Control exposing
    ( Control, Form, toForm
    , bool, int, float, string, enum, initWith
    , create
    , wrapper, tuple, maybe, list, dict
    , record, field, hiddenField, readOnlyField, end, layout
    , customType, tag0, tag1, tag2
    , failIf, flagIf, onFlag, flagListAt, debounce
    , State, Delta, ListDelta, End
    )

{-| About this library


# Creating a form

@docs Control, Form, toForm


# Simple controls

@docs bool, int, float, string, enum, initWith


# Creating a new control

@docs create


# Control combinators

@docs wrapper, tuple, maybe, list, dict


# Record combinators

@docs record, field, hiddenField, readOnlyField, end, layout


# Custom type combinators

@docs customType, tag0, tag1, tag2


# Validation

@docs failIf, flagIf, onFlag, flagListAt, debounce


# Form internals

@docs State, Delta, ListDelta, End

-}

import Dict
import Html
import Internal
import Path


{-| A `Control` is the basic unit from which forms are composed. This library
provides various example controls and combinators that you can use to build a
form for any arbitrarily complex Elm type.
-}
type alias Control state delta output =
    Internal.Control state delta output


{-| A `Form` is an interface containing functions that you can use in your
program to initialise, view, update and submit your form.
-}
type alias Form state delta output msg =
    Internal.Form state delta output msg


type alias ControlConfig state delta output =
    Internal.ControlConfig state delta output


{-| `State` is the form's equivalent of an Elm program's `Model`type. It is
used to manage the form's internal state.
-}
type alias State state =
    Internal.State state


{-| A `Delta` is the form's equivalent of an Elm program's `Msg` type. It is
used internally within a form to update its state.
-}
type alias Delta delta =
    Internal.Delta delta


{-| Under the hood, this library encodes complex Elm types (such as records and
custom types) as nested tuples, which are used in both the `State` and `Delta`
types for each `Control`. The `End` type is used to mark the end of a nested
tuple.

For example, the `State` for this Elm type:

    type alias User =
        { name : String, age : Int }

Could be encoded as:

    ( String, ( Int, End ) )

-}
type alias End =
    Internal.End


{-| Special `Delta` type used only by `list` controls
-}
type alias ListDelta delta =
    Internal.ListDelta delta


type alias ViewConfig state =
    Internal.ViewConfig state


{-| Convert a `Control` into a `Form` by providing:

1.  A name/title for the form
2.  A `Msg` variant for updating the form
3.  A `Msg` variant for submitting the form
4.  The `Control` itself

```
type alias Msg
    = FormUpdated (Delta Int)
    | FormSubmitted

myForm =
    toForm "My Form" FormUpdated FormSubmitted int
```

-}
toForm :
    String
    -> (Delta delta -> msg)
    -> msg
    -> Control state delta output
    -> Form state delta output msg
toForm =
    Internal.fromControl


{-| A control that produces a Boolean value (`True` or `False`). Renders as an HTML checkbox???
-}
bool : { trueLabel : String, falseLabel : String } -> Control Bool Bool Bool
bool { trueLabel, falseLabel } =
    Internal.bool trueLabel falseLabel


{-| A control that produces an integer. Renders as an HTML text input.
-}
int : Control String String Int
int =
    Internal.int


{-| A control that produces a floating-point number. Renders as an HTML text input.
-}
float : Control String String Float
float =
    Internal.float


{-| A control that produces a string. Renders as an HTML text input.
-}
string : Control String String String
string =
    Internal.string


{-| A control that produces a custom type where none of the
variants have any payload. Renders as an HTML radio input.

    type Colour
        = Red
        | Green
        | Blue

    colourControl : Control Colour Colour Colour
    colourControl =
        enum
            ( "Red", Red )
            [ ( "Green", Green )
            , ( "Blue", Blue )
            ]

-}
enum :
    ( String, enum )
    -> ( String, enum )
    -> List ( String, enum )
    -> Control enum enum enum
enum =
    Internal.enum


{-| A combinator that produces a `List` of controls of a given type.

    myListControl =
        list int

-}
list :
    Control state delta output
    -> Control (List (State state)) (ListDelta delta) (List output)
list =
    Internal.list


{-| A combinator that produces a `Dict` from controls of given key and value
types. The key type must be `comparable`.

    myDictControl =
        dict "Key" int "Value" string

-}
dict :
    String
    -> Control keyState keyDelta comparable
    -> String
    -> Control valueState valueDelta value
    ->
        Control
            ( State (List (State ( State keyState, ( State valueState, End ) ))), End )
            ( Delta (ListDelta ( Delta keyDelta, ( Delta valueDelta, End ) )), End )
            (Dict.Dict comparable value)
dict =
    Internal.dict


{-| A combinator that produces a `Maybe` of a control of a given type.

    myMaybeControl =
        maybe int

-}
maybe :
    Control state delta output
    ->
        Control
            ( State (), ( State ( State state, End ), End ) )
            ( Delta (), ( Delta ( Delta delta, End ), End ) )
            (Maybe output)
maybe =
    Internal.maybe


{-| A combinator that produces a tuple of two controls of given types.

    myTupleControl =
        tuple "First" string "Second" int

-}
tuple :
    String
    -> Control state1 delta1 output1
    -> String
    -> Control state2 delta2 output2
    ->
        Control
            ( State state1, ( State state2, End ) )
            ( Delta delta1, ( Delta delta2, End ) )
            ( output1, output2 )
tuple =
    Internal.tuple


{-| A combinator that produces a wrapper type around a given type.

    type Id
        = Id Int

    idControl =
        wrapper Id (\(Id i) -> i) int

-}
wrapper :
    (output -> wrapped)
    -> (wrapped -> output)
    -> Control state delta output
    -> Control ( State state, End ) ( Delta delta, End ) wrapped
wrapper =
    Internal.wrapper


{-| A combinator that produces a custom type.

    type MyCustomType
        = NoArgs
        | OneArg String
        | TwoArgs Int Float

    myCustomTypeControl =
        customType
            (\noArgs oneArg twoArgs tag ->
                case tag of
                    NoArgs ->
                        noArgs

                    OneArg arg1 ->
                        oneArg arg1

                    TwoArgs arg1 arg2 ->
                        TwoArgs arg1 arg2
            )
            |> tag0 "NoArgs" NoArgs
            |> tag1 "OneArg" OneArg string
            |> tag2 "TwoArgs" TwoArgs int float
            |> end

-}
customType :
    b
    ->
        Internal.Builder
            r
            { applyInputs : a15 -> a15
            , deltaAfter : End
            , deltaAfters : End
            , deltaBefore : a14 -> a14
            , deltaBefores : a13 -> a13
            , destructor : b
            , flagEmitter : a12 -> a12
            , errorCollector : a11 -> a11
            , fns : c -> d -> d
            , idleSetter : a10 -> a10
            , index : number
            , labels : List e
            , makeDeltaSetters : a9 -> a9
            , makeStateSetters : a8 -> a8
            , parser : a7 -> a7
            , debouncingReceiverCollector : a6 -> a6
            , stateAfter : End
            , stateAfters : End
            , stateBefore : a5 -> a5
            , stateBefores : a4 -> a4
            , stateInserter : a3 -> a3
            , states : f -> g -> g
            , toArgStates : a2 -> a2
            , updater : a1 -> a1
            , viewer : a -> a
            }
customType =
    Internal.customType


{-| Add a tag with no arguments to a custom type.

    type Unit
        = Unit

    unitControl =
        customType
            (\unit tag ->
                case tag of
                    Unit ->
                        unit
            )
            |> tag0 "Unit"
            |> end

-}
tag0 :
    String
    -> output8
    -> Internal.Builder rec { applyInputs : a18 -> state0 -> restStateSetters1 -> state1_1, debouncingReceiverCollector : a17 -> List Internal.Flag -> restFns7 -> restStates6 -> List Internal.Flag, deltaAfter : b, deltaAfters : d, deltaBefore : ( Internal.Delta delta10, a16 ) -> c7, deltaBefores : ( ( Internal.Delta delta10, a16 ) -> c7, a15 ) -> c6, destructor : e, errorCollector : a14 -> List Internal.Flag -> List ( String, String ) -> restFns6 -> restStates5 -> List ( String, String ), flagEmitter : a13 -> List Internal.Flag -> Int -> restFns5 -> restStates4 -> List Internal.Flag, fns : Path.Path -> ( { baseUpdate : Float -> Internal.Delta () -> Internal.State () -> ( Internal.State (), Cmd (Internal.Delta ()) ), childViews : Internal.ViewConfig () -> List (Html.Html (Internal.Delta ())), collectDebouncingReceivers : Internal.State () -> List Internal.Flag, collectErrors : Internal.State () -> List Internal.Flag -> List ( String, String ), delta : Internal.Delta (), emitFlags : Internal.State () -> List Internal.Flag, index : Int, init : Internal.State (), initialise : output8 -> Internal.State (), parse : Internal.State () -> Result (List ( String, String )) output8, path : Path.Path, receiverCount : List Internal.Flag, setAllIdle : Internal.State () -> Internal.State (), update : Internal.Delta () -> Internal.State () -> ( Internal.State (), Cmd (Internal.Delta ()) ), view : Internal.ViewConfig () -> Html.Html (Internal.Delta ()) }, a12 ) -> c5, idleSetter : a11 -> Int -> restFns4 -> restStates3 -> restStates3, index : Int, labels : List String, makeDeltaSetters : a10 -> befores1 -> afters1 -> next, makeStateSetters : a9 -> ((Int -> Int -> (Internal.End -> state3) -> Internal.End -> Internal.End -> Internal.State state3) -> Int -> Int -> (c4 -> c4) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ) -> ( Internal.State argState1, restInits ) -> Internal.State tagStates1) -> ( Internal.State argState1, restInits ) -> restFns3 -> restToArgStates -> befores -> afters -> restStateSetters, parser : a8 -> Result (List ( String, String )) output2 -> Int -> restFns2 -> restStates2 -> Result (List ( String, String )) output2, stateAfter : f, stateAfters : g, stateBefore : ( Maybe a19, a6 ) -> c3, stateBefores : ( ( Maybe a19, a6 ) -> c3, a5 ) -> c2, stateInserter : a4 -> Int -> Int -> (restArgStates -> tagStates) -> restMaybeArgStates -> restArgStates -> Internal.State tagStates, states : Path.Path -> ( Internal.State (), a3 ) -> c1, toArgStates : ( (output8 -> h) -> h, a2 ) -> c, updater : a1 -> { newCmds : List (Cmd recordDelta), newStates : restStates1 -> recordState0 } -> restFns1 -> restDeltaSetters -> restDeltas -> restStates1 -> { newCmds : List (Cmd recordDelta), newStates : recordState1 }, viewer : a -> Maybe (Html.Html delta1) -> List Internal.Flag -> Int -> restFns -> restSetters -> restStates -> Maybe (Html.Html delta1) }
    -> Internal.Builder rec { applyInputs : a18 -> (stateSetter1 -> state0) -> ( stateSetter1, restStateSetters1 ) -> state1_1, debouncingReceiverCollector : a17 -> List Internal.Flag -> ( Internal.ControlFns input6 state7 delta9 output7, restFns7 ) -> ( Internal.State state7, restStates6 ) -> List Internal.Flag, deltaAfter : ( Internal.Delta delta8, b ), deltaAfters : ( b, d ), deltaBefore : a16 -> c7, deltaBefores : a15 -> c6, destructor : e, errorCollector : a14 -> List Internal.Flag -> List ( String, String ) -> ( Internal.ControlFns input5 state6 delta7 output6, restFns6 ) -> ( Internal.State state6, restStates5 ) -> List ( String, String ), flagEmitter : a13 -> List Internal.Flag -> Int -> ( Internal.ControlFns input4 state5 delta6 output5, restFns5 ) -> ( Internal.State state5, restStates4 ) -> List Internal.Flag, fns : Path.Path -> a12 -> c5, idleSetter : a11 -> Int -> ( Internal.ControlFns input3 state4 delta5 output4, restFns4 ) -> ( Internal.State state4, restStates3 ) -> ( Internal.State state4, restStates3 ), index : Int, labels : List String, makeDeltaSetters : a10 -> ( ( value, after1 ) -> delta4, befores1 ) -> ( after1, afters1 ) -> ( value -> delta4, next ), makeStateSetters : a9 -> ((Int -> Int -> (Internal.End -> state3) -> Internal.End -> Internal.End -> Internal.State state3) -> Int -> Int -> (c4 -> c4) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ) -> ( Internal.State argState1, restInits ) -> Internal.State tagStates1) -> ( Internal.State argState1, restInits ) -> ( Internal.ControlFns fieldInput fieldState delta3 output3, restFns3 ) -> ( (fieldInput -> Internal.State tagStates1) -> stateSetter, restToArgStates ) -> ( ( Maybe (Internal.State fieldState), after ) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ), befores ) -> ( after, afters ) -> ( stateSetter, restStateSetters ), parser : a8 -> Result (List ( String, String )) output2 -> Int -> ( Internal.ControlFns input2 state2 delta2 output2, restFns2 ) -> ( Internal.State state2, restStates2 ) -> Result (List ( String, String )) output2, stateAfter : ( Maybe a7, f ), stateAfters : ( f, g ), stateBefore : a6 -> c3, stateBefores : a5 -> c2, stateInserter : a4 -> Int -> Int -> (( Internal.State argState, restArgStates ) -> tagStates) -> ( Maybe (Internal.State argState), restMaybeArgStates ) -> ( Internal.State argState, restArgStates ) -> Internal.State tagStates, states : Path.Path -> a3 -> c1, toArgStates : a2 -> c, updater : a1 -> { newCmds : List (Cmd recordDelta), newStates : ( Internal.State state1, restStates1 ) -> recordState0 } -> ( Internal.ControlFns input1 state1 delta output1, restFns1 ) -> ( Internal.Delta delta -> recordDelta, restDeltaSetters ) -> ( Internal.Delta delta, restDeltas ) -> ( Internal.State state1, restStates1 ) -> { newCmds : List (Cmd recordDelta), newStates : recordState1 }, viewer : a -> Maybe (Html.Html delta1) -> List Internal.Flag -> Int -> ( Internal.ControlFns input state delta0 output, restFns ) -> ( Internal.Delta delta0 -> delta1, restSetters ) -> ( Internal.State state, restStates ) -> Maybe (Html.Html delta1) }
tag0 =
    Internal.tag0


{-| Add a tag with one argument to a custom type.

    type alias MyResult =
        Result String Int

    myResultControl =
        customType
            (\ok err tag ->
                case tag of
                    Ok value ->
                        ok value

                    Err error ->
                        err error
            )
            |> tag1 "Ok" Ok int
            |> tag1 "Err" Err string
            |> end

-}
tag1 :
    String
    -> (output9 -> output8)
    -> Internal.AdvancedControl output9 state8 delta10 output9
    -> Internal.Builder r { applyInputs : a18 -> state0 -> restStateSetters1 -> state1_1, debouncingReceiverCollector : a17 -> List Internal.Flag -> restFns7 -> restStates6 -> List Internal.Flag, deltaAfter : d, deltaAfters : e, deltaBefore : ( Internal.Delta delta11, a16 ) -> c7, deltaBefores : ( ( Internal.Delta delta11, a16 ) -> c7, a15 ) -> c6, destructor : f, errorCollector : a14 -> List Internal.Flag -> List ( String, String ) -> restFns6 -> restStates5 -> List ( String, String ), flagEmitter : a13 -> List Internal.Flag -> Int -> restFns5 -> restStates4 -> List Internal.Flag, fns : Path.Path -> ( { baseUpdate : Float -> Internal.Delta ( Internal.Delta delta10, Internal.End ) -> Internal.State ( Internal.State state8, Internal.End ) -> ( Internal.State ( Internal.State state8, Internal.End ), Cmd (Internal.Delta ( Internal.Delta delta10, Internal.End )) ), childViews : Internal.ViewConfig ( Internal.State state8, Internal.End ) -> List (Html.Html (Internal.Delta ( Internal.Delta delta10, Internal.End ))), collectDebouncingReceivers : Internal.State ( Internal.State state8, Internal.End ) -> List Internal.Flag, collectErrors : Internal.State ( Internal.State state8, Internal.End ) -> List Internal.Flag -> List ( String, String ), delta : Internal.Delta ( Internal.Delta delta10, Internal.End ), emitFlags : Internal.State ( Internal.State state8, Internal.End ) -> List Internal.Flag, index : Int, init : Internal.State ( Internal.State state8, Internal.End ), initialise : ( output9, b ) -> Internal.State ( Internal.State state8, Internal.End ), parse : Internal.State ( Internal.State state8, Internal.End ) -> Result (List ( String, String )) output8, path : Path.Path, receiverCount : List Internal.Flag, setAllIdle : Internal.State ( Internal.State state8, Internal.End ) -> Internal.State ( Internal.State state8, Internal.End ), update : Internal.Delta ( Internal.Delta delta10, Internal.End ) -> Internal.State ( Internal.State state8, Internal.End ) -> ( Internal.State ( Internal.State state8, Internal.End ), Cmd (Internal.Delta ( Internal.Delta delta10, Internal.End )) ), view : Internal.ViewConfig ( Internal.State state8, Internal.End ) -> Html.Html (Internal.Delta ( Internal.Delta delta10, Internal.End )) }, a12 ) -> c5, idleSetter : a11 -> Int -> restFns4 -> restStates3 -> restStates3, index : Int, labels : List String, makeDeltaSetters : a10 -> befores1 -> afters1 -> next, makeStateSetters : a9 -> ((Int -> Int -> (Internal.End -> state3) -> Internal.End -> Internal.End -> Internal.State state3) -> Int -> Int -> (c4 -> c4) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ) -> ( Internal.State argState1, restInits ) -> Internal.State tagStates1) -> ( Internal.State argState1, restInits ) -> restFns3 -> restToArgStates -> befores -> afters -> restStateSetters, parser : a8 -> Result (List ( String, String )) output2 -> Int -> restFns2 -> restStates2 -> Result (List ( String, String )) output2, stateAfter : g, stateAfters : h, stateBefore : ( Maybe a19, a6 ) -> c3, stateBefores : ( ( Maybe a19, a6 ) -> c3, a5 ) -> c2, stateInserter : a4 -> Int -> Int -> (restArgStates -> tagStates) -> restMaybeArgStates -> restArgStates -> Internal.State tagStates, states : Path.Path -> ( Internal.State ( Internal.State state8, Internal.End ), a3 ) -> c1, toArgStates : ( (( i, Internal.End ) -> j) -> i -> j, a2 ) -> c, updater : a1 -> { newCmds : List (Cmd recordDelta), newStates : restStates1 -> recordState0 } -> restFns1 -> restDeltaSetters -> restDeltas -> restStates1 -> { newCmds : List (Cmd recordDelta), newStates : recordState1 }, viewer : a -> Maybe (Html.Html delta1) -> List Internal.Flag -> Int -> restFns -> restSetters -> restStates -> Maybe (Html.Html delta1) }
    -> Internal.Builder r { applyInputs : a18 -> (stateSetter1 -> state0) -> ( stateSetter1, restStateSetters1 ) -> state1_1, debouncingReceiverCollector : a17 -> List Internal.Flag -> ( Internal.ControlFns input6 state7 delta9 output7, restFns7 ) -> ( Internal.State state7, restStates6 ) -> List Internal.Flag, deltaAfter : ( Internal.Delta delta8, d ), deltaAfters : ( d, e ), deltaBefore : a16 -> c7, deltaBefores : a15 -> c6, destructor : f, errorCollector : a14 -> List Internal.Flag -> List ( String, String ) -> ( Internal.ControlFns input5 state6 delta7 output6, restFns6 ) -> ( Internal.State state6, restStates5 ) -> List ( String, String ), flagEmitter : a13 -> List Internal.Flag -> Int -> ( Internal.ControlFns input4 state5 delta6 output5, restFns5 ) -> ( Internal.State state5, restStates4 ) -> List Internal.Flag, fns : Path.Path -> a12 -> c5, idleSetter : a11 -> Int -> ( Internal.ControlFns input3 state4 delta5 output4, restFns4 ) -> ( Internal.State state4, restStates3 ) -> ( Internal.State state4, restStates3 ), index : Int, labels : List String, makeDeltaSetters : a10 -> ( ( value, after1 ) -> delta4, befores1 ) -> ( after1, afters1 ) -> ( value -> delta4, next ), makeStateSetters : a9 -> ((Int -> Int -> (Internal.End -> state3) -> Internal.End -> Internal.End -> Internal.State state3) -> Int -> Int -> (c4 -> c4) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ) -> ( Internal.State argState1, restInits ) -> Internal.State tagStates1) -> ( Internal.State argState1, restInits ) -> ( Internal.ControlFns fieldInput fieldState delta3 output3, restFns3 ) -> ( (fieldInput -> Internal.State tagStates1) -> stateSetter, restToArgStates ) -> ( ( Maybe (Internal.State fieldState), after ) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ), befores ) -> ( after, afters ) -> ( stateSetter, restStateSetters ), parser : a8 -> Result (List ( String, String )) output2 -> Int -> ( Internal.ControlFns input2 state2 delta2 output2, restFns2 ) -> ( Internal.State state2, restStates2 ) -> Result (List ( String, String )) output2, stateAfter : ( Maybe a7, g ), stateAfters : ( g, h ), stateBefore : a6 -> c3, stateBefores : a5 -> c2, stateInserter : a4 -> Int -> Int -> (( Internal.State argState, restArgStates ) -> tagStates) -> ( Maybe (Internal.State argState), restMaybeArgStates ) -> ( Internal.State argState, restArgStates ) -> Internal.State tagStates, states : Path.Path -> a3 -> c1, toArgStates : a2 -> c, updater : a1 -> { newCmds : List (Cmd recordDelta), newStates : ( Internal.State state1, restStates1 ) -> recordState0 } -> ( Internal.ControlFns input1 state1 delta output1, restFns1 ) -> ( Internal.Delta delta -> recordDelta, restDeltaSetters ) -> ( Internal.Delta delta, restDeltas ) -> ( Internal.State state1, restStates1 ) -> { newCmds : List (Cmd recordDelta), newStates : recordState1 }, viewer : a -> Maybe (Html.Html delta1) -> List Internal.Flag -> Int -> ( Internal.ControlFns input state delta0 output, restFns ) -> ( Internal.Delta delta0 -> delta1, restSetters ) -> ( Internal.State state, restStates ) -> Maybe (Html.Html delta1) }
tag1 =
    Internal.tag1


{-| Add a tag with two arguments to a custom type.

    type Point
        = Point Float Float

    pointControl =
        customType
            (\point tag ->
                Point x y ->
                    point x y
            )
            |> tag2 Point ("X", float) ("Y", float)

-}
tag2 :
    String
    -> (output10 -> output9 -> output8)
    -> ( String, Internal.AdvancedControl output10 state9 delta11 output10 )
    -> ( String, Internal.AdvancedControl output9 state8 delta10 output9 )
    -> Internal.Builder r { applyInputs : a18 -> state0 -> restStateSetters1 -> state1_1, debouncingReceiverCollector : a17 -> List Internal.Flag -> restFns7 -> restStates6 -> List Internal.Flag, deltaAfter : d, deltaAfters : e, deltaBefore : ( Internal.Delta delta12, a16 ) -> c7, deltaBefores : ( ( Internal.Delta delta12, a16 ) -> c7, a15 ) -> c6, destructor : f, errorCollector : a14 -> List Internal.Flag -> List ( String, String ) -> restFns6 -> restStates5 -> List ( String, String ), flagEmitter : a13 -> List Internal.Flag -> Int -> restFns5 -> restStates4 -> List Internal.Flag, fns : Path.Path -> ( { baseUpdate : Float -> Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) ) -> Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> ( Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ), Cmd (Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) )) ), childViews : Internal.ViewConfig ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> List (Html.Html (Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) ))), collectDebouncingReceivers : Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> List Internal.Flag, collectErrors : Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> List Internal.Flag -> List ( String, String ), delta : Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) ), emitFlags : Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> List Internal.Flag, index : Int, init : Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ), initialise : ( output10, ( output9, b ) ) -> Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ), parse : Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> Result (List ( String, String )) output8, path : Path.Path, receiverCount : List Internal.Flag, setAllIdle : Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ), update : Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) ) -> Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> ( Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ), Cmd (Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) )) ), view : Internal.ViewConfig ( Internal.State state9, ( Internal.State state8, Internal.End ) ) -> Html.Html (Internal.Delta ( Internal.Delta delta11, ( Internal.Delta delta10, Internal.End ) )) }, a12 ) -> c5, idleSetter : a11 -> Int -> restFns4 -> restStates3 -> restStates3, index : Int, labels : List String, makeDeltaSetters : a10 -> befores1 -> afters1 -> next, makeStateSetters : a9 -> ((Int -> Int -> (Internal.End -> state3) -> Internal.End -> Internal.End -> Internal.State state3) -> Int -> Int -> (c4 -> c4) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ) -> ( Internal.State argState1, restInits ) -> Internal.State tagStates1) -> ( Internal.State argState1, restInits ) -> restFns3 -> restToArgStates -> befores -> afters -> restStateSetters, parser : a8 -> Result (List ( String, String )) output2 -> Int -> restFns2 -> restStates2 -> Result (List ( String, String )) output2, stateAfter : g, stateAfters : h, stateBefore : ( Maybe a19, a6 ) -> c3, stateBefores : ( ( Maybe a19, a6 ) -> c3, a5 ) -> c2, stateInserter : a4 -> Int -> Int -> (restArgStates -> tagStates) -> restMaybeArgStates -> restArgStates -> Internal.State tagStates, states : Path.Path -> ( Internal.State ( Internal.State state9, ( Internal.State state8, Internal.End ) ), a3 ) -> c1, toArgStates : ( (( i, ( j, Internal.End ) ) -> k) -> i -> j -> k, a2 ) -> c, updater : a1 -> { newCmds : List (Cmd recordDelta), newStates : restStates1 -> recordState0 } -> restFns1 -> restDeltaSetters -> restDeltas -> restStates1 -> { newCmds : List (Cmd recordDelta), newStates : recordState1 }, viewer : a -> Maybe (Html.Html delta1) -> List Internal.Flag -> Int -> restFns -> restSetters -> restStates -> Maybe (Html.Html delta1) }
    -> Internal.Builder r { applyInputs : a18 -> (stateSetter1 -> state0) -> ( stateSetter1, restStateSetters1 ) -> state1_1, debouncingReceiverCollector : a17 -> List Internal.Flag -> ( Internal.ControlFns input6 state7 delta9 output7, restFns7 ) -> ( Internal.State state7, restStates6 ) -> List Internal.Flag, deltaAfter : ( Internal.Delta delta8, d ), deltaAfters : ( d, e ), deltaBefore : a16 -> c7, deltaBefores : a15 -> c6, destructor : f, errorCollector : a14 -> List Internal.Flag -> List ( String, String ) -> ( Internal.ControlFns input5 state6 delta7 output6, restFns6 ) -> ( Internal.State state6, restStates5 ) -> List ( String, String ), flagEmitter : a13 -> List Internal.Flag -> Int -> ( Internal.ControlFns input4 state5 delta6 output5, restFns5 ) -> ( Internal.State state5, restStates4 ) -> List Internal.Flag, fns : Path.Path -> a12 -> c5, idleSetter : a11 -> Int -> ( Internal.ControlFns input3 state4 delta5 output4, restFns4 ) -> ( Internal.State state4, restStates3 ) -> ( Internal.State state4, restStates3 ), index : Int, labels : List String, makeDeltaSetters : a10 -> ( ( value, after1 ) -> delta4, befores1 ) -> ( after1, afters1 ) -> ( value -> delta4, next ), makeStateSetters : a9 -> ((Int -> Int -> (Internal.End -> state3) -> Internal.End -> Internal.End -> Internal.State state3) -> Int -> Int -> (c4 -> c4) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ) -> ( Internal.State argState1, restInits ) -> Internal.State tagStates1) -> ( Internal.State argState1, restInits ) -> ( Internal.ControlFns fieldInput fieldState delta3 output3, restFns3 ) -> ( (fieldInput -> Internal.State tagStates1) -> stateSetter, restToArgStates ) -> ( ( Maybe (Internal.State fieldState), after ) -> ( Maybe (Internal.State argState1), restMaybeArgStates1 ), befores ) -> ( after, afters ) -> ( stateSetter, restStateSetters ), parser : a8 -> Result (List ( String, String )) output2 -> Int -> ( Internal.ControlFns input2 state2 delta2 output2, restFns2 ) -> ( Internal.State state2, restStates2 ) -> Result (List ( String, String )) output2, stateAfter : ( Maybe a7, g ), stateAfters : ( g, h ), stateBefore : a6 -> c3, stateBefores : a5 -> c2, stateInserter : a4 -> Int -> Int -> (( Internal.State argState, restArgStates ) -> tagStates) -> ( Maybe (Internal.State argState), restMaybeArgStates ) -> ( Internal.State argState, restArgStates ) -> Internal.State tagStates, states : Path.Path -> a3 -> c1, toArgStates : a2 -> c, updater : a1 -> { newCmds : List (Cmd recordDelta), newStates : ( Internal.State state1, restStates1 ) -> recordState0 } -> ( Internal.ControlFns input1 state1 delta output1, restFns1 ) -> ( Internal.Delta delta -> recordDelta, restDeltaSetters ) -> ( Internal.Delta delta, restDeltas ) -> ( Internal.State state1, restStates1 ) -> { newCmds : List (Cmd recordDelta), newStates : recordState1 }, viewer : a -> Maybe (Html.Html delta1) -> List Internal.Flag -> Int -> ( Internal.ControlFns input state delta0 output, restFns ) -> ( Internal.Delta delta0 -> delta1, restSetters ) -> ( Internal.State state, restStates ) -> Maybe (Html.Html delta1) }
tag2 =
    Internal.tag2


{-| A combinator that produces a record type, or any 'product' type that needs to
be constructed from multiple controls. (For example, the `tuple` combinator in
this library is built using the `record` combinator).

    type alias MyRecord =
        { name : String
        , age : Int
        }

    myRecordControl =
        record
            (\name age ->
                { name = name
                , age = age
                }
            )
            |> field "Name" string
            |> field "Age" int
            |> end

-}
record :
    constructor
    ->
        Internal.Builder
            { after : Internal.End
            , afters : Internal.End
            , before : l -> l
            , befores : m -> m
            , debouncingReceiverCollector : q -> q
            , errorCollector : p -> p
            , fields : c -> d -> d
            , flagEmitter : o -> o
            , idleSetter : j -> j
            , index : Int
            , initialiser : k -> k
            , labels : List String
            , makeSetters : n -> n
            , parser : i -> i
            , states : e -> f -> f
            , toOutput : constructor
            , updater : g -> g
            , viewer : h -> h
            }
            cus
record =
    Internal.record


{-| Add a field to a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field "Hello" string
            |> end

-}
field :
    String
    -> c
    -> Internal.AdvancedControl input state delta output
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> ( { field : Internal.ControlFns input state delta output, fromInput : c, access : Internal.Access }, d ) -> e
            , states : Path.Path -> ( Internal.State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> k -> l -> m -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Internal.Delta w, x ) -> y
            , befores : ( ( Internal.Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Internal.Flag -> f1 -> g1 -> List Internal.Flag
            , errorCollector : h1 -> List Internal.Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Internal.Flag -> l1 -> m1 -> List Internal.Flag
            }
            n1
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> d -> e
            , states : Path.Path -> f -> g
            , updater : h -> { newStates : ( Internal.State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( Internal.RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Internal.Delta q1 -> recordDelta, restDeltaSetters ) -> ( Internal.Delta q1, restDeltas ) -> ( Internal.State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> ( Internal.RecordFns s1 t1 u1 v1 w1, k ) -> ( Internal.Delta u1 -> j, l ) -> ( Internal.State t1, m ) -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( Internal.RecordFns x1 y1 z1 output0 a2, o ) -> ( Internal.State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( Internal.RecordFns b2 c2 d2 e2 f2, r ) -> ( Internal.State c2, s ) -> ( Internal.State c2, s )
            , initialiser : t -> recordInput -> ( Internal.RecordFns g2 h2 i2 g2 recordInput, u ) -> ( Internal.State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Internal.Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Internal.Flag -> ( Internal.RecordFns l2 m2 n2 o2 p2, f1 ) -> ( Internal.State m2, g1 ) -> List Internal.Flag
            , errorCollector : h1 -> List Internal.Flag -> List ( String, String ) -> ( Internal.RecordFns q2 r2 s2 t2 u2, i1 ) -> ( Internal.State r2, j1 ) -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Internal.Flag -> ( Internal.RecordFns v2 w2 x2 y2 z2, l1 ) -> ( Internal.State w2, m1 ) -> List Internal.Flag
            }
            n1
field =
    Internal.field


{-| Add a read-only field to a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> readOnlyfield "Hello" string
            |> end

-}
readOnlyField :
    String
    -> c
    -> Internal.AdvancedControl input state delta output
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> ( { field : Internal.ControlFns input state delta output, fromInput : c, access : Internal.Access }, d ) -> e
            , states : Path.Path -> ( Internal.State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> k -> l -> m -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Internal.Delta w, x ) -> y
            , befores : ( ( Internal.Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Internal.Flag -> f1 -> g1 -> List Internal.Flag
            , errorCollector : h1 -> List Internal.Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Internal.Flag -> l1 -> m1 -> List Internal.Flag
            }
            n1
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> d -> e
            , states : Path.Path -> f -> g
            , updater : h -> { newStates : ( Internal.State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( Internal.RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Internal.Delta q1 -> recordDelta, restDeltaSetters ) -> ( Internal.Delta q1, restDeltas ) -> ( Internal.State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> ( Internal.RecordFns s1 t1 u1 v1 w1, k ) -> ( Internal.Delta u1 -> j, l ) -> ( Internal.State t1, m ) -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( Internal.RecordFns x1 y1 z1 output0 a2, o ) -> ( Internal.State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( Internal.RecordFns b2 c2 d2 e2 f2, r ) -> ( Internal.State c2, s ) -> ( Internal.State c2, s )
            , initialiser : t -> recordInput -> ( Internal.RecordFns g2 h2 i2 g2 recordInput, u ) -> ( Internal.State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Internal.Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Internal.Flag -> ( Internal.RecordFns l2 m2 n2 o2 p2, f1 ) -> ( Internal.State m2, g1 ) -> List Internal.Flag
            , errorCollector : h1 -> List Internal.Flag -> List ( String, String ) -> ( Internal.RecordFns q2 r2 s2 t2 u2, i1 ) -> ( Internal.State r2, j1 ) -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Internal.Flag -> ( Internal.RecordFns v2 w2 x2 y2 z2, l1 ) -> ( Internal.State w2, m1 ) -> List Internal.Flag
            }
            n1
readOnlyField =
    Internal.readOnlyField


{-| Add a hidden field to a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> hiddenField "Hello" string
            |> end

-}
hiddenField :
    String
    -> c
    -> Internal.AdvancedControl input state delta output
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> ( { field : Internal.ControlFns input state delta output, fromInput : c, access : Internal.Access }, d ) -> e
            , states : Path.Path -> ( Internal.State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> k -> l -> m -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Internal.Delta w, x ) -> y
            , befores : ( ( Internal.Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Internal.Flag -> f1 -> g1 -> List Internal.Flag
            , errorCollector : h1 -> List Internal.Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Internal.Flag -> l1 -> m1 -> List Internal.Flag
            }
            n1
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> d -> e
            , states : Path.Path -> f -> g
            , updater : h -> { newStates : ( Internal.State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( Internal.RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Internal.Delta q1 -> recordDelta, restDeltaSetters ) -> ( Internal.Delta q1, restDeltas ) -> ( Internal.State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> ( Internal.RecordFns s1 t1 u1 v1 w1, k ) -> ( Internal.Delta u1 -> j, l ) -> ( Internal.State t1, m ) -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( Internal.RecordFns x1 y1 z1 output0 a2, o ) -> ( Internal.State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( Internal.RecordFns b2 c2 d2 e2 f2, r ) -> ( Internal.State c2, s ) -> ( Internal.State c2, s )
            , initialiser : t -> recordInput -> ( Internal.RecordFns g2 h2 i2 g2 recordInput, u ) -> ( Internal.State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Internal.Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Internal.Flag -> ( Internal.RecordFns l2 m2 n2 o2 p2, f1 ) -> ( Internal.State m2, g1 ) -> List Internal.Flag
            , errorCollector : h1 -> List Internal.Flag -> List ( String, String ) -> ( Internal.RecordFns q2 r2 s2 t2 u2, i1 ) -> ( Internal.State r2, j1 ) -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Internal.Flag -> ( Internal.RecordFns v2 w2 x2 y2 z2, l1 ) -> ( Internal.State w2, m1 ) -> List Internal.Flag
            }
            n1
hiddenField =
    Internal.hiddenField


{-| Finalise the construction of a `record` or `customType` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field "Hello" string
            |> end

-}
end :
    Internal.Builder
        { d
            | afters : afters1
            , befores : Internal.End -> befores1
            , fields : Path.Path -> Internal.End -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 )
            , flagEmitter : (List Internal.Flag -> Internal.End -> Internal.End -> List Internal.Flag) -> List Internal.Flag -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( Internal.State state, restStates ) -> List Internal.Flag
            , errorCollector : (List Internal.Flag -> List ( String, String ) -> Internal.End -> Internal.End -> List ( String, String )) -> List Internal.Flag -> List ( String, String ) -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( Internal.State state, restStates ) -> List ( String, String )
            , idleSetter : (Internal.End -> Internal.End -> Internal.End) -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( Internal.State state, restStates ) -> ( Internal.State state, restStates )
            , initialiser : (input -> Internal.End -> Internal.End) -> input -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( Internal.State state, restStates )
            , labels : List String
            , makeSetters : (Internal.End -> Internal.End -> Internal.End) -> befores1 -> afters1 -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
            , parser : (Result (List ( String, String )) output -> Internal.End -> Internal.End -> Result (List ( String, String )) output) -> Result (List ( String, String )) output1_1 -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( Internal.State state, restStates ) -> Result (List ( String, String )) output
            , debouncingReceiverCollector : (List Internal.Flag -> Internal.End -> Internal.End -> List Internal.Flag) -> List Internal.Flag -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( Internal.State state, restStates ) -> List Internal.Flag
            , states : Path.Path -> Internal.End -> ( Internal.State state, restStates )
            , toOutput : output1_1
            , updater :
                ({ newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : Internal.End -> ( Internal.State state, restStates ) } -> Internal.End -> Internal.End -> Internal.End -> Internal.End -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : Internal.End -> ( Internal.State state, restStates ) })
                -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : ( Internal.State state, restStates ) -> ( Internal.State state, restStates ) }
                -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 )
                -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
                -> ( Internal.Delta delta, restDeltas )
                -> ( Internal.State state, restStates )
                -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : Internal.End -> ( Internal.State state, restStates ) }
            , viewer :
                (List (Html.Html (Internal.Delta ( Internal.Delta delta, restDeltas ))) -> List Internal.Flag -> Internal.End -> Internal.End -> Internal.End -> List (Html.Html (Internal.Delta ( Internal.Delta delta, restDeltas ))))
                -> List (Html.Html (Internal.Delta ( Internal.Delta delta, restDeltas )))
                -> List Internal.Flag
                -> ( Internal.RecordFns input2 state delta output2 recordOutput, restFns1 )
                -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
                -> ( Internal.State state, restStates )
                -> List (Html.Html (Internal.Delta ( Internal.Delta delta, restDeltas )))
        }
        { e
            | applyInputs : (state1 -> Internal.End -> state1) -> (stateSetter -> state0) -> ( stateSetter, restStateSetters ) -> input -> Internal.State ( Internal.State state, restStates )
            , deltaAfters : afters
            , deltaBefores : Internal.End -> befores
            , destructor : stateSetter -> state0
            , flagEmitter : (List Internal.Flag -> Int -> Internal.End -> Internal.End -> List Internal.Flag) -> List Internal.Flag -> Int -> ( Internal.ControlFns input1 state delta output1, restFns ) -> ( Internal.State state, restStates ) -> List Internal.Flag
            , errorCollector : (List Internal.Flag -> List ( String, String ) -> Internal.End -> Internal.End -> List ( String, String )) -> List Internal.Flag -> List ( String, String ) -> ( Internal.ControlFns input1 state delta output1, restFns ) -> ( Internal.State state, restStates ) -> List ( String, String )
            , fns : Path.Path -> Internal.End -> ( Internal.ControlFns input1 state delta output1, restFns )
            , idleSetter : (Int -> Internal.End -> Internal.End -> Internal.End) -> Int -> ( Internal.ControlFns input1 state delta output1, restFns ) -> ( Internal.State state, restStates ) -> ( Internal.State state, restStates )
            , labels : List String
            , makeDeltaSetters : (Internal.End -> Internal.End -> Internal.End) -> befores -> afters -> ( Internal.Delta delta -> ( Internal.Delta delta, restDeltas ), restSetters )
            , makeStateSetters : (a1 -> b1 -> Internal.End -> Internal.End -> Internal.End -> Internal.End -> Internal.End) -> c -> ( Internal.State state, restStates ) -> ( Internal.ControlFns input1 state delta output1, restFns ) -> f -> g -> h -> ( stateSetter, restStateSetters )
            , parser : (a -> b -> Internal.End -> Internal.End -> a) -> Result (List ( String, String )) value -> Int -> ( Internal.ControlFns input1 state delta output1, restFns ) -> ( Internal.State state, restStates ) -> Result (List ( String, String )) output
            , debouncingReceiverCollector : (List Internal.Flag -> Internal.End -> Internal.End -> List Internal.Flag) -> List Internal.Flag -> ( Internal.ControlFns input1 state delta output1, restFns ) -> ( Internal.State state, restStates ) -> List Internal.Flag
            , stateAfters : h
            , stateBefores : Internal.End -> g
            , stateInserter : c
            , states : Path.Path -> Internal.End -> ( Internal.State state, restStates )
            , toArgStates : Internal.End -> f
            , updater :
                ({ newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : Internal.End -> ( Internal.State state, restStates ) }
                 -> Internal.End
                 -> Internal.End
                 -> Internal.End
                 -> Internal.End
                 -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : Internal.End -> ( Internal.State state, restStates ) }
                )
                -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : ( Internal.State state, restStates ) -> ( Internal.State state, restStates ) }
                -> ( Internal.ControlFns input1 state delta output1, restFns )
                -> ( Internal.Delta delta -> ( Internal.Delta delta, restDeltas ), restSetters )
                -> ( Internal.Delta delta, restDeltas )
                -> ( Internal.State state, restStates )
                -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas )), newStates : Internal.End -> ( Internal.State state, restStates ) }
            , viewer :
                (Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
                 -> List Internal.Flag
                 -> Int
                 -> Internal.End
                 -> Internal.End
                 -> Internal.End
                 -> Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
                )
                -> Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
                -> List Internal.Flag
                -> Int
                -> ( Internal.ControlFns input1 state delta output1, restFns )
                -> ( Internal.Delta delta -> ( Internal.Delta delta, restDeltas ), restSetters )
                -> ( Internal.State state, restStates )
                -> Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
        }
    ->
        Internal.AdvancedControl
            input
            ( Internal.State state, restStates )
            ( Internal.Delta delta, restDeltas )
            output
end =
    Internal.end


{-| Create a simple control, with any arbitrary `State` and `Delta` types, and
producing any arbitrary output type.

Here's how we could create a control like the famous
[counter example](https://elm-lang.org/examples/buttons) from the Elm Guide

    type CounterDelta
        = Increment
        | Decrement

    counterControl =
        create
            { empty = 0
            , initialise = identity
            , parse = Ok
            , update =
                \delta state ->
                    case delta of
                        Increment ->
                            state + 1

                        Decrement ->
                            state - 1
            , view =
                \state ->
                    Html.div []
                        [ Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick Increment
                            ]
                            [ Html.text "+" ]
                        , Html.div [] [ Html.text <| String.fromInt state ]
                        , Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick Decrement
                            ]
                            [ Html.text "-" ]
                        ]
            }

-}
create : ControlConfig state delta output -> Control state delta output
create =
    Internal.makeControl


{-| Initialise a `Control` using a value of the type that the `Control` outputs.

    tuple "Int" int "String" string
        |> initWith ( 1, "hello" )

-}
initWith :
    output
    -> Control state delta output
    -> Control state delta output
initWith =
    Internal.initWith


{-| Debounce a `Control` for a number of milliseconds before it will display
any parsing or validation errors. This can be used to allow the user to finish
typing into a text input before we show them feedback.

    string
        |> debounce 500

-}
debounce :
    Float
    -> Control state delta output
    -> Control state delta output
debounce =
    Internal.debounce


{-| Display an error on a `Control` if its output fails to meet the predicate.

    int
        |> failIf
            (\int_ -> int_ < 1)
            "Integer must be greater than zero!"

-}
failIf :
    (output -> Bool)
    -> String
    -> Control state delta output
    -> Control state delta output
failIf =
    Internal.failIf


{-| Throw a "flag" from a `Control` if its output fails to meet the predicate.

This is meant to be used in combination with `onFlag`, which catches the "flag"
and displays an error.

This is particularly useful for multi-field validations, where you want to
display errors on one or more controls based on the combination of the
outputs of those controls.

    passwordControl =
        record
            (\password confirmation ->
                { password = password
                , confirmation = confirmation
                }
            )
            |> field "Enter password"
                (string
                    |> onFlag
                        "flag-passwords-do-not-match"
                        "The passwords do not match"
                )
            |> field "Confirm password"
                (string
                    |> onFlag
                        "flag-passwords-do-not-match"
                        "The passwords do not match"
                )
            |> end
            |> flagIf
                (\{ password, confirmation } -> password /= confirmation)
                "flag-passwords-do-not-match"

-}
flagIf :
    (output -> Bool)
    -> String
    -> Control state delta output
    -> Control state delta output
flagIf =
    Internal.flagIf


{-| Catch a "flag" thrown by another `Control` and display an error on this
control.

This is meant to be used in combination with `flagIf`, which throws the "flag".

-}
onFlag :
    String
    -> String
    -> Control state delta output
    -> Control state delta output
onFlag =
    Internal.onFlag


{-| Conditionally display an error on one or more items in a `list` control,
based on the output of the `list` control.

The following example will display errors on the first two items in a list of
strings, if and only if those first two items are "hello" and "world":

    myString =
        string
            |> onFlag
                "flag-hello-world"
                "The first two items in the list must not be \"hello\" and \"world\".")

    list myString
        |> flagListAt
            (\list_ ->
                case list of
                    "hello" :: "world" :: _ -> [ 0, 1 ]

                    _ -> []
            )
            "flag-hello-world"

-}
flagListAt :
    (output -> List Int)
    -> String
    -> Control (List state) (ListDelta delta) output
    -> Control (List state) (ListDelta delta) output
flagListAt =
    Internal.flagListAt


{-| Change the layout of the fields of a `record` control.

    type alias MyRecord =
        { name : String
        , age : Int
        }

    myRecordControl =
        record
            (\name age ->
                { name = name
                , age = age
                }
            )
            |> field "Name" string
            |> field "Age" int
            |> end
            |> layout (\fieldViews config -> Html.div [] fieldViews)

-}
layout :
    (List (Html.Html (Delta delta))
     -> ViewConfig state
     -> Html.Html (Delta delta)
    )
    -> Control state delta output
    -> Control state delta output
layout =
    Internal.layout
