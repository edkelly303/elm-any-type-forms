module Control exposing
    ( Control, Form, toProgram, toForm
    , bool, checkbox, int, float, string, char, enum
    , wrapper, tuple, triple, maybe, result, list, dict, set, array
    , ControlConfig, create
    , failIf
    , throwFlagIf, catchFlag
    , throwFlagsAt
    , initWith, debounce, id, name, label, class, classList
    , record, field, hiddenField, readOnlyField, end, layout
    , customType, tag0, tag1, tag2, tag3, tag4, tag5
    , State, Delta, ListDelta, End
    , Access, AdvancedControl, Builder, ControlFns, Flag, RecordFns, Status, ViewConfigStatic, ViewConfigDynamic, Path
    )

{-|


# Creating a form

@docs Control, Form, toProgram, toForm


# Basic controls

@docs bool, checkbox, int, float, string, char, enum


# Basic combinators

@docs wrapper, tuple, triple, maybe, result, list, dict, set, array


# Creating a new control

@docs ControlConfig, create


# Validating controls


## Simple validation

@docs failIf


## Multi-field validation

You may want display errors on one or more controls based on the validation of
another control higher up the tree.

Take the example of a password creation form. You want to validate that the
"Enter password" and "Confirm password" fields both contain the same string, and
show helpful error messages on both fields if they don't. You can achieve this
as follows:

    passwordControl =
        record
            (\password confirmation ->
                { password = password
                , confirmation = confirmation
                }
            )
            |> field "Enter password"
                (string
                    |> catchFlag
                        "flag-passwords-do-not-match"
                        "Must match 'Confirm password' field"
                )
            |> field "Confirm password"
                (string
                    |> catchFlag
                        "flag-passwords-do-not-match"
                        "Must match 'Enter password' field"
                )
            |> end
            |> throwFlagIf
                (\{ password, confirmation } -> password /= confirmation)
                "flag-passwords-do-not-match"

@docs throwFlagIf, catchFlag


## List validation

@docs throwFlagsAt


# Configuring controls

@docs initWith, debounce, id, name, label, class, classList


# Building record combinators

@docs record, field, hiddenField, readOnlyField, end, layout


# Building custom type combinators

@docs customType, tag0, tag1, tag2, tag3, tag4, tag5


# Form internals

These are types that you will see in your form's `State` and `Delta` type signatures.

@docs State, Delta, ListDelta, End


# Internal stuff that you can ignore

A user of this package shouldn't need to know about any of these types -
they are only exposed to make it possible to write type signatures.

@docs Access, AdvancedControl, Builder, ControlFns, Flag, RecordFns, Status, ViewConfigStatic, ViewConfigDynamic, Path

-}

import Array
import Browser
import Dict
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import List.Extra
import Path
import Process
import Set
import Task
import Time



{-
   d888888b db    db d8888b. d88888b .d8888.
   `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
      88     `8bd8'  88oodD' 88ooooo `8bo.
      88       88    88~~~   88~~~~~   `Y8b.
      88       88    88      88.     db   8D
      YP       YP    88      Y88888P `8888Y'
-}


{-| A `Form` is an interface containing functions that you can use in your
program to initialise, view, update and submit your form.
-}
type alias Form state delta output msg =
    { init : State state
    , initWith : output -> State state
    , update : Delta delta -> State state -> ( State state, Cmd msg )
    , view : State state -> Html msg
    , submit : State state -> ( State state, Result (List ( String, String )) output )
    }


{-| Configuration for a custom control. You need to supply the following fields:

  - `initEmpty`: a default initial "empty" value for the `state` of the control. For a control whose `state` is of type `String`, this might be `""`, for a number it might be `0`.
  - `initWith`: a function that initialises the control's `state` from a value of the control's `output` type. For a control that outputs an `Int`, but whose `state` is a `String`, this might be `String.fromInt`.
  - `update`: a function that updates the `state` of the control based on its existing `state` and a `delta` type that it receives. This is exactly analogous to an Elm program's update function, where `state` = `Model` and `delta` = `Msg`.
  - `view`: a function that outputs `Html delta` based on the `state` of the control. This is similar to an Elm program's view function, except instead of just taking the `state` as an argument, it takes a record containing the `state` plus the id, name, and label for the input.
  - `parse`: a function that attempts to parse the control's state into its output type, producing a result. If parsing fails, it can provide a list of errors.

-}
type alias ControlConfig state delta output =
    { initEmpty : state
    , initWith : output -> state
    , update : delta -> state -> state
    , view : { label : String, id : String, name : String, class : String, state : state } -> Html delta
    , parse : state -> Result (List String) output
    }


{-| A `Control` is the basic unit from which forms are composed. This library
provides various example controls and combinators that you can use to build a
form for any arbitrarily complex Elm type.
-}
type alias Control state delta output =
    AdvancedControl output state delta output


{-| A slightly more flexible version of `Control`
-}
type AdvancedControl input state delta output
    = Control (Path -> ControlFns input state delta output)


{-| Some internal stuff needed to build up a record control
-}
type alias RecordFns input state delta output recordOutput =
    { access : Access
    , field : ControlFns input state delta output
    , fromInput : recordOutput -> output
    }


{-| Some internal stuff needed to build up record and custom type controls
-}
type alias ControlFns input state delta output =
    { index : Int
    , init : State state
    , initWith : input -> State state
    , baseUpdate : Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
    , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
    , view : ViewConfigStatic -> ViewConfigDynamic state -> Html (Delta delta)
    , childViews : ViewConfigStatic -> ViewConfigDynamic state -> List (Html (Delta delta))
    , parse : State state -> Result (List ( String, String )) output
    , path : Path
    , emitFlags : State state -> List Flag
    , collectDebouncingReceivers : State state -> List Flag
    , collectErrors : State state -> List Flag -> List ( String, String )
    , receiverCount : Int
    , setAllIdle : State state -> State state
    , label : Maybe String
    , id : Maybe String
    , name : Maybe String
    , class : List String
    }


{-| Some internal stuff needed to handle validations
-}
type Flag
    = FlagLabel String
    | FlagPath Path Int
    | FlagList Path String (List Int)


{-| Some internal stuff needed to view controls
-}
type alias ViewConfigDynamic state =
    { state : state
    , status : Status
    , flags : List Flag
    , selected : Int
    }


{-| Some internal stuff needed to view controls
-}
type alias ViewConfigStatic =
    { name : Maybe String
    , id : Maybe String
    , label : Maybe String
    , class : List String
    }


{-| an internal type needed to represent the validation state of a control
-}
type Status
    = Intact
    | Debouncing
    | Idle (List (Result ( String, String ) ( String, String )))


{-| an internal type needed to track the position of a control within a tree of
controls
-}
type alias Path =
    Path.Path



{-
   .d8888. d888888b  .d8b.  d888888b d88888b
   88'  YP `~~88~~' d8' `8b `~~88~~' 88'
   `8bo.      88    88ooo88    88    88ooooo
     `Y8b.    88    88~~~88    88    88~~~~~
   db   8D    88    88   88    88    88.
   `8888Y'    YP    YP   YP    YP    Y88888P
-}


{-| `State` is the form's equivalent of an Elm program's `Model` type. It is
used to manage the form's internal state.
-}
type State state
    = State InternalState state


type alias InternalState =
    { status : InternalStatus
    , selected : Int
    }


type InternalStatus
    = Intact_
    | DebouncingSince Time.Posix
    | Idle_



{-
   d8888b. d88888b db      d888888b  .d8b.
   88  `8D 88'     88      `~~88~~' d8' `8b
   88   88 88ooooo 88         88    88ooo88
   88   88 88~~~~~ 88         88    88~~~88
   88  .8D 88.     88booo.    88    88   88
   Y8888D' Y88888P Y88888P    YP    YP   YP
-}


{-| A `Delta` is the form's equivalent of an Elm program's `Msg` type. It is
used internally within a form to update its state.
-}
type Delta delta
    = Skip
    | ChangeState delta
    | StartDebouncing Time.Posix
    | CheckDebouncer Time.Posix
    | TagSelected Int


{-| Special `Delta` type used only by `list` controls
-}
type ListDelta delta
    = InsertItem Int
    | DeleteItem Int
    | ChangeItem Int (Delta delta)



{-
   d88888b d8b   db d8888b.
   88'     888o  88 88  `8D
   88ooooo 88V8o 88 88   88
   88~~~~~ 88 V8o88 88   88
   88.     88  V888 88  .8D
   Y88888P VP   V8P Y8888D'
-}


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
type End
    = End



{-
   d88888b  .d88b.  d8888b. .88b  d88. .d8888.
   88'     .8P  Y8. 88  `8D 88'YbdP`88 88'  YP
   88ooo   88    88 88oobY' 88  88  88 `8bo.
   88~~~   88    88 88`8b   88  88  88   `Y8b.
   88      `8b  d8' 88 `88. 88  88  88 db   8D
   YP       `Y88P'  88   YD YP  YP  YP `8888Y'
-}


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
toForm : String -> (Delta delta -> msg) -> msg -> Control state delta output -> Form state delta output msg
toForm label_ toFormUpdatedMsg formSubmittedMsg (Control control) =
    let
        fns =
            control Path.root
    in
    { init = fns.init
    , initWith =
        \output ->
            let
                (Control initialisedControl) =
                    Control control
                        |> initWith output
            in
            initialisedControl Path.root
                |> .init
    , update =
        \msg state ->
            fns.update msg state
                |> Tuple.mapSecond (Cmd.map toFormUpdatedMsg)
    , view =
        \((State internalState state) as s) ->
            let
                emittedFlags =
                    fns.emitFlags s

                debouncingReceivers =
                    fns.collectDebouncingReceivers s

                flags =
                    List.filter (\f -> not <| List.member f debouncingReceivers) emittedFlags
            in
            H.form [ HE.onSubmit formSubmittedMsg ]
                [ H.h1 [] [ H.text label_ ]
                , fns.view
                    { id = fns.id
                    , name = fns.name
                    , label = fns.label
                    , class = fns.class
                    }
                    { state = state
                    , status = getStatus fns.parse fns.collectErrors flags (State internalState state)
                    , flags = flags
                    , selected = internalState.selected
                    }
                    |> H.map toFormUpdatedMsg
                , button formSubmittedMsg "Submit"
                ]
    , submit =
        \state ->
            let
                parsingResult =
                    fns.parse state

                validationErrors =
                    fns.emitFlags state
                        |> fns.collectErrors state
            in
            ( fns.setAllIdle state
            , case ( parsingResult, validationErrors ) of
                ( Ok output, [] ) ->
                    Ok output

                ( Ok _, vErrs ) ->
                    Err vErrs

                ( Err pErrs, vErrs ) ->
                    Err (pErrs ++ vErrs)
            )
    }


{-| Convert a `Control` into a `Program` by providing:

1.  A name/title for the form
2.  The `Control` itself

```
main =
    toProgram "My Form" int
```

This is useful when you're rapidly iterating on designing a form, and you don't yet want the hassle of plumbing it into
your Elm application's `Model`and `Msg` types.

Once you're happy with the form, you can then ask the Elm compiler (or your editor's Elm plugin) to tell you the type
signature of the `Program`, which will give you the form's `state` and `delta` types. You can then plug these into your
main `Model`and `Msg` types wherever appropriate.

-}
toProgram : String -> Control state delta output -> Program () (State state) (Delta delta)
toProgram label_ (Control control) =
    let
        fns =
            control Path.root
    in
    Browser.element
        { init =
            \() ->
                ( fns.init, Cmd.none )
        , update =
            \msg state ->
                fns.update msg state
        , view =
            \((State internalState state) as s) ->
                let
                    emittedFlags =
                        fns.emitFlags s

                    debouncingReceivers =
                        fns.collectDebouncingReceivers s

                    flags =
                        List.filter (\f -> not <| List.member f debouncingReceivers) emittedFlags
                in
                H.form []
                    [ H.h1 [] [ H.text label_ ]
                    , fns.view
                        { id = fns.id
                        , name = fns.name
                        , label = fns.label
                        , class = fns.class
                        }
                        { state = state
                        , status = getStatus fns.parse fns.collectErrors flags (State internalState state)
                        , flags = flags
                        , selected = internalState.selected
                        }
                    ]
        , subscriptions = \_ -> Sub.none
        }



{-
    .o88b.  .d88b.  d8b   db d888888b d8888b.  .d88b.  db      .d8888.
   d8P  Y8 .8P  Y8. 888o  88 `~~88~~' 88  `8D .8P  Y8. 88      88'  YP
   8P      88    88 88V8o 88    88    88oobY' 88    88 88      `8bo.
   8b      88    88 88 V8o88    88    88`8b   88    88 88        `Y8b.
   Y8b  d8 `8b  d8' 88  V888    88    88 `88. `8b  d8' 88booo. db   8D
    `Y88P'  `Y88P'  VP   V8P    YP    88   YD  `Y88P'  Y88888P `8888Y'
-}


{-| Create a simple control, with any arbitrary `state` and `delta` types, and
producing any arbitrary `output` type.

Here's how we could create a control like the famous
[counter example](https://elm-lang.org/examples/buttons) from the Elm Guide

    type CounterDelta
        = Increment
        | Decrement

    counterControl : Control Int CounterDelta Int
    counterControl =
        create
            { initEmpty = 0
            , initWith = identity
            , parse = Ok
            , update =
                \delta state ->
                    case delta of
                        Increment ->
                            state + 1

                        Decrement ->
                            state - 1
            , view =
                \{ state, name, id, label } ->
                    Html.div []
                        [ Html.label
                            [ Html.Attributes.for id ]
                            [ Html.text label ]
                        , Html.div
                            [ Html.Attributes.id id
                            , Html.Attributes.name name
                            ]
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
                        ]
            }

-}
create : ControlConfig state delta output -> Control state delta output
create config =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate
                        (\delta state ->
                            ( config.update delta state
                            , Cmd.none
                            )
                        )

                parse =
                    \(State _ state) ->
                        config.parse state
                            |> Result.mapError (List.map (Tuple.pair (Path.toString path)))
            in
            { path = path
            , index = 0
            , init = State { status = Intact_, selected = 0 } config.initEmpty
            , initWith = \input -> State { status = Intact_, selected = 0 } (config.initWith input)
            , baseUpdate = preUpdate
            , update = preUpdate 0
            , childViews = \_ _ -> []
            , view =
                \staticConfig dynamicConfig ->
                    config.view
                        { state = dynamicConfig.state
                        , label = Maybe.withDefault (Path.last path) staticConfig.label
                        , id = Maybe.withDefault (Path.toString path) staticConfig.id
                        , name = Maybe.withDefault (Path.toString path) staticConfig.name
                        , class = String.join " " staticConfig.class
                        }
                        |> wrappedView dynamicConfig.status
                        |> H.map ChangeState
            , parse = parse
            , setAllIdle = \(State i s) -> State { i | status = Idle_ } s
            , emitFlags = \_ -> []
            , collectDebouncingReceivers = \_ -> []
            , collectErrors = \_ _ -> []
            , receiverCount = 0
            , label = Nothing
            , id = Nothing
            , name = Nothing
            , class = []
            }
        )


wrapUpdate : (delta -> state -> ( state, Cmd delta )) -> Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
wrapUpdate innerUpdate debounce_ wrappedDelta (State internalState state) =
    case wrappedDelta of
        Skip ->
            ( State internalState state
            , Cmd.none
            )

        ChangeState delta ->
            let
                ( newState, cmd ) =
                    innerUpdate delta state
            in
            if debounce_ > 0 then
                ( State internalState newState
                , Cmd.batch
                    [ Task.perform StartDebouncing Time.now
                    , Cmd.map ChangeState cmd
                    ]
                )

            else
                ( State { internalState | status = Idle_ } newState
                , Cmd.map ChangeState cmd
                )

        StartDebouncing now ->
            ( State { internalState | status = DebouncingSince now } state
            , Task.perform (\() -> CheckDebouncer now) (Process.sleep debounce_)
            )

        CheckDebouncer now ->
            case internalState.status of
                DebouncingSince startTime ->
                    if now == startTime then
                        ( State { internalState | status = Idle_ } state
                        , Cmd.none
                        )

                    else
                        ( State internalState state
                        , Cmd.none
                        )

                _ ->
                    ( State internalState state
                    , Cmd.none
                    )

        TagSelected idx ->
            ( State { internalState | selected = idx } state
            , Cmd.none
            )



{-
   db    db  .d8b.  db      d888888b d8888b.  .d8b.  d888888b d888888b  .d88b.  d8b   db
   88    88 d8' `8b 88        `88'   88  `8D d8' `8b `~~88~~'   `88'   .8P  Y8. 888o  88
   Y8    8P 88ooo88 88         88    88   88 88ooo88    88       88    88    88 88V8o 88
   `8b  d8' 88~~~88 88         88    88   88 88~~~88    88       88    88    88 88 V8o88
    `8bd8'  88   88 88booo.   .88.   88  .8D 88   88    88      .88.   `8b  d8' 88  V888
      YP    YP   YP Y88888P Y888888P Y8888D' YP   YP    YP    Y888888P  `Y88P'  VP   V8P
-}


{-| Conditionally display an error on one or more items in a `list` control,
based on the output of the `list` control.

The following example will display errors on the first two items in a list of
strings, if and only if those first two items are "hello" and "world":

    myString =
        string
            |> catchFlag
                "flag-hello-world"
                "The first two items in the list must not be \"hello\" and \"world\".")

    list myString
        |> throwFlagsAt
            (\list_ ->
                case list of
                    "hello" :: "world" :: _ -> [ 0, 1 ]

                    _ -> []
            )
            "flag-hello-world"

-}
throwFlagsAt :
    (output -> List Int)
    -> String
    -> Control (List state) (ListDelta delta) output
    -> Control (List state) (ListDelta delta) output
throwFlagsAt check flagLabel (Control control) =
    Control (control >> listFlagEmitter check flagLabel)


listFlagEmitter :
    (output -> List Int)
    -> String
    -> ControlFns input (List state) (ListDelta delta) output
    -> ControlFns input (List state) (ListDelta delta) output
listFlagEmitter check flagLabel ctrl =
    { ctrl
        | emitFlags =
            \state ->
                let
                    oldFlags =
                        ctrl.emitFlags state

                    newFlags =
                        case ctrl.parse state of
                            Ok output ->
                                [ FlagList ctrl.path flagLabel (check output) ]

                            Err _ ->
                                []
                in
                List.Extra.unique (oldFlags ++ newFlags)
    }


{-| Throw a "flag" from a `Control` if its output fails to meet the predicate.

This is meant to be used in combination with `catchFlag`, which catches the "flag"
and displays an error.

-}
throwFlagIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
throwFlagIf check flag (Control control) =
    Control (control >> flagEmitter check (FlagLabel flag))


flagEmitter : (output -> Bool) -> Flag -> ControlFns input state delta output -> ControlFns input state delta output
flagEmitter check flag ctrl =
    { ctrl
        | emitFlags =
            \state ->
                let
                    oldFlags =
                        ctrl.emitFlags state

                    newFlags =
                        case ctrl.parse state of
                            Ok output ->
                                if check output then
                                    [ flag ]

                                else
                                    []

                            Err _ ->
                                []
                in
                List.Extra.unique (oldFlags ++ newFlags)
    }


{-| Catch a "flag" thrown by another `Control` and display an error on this
control.

This is meant to be used in combination with `throwFlagIf`, which throws the "flag".

-}
catchFlag : String -> String -> Control state delta output -> Control state delta output
catchFlag flag message (Control control) =
    Control (control >> flagReceiver (FlagLabel flag) message)


flagReceiver : Flag -> String -> ControlFns input state delta output -> ControlFns input state delta output
flagReceiver flag message ctrl =
    { ctrl
        | collectErrors =
            \state flags ->
                let
                    oldReceiver =
                        ctrl.collectErrors state flags

                    newReceiver =
                        if List.member flag flags then
                            [ ( Path.toString ctrl.path, message ) ]

                        else
                            []
                in
                List.Extra.unique (oldReceiver ++ newReceiver)
        , receiverCount = ctrl.receiverCount + 1
        , collectDebouncingReceivers =
            \((State internalState _) as state) ->
                case internalState.status of
                    DebouncingSince _ ->
                        flag :: ctrl.collectDebouncingReceivers state

                    _ ->
                        ctrl.collectDebouncingReceivers state
    }


{-| Display an error on a `Control` if its output fails to meet the predicate.

    int
        |> failIf
            (\int_ -> int_ < 1)
            "Integer must be greater than zero!"

-}
failIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
failIf check message (Control c) =
    Control
        (\path ->
            let
                control =
                    c path

                flag =
                    FlagPath path control.receiverCount
            in
            control
                |> flagEmitter check flag
                |> flagReceiver flag message
        )



{-
   d8888b. d88888b d8888b.  .d88b.  db    db d8b   db  .o88b. d88888b
   88  `8D 88'     88  `8D .8P  Y8. 88    88 888o  88 d8P  Y8 88'
   88   88 88ooooo 88oooY' 88    88 88    88 88V8o 88 8P      88ooooo
   88   88 88~~~~~ 88~~~b. 88    88 88    88 88 V8o88 8b      88~~~~~
   88  .8D 88.     88   8D `8b  d8' 88b  d88 88  V888 Y8b  d8 88.
   Y8888D' Y88888P Y8888P'  `Y88P'  ~Y8888P' VP   V8P  `Y88P' Y88888P
-}


{-| Debounce a `Control` for a number of milliseconds before it will display
any parsing or validation errors. This can be used to allow the user to finish
typing into a text input before we show them feedback.

    string
        |> debounce 500

-}
debounce : Float -> Control state delta output -> Control state delta output
debounce millis (Control control) =
    let
        debouncer i =
            { i | update = i.baseUpdate millis }
    in
    Control (control >> debouncer)



{-
   d888888b d8888b.
     `88'   88  `8D
      88    88   88
      88    88   88
     .88.   88  .8D
   Y888888P Y8888D'
-}


{-| Set an id for a `Control`.

    string
        |> id "My string"

-}
id : String -> Control state delta output -> Control state delta output
id id_ (Control control) =
    let
        identifier i =
            { i | id = Just id_ }
    in
    Control (control >> identifier)



{-
   d8b   db  .d8b.  .88b  d88. d88888b
   888o  88 d8' `8b 88'YbdP`88 88'
   88V8o 88 88ooo88 88  88  88 88ooooo
   88 V8o88 88~~~88 88  88  88 88~~~~~
   88  V888 88   88 88  88  88 88.
   VP   V8P YP   YP YP  YP  YP Y88888P
-}


{-| Set a name for a `Control`.

    string
        |> name "My string"

-}
name : String -> Control state delta output -> Control state delta output
name name_ (Control control) =
    let
        namer i =
            { i | name = Just name_ }
    in
    Control (control >> namer)



{-
   db       .d8b.  d8888b. d88888b db
   88      d8' `8b 88  `8D 88'     88
   88      88ooo88 88oooY' 88ooooo 88
   88      88~~~88 88~~~b. 88~~~~~ 88
   88booo. 88   88 88   8D 88.     88booo.
   Y88888P YP   YP Y8888P' Y88888P Y88888P
-}


{-| Set a label for a `Control`.

    string
        |> label "Enter your name"

-}
label : String -> Control state delta output -> Control state delta output
label label_ (Control control) =
    let
        labeller i =
            { i | label = Just label_ }
    in
    Control (control >> labeller)



{-
       .o88b. db       .d8b.  .d8888. .d8888.
      d8P  Y8 88      d8' `8b 88'  YP 88'  YP
      8P      88      88ooo88 `8bo.   `8bo.
      8b      88      88~~~88   `Y8b.   `Y8b.
      Y8b  d8 88booo. 88   88 db   8D db   8D
       `Y88P' Y88888P YP   YP `8888Y' `8888Y'
   S
-}


{-| Add an HTML class attribute for a `Control`. See docs for Html.Attributes.class in elm-html.

    string
        |> class "important"

-}
class : String -> Control state delta output -> Control state delta output
class class_ (Control control) =
    let
        classifier i =
            { i | class = class_ :: i.class }
    in
    Control (control >> classifier)


{-| Add a list of HTML class attributes to a `Control`. See docs for Html.Attributes.classList in elm-html.

    string
        |> classList
            [ ( "text", True )
            , ( "text-important", isImportant )
            , ( "text-seen", isSeen )
            ]

-}
classList : List ( String, Bool ) -> Control state delta output -> Control state delta output
classList classList_ (Control control) =
    let
        classifier i =
            { i
                | class =
                    List.filterMap
                        (\( class_, isActive ) ->
                            if isActive then
                                Just class_

                            else
                                Nothing
                        )
                        classList_
                        ++ i.class
            }
    in
    Control (control >> classifier)



{-
   db       .d8b.  db    db  .d88b.  db    db d888888b
   88      d8' `8b `8b  d8' .8P  Y8. 88    88 `~~88~~'
   88      88ooo88  `8bd8'  88    88 88    88    88
   88      88~~~88    88    88    88 88    88    88
   88booo. 88   88    88    `8b  d8' 88b  d88    88
   Y88888P YP   YP    YP     `Y88P'  ~Y8888P'    YP
-}


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
    (List (Html (Delta delta)) -> ViewConfigStatic -> ViewConfigDynamic state -> Html (Delta delta))
    -> Control state delta output
    -> Control state delta output
layout v (Control control) =
    let
        viewer i =
            { i | view = \staticConfig dynamicConfig -> v (i.childViews staticConfig dynamicConfig) staticConfig dynamicConfig }
    in
    Control (control >> viewer)



{-
   d888888b d8b   db d888888b d888888b db   d8b   db d888888b d888888b db   db
     `88'   888o  88   `88'   `~~88~~' 88   I8I   88   `88'   `~~88~~' 88   88
      88    88V8o 88    88       88    88   I8I   88    88       88    88ooo88
      88    88 V8o88    88       88    Y8   I8I   88    88       88    88~~~88
     .88.   88  V888   .88.      88    `8b d8'8b d8'   .88.      88    88   88
   Y888888P VP   V8P Y888888P    YP     `8b8' `8d8'  Y888888P    YP    YP   YP
-}


{-| Initialise a `Control` using a value of the type that the `Control` outputs.

    tuple "Int" int "String" string
        |> initWith ( 1, "hello" )

-}
initWith : input -> AdvancedControl input state delta output -> AdvancedControl input state delta output
initWith input (Control control) =
    let
        initialiser i =
            { i | init = i.initWith input }
    in
    Control (control >> initialiser)



{-
   d888888b d8b   db d888888b
     `88'   888o  88 `~~88~~'
      88    88V8o 88    88
      88    88 V8o88    88
     .88.   88  V888    88
   Y888888P VP   V8P    YP
-}


{-| A control that produces an `Int`. Renders as an HTML text input.
-}
int : Control String String Int
int =
    create
        { initEmpty = ""
        , initWith = String.fromInt
        , update = \delta _ -> delta
        , view = textControlView "number"
        , parse =
            \state ->
                case String.toInt state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "must be a whole number" ]
        }
        |> debounce 500



{-
   d88888b db       .d88b.   .d8b.  d888888b
   88'     88      .8P  Y8. d8' `8b `~~88~~'
   88ooo   88      88    88 88ooo88    88
   88~~~   88      88    88 88~~~88    88
   88      88booo. `8b  d8' 88   88    88
   YP      Y88888P  `Y88P'  YP   YP    YP

-}


{-| A control that produces a `Float`. Renders as an HTML text input.
-}
float : Control String String Float
float =
    create
        { initEmpty = ""
        , initWith = String.fromFloat
        , update = \delta _ -> delta
        , view = textControlView "number"
        , parse =
            \state ->
                case String.toFloat state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "must be a number" ]
        }
        |> debounce 500



{-
   .d8888. d888888b d8888b. d888888b d8b   db  d888b
   88'  YP `~~88~~' 88  `8D   `88'   888o  88 88' Y8b
   `8bo.      88    88oobY'    88    88V8o 88 88
     `Y8b.    88    88`8b      88    88 V8o88 88  ooo
   db   8D    88    88 `88.   .88.   88  V888 88. ~8~
   `8888Y'    YP    88   YD Y888888P VP   V8P  Y888P
-}


{-| A control that produces a `String`. Renders as an HTML text input.
-}
string : Control String String String
string =
    create
        { initEmpty = ""
        , initWith = identity
        , update = \delta _ -> delta
        , view = textControlView "text"
        , parse = Ok
        }
        |> debounce 500



{-
    .o88b. db   db  .d8b.  d8888b.
   d8P  Y8 88   88 d8' `8b 88  `8D
   8P      88ooo88 88ooo88 88oobY'
   8b      88~~~88 88~~~88 88`8b
   Y8b  d8 88   88 88   88 88 `88.
    `Y88P' YP   YP YP   YP 88   YD
-}


{-| A control that produces a `Char`. Renders as an HTML text input.
-}
char : Control String String Char
char =
    create
        { initEmpty = ""
        , initWith = String.fromChar
        , update = \delta _ -> delta
        , view = textControlView "text"
        , parse =
            \str ->
                case String.uncons str of
                    Just ( char_, rest ) ->
                        if String.isEmpty rest then
                            Ok char_

                        else
                            Err [ "must be exactly one character" ]

                    Nothing ->
                        Err [ "must not be blank" ]
        }
        |> debounce 500



{-
   d88888b d8b   db db    db .88b  d88.
   88'     888o  88 88    88 88'YbdP`88
   88ooooo 88V8o 88 88    88 88  88  88
   88~~~~~ 88 V8o88 88    88 88  88  88
   88.     88  V888 88b  d88 88  88  88
   Y88888P VP   V8P ~Y8888P' YP  YP  YP
-}


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
            ( "Green", Green )
            [ ( "Blue", Blue ) ]

-}
enum :
    ( String, enum )
    -> ( String, enum )
    -> List ( String, enum )
    -> Control enum enum enum
enum first second rest =
    create
        { initEmpty = Tuple.second first
        , initWith = identity
        , update = \delta _ -> delta
        , view = enumView (first :: second :: rest)
        , parse = Ok
        }



{-
   d8888b.  .d88b.   .d88b.  db
   88  `8D .8P  Y8. .8P  Y8. 88
   88oooY' 88    88 88    88 88
   88~~~b. 88    88 88    88 88
   88   8D `8b  d8' `8b  d8' 88booo.
   Y8888P'  `Y88P'   `Y88P'  Y88888P
-}


{-| A control that produces a Boolean value. Renders as an HTML radio control.
-}
bool : String -> String -> Control Bool Bool Bool
bool trueId falseId =
    enum ( trueId, True ) ( falseId, False ) []


{-| A control that produces a Boolean value. Renders as an HTML checkbox.
-}
checkbox : Control Bool Bool Bool
checkbox =
    create
        { initEmpty = False
        , initWith = identity
        , view =
            \config ->
                H.div []
                    [ H.input
                        [ HA.type_ "checkbox"
                        , HA.id config.id
                        , HA.name config.name
                        , HA.class config.class
                        , HA.checked config.state
                        , HE.onClick (not config.state)
                        ]
                        []
                    , H.label
                        [ HA.for config.id ]
                        [ H.text config.label ]
                    ]
        , update = \delta _ -> delta
        , parse = Ok
        }



{-
   db   d8b   db d8888b.  .d8b.  d8888b. d8888b. d88888b d8888b.
   88   I8I   88 88  `8D d8' `8b 88  `8D 88  `8D 88'     88  `8D
   88   I8I   88 88oobY' 88ooo88 88oodD' 88oodD' 88ooooo 88oobY'
   Y8   I8I   88 88`8b   88~~~88 88~~~   88~~~   88~~~~~ 88`8b
   `8b d8'8b d8' 88 `88. 88   88 88      88      88.     88 `88.
    `8b8' `8d8'  88   YD YP   YP 88      88      Y88888P 88   YD
-}


{-| A combinator that produces a wrapper type around a given type.

    type Id
        = Id Int

    idControl =
        wrapper Id (\(Id i) -> i) int

-}
wrapper :
    { label : String, wrap : output -> wrapped, unwrap : wrapped -> output }
    -> Control state delta output
    ->
        Control
            ( State state, End )
            ( Delta delta, End )
            wrapped
wrapper config control =
    record config.wrap
        |> field config.label config.unwrap control
        |> end



{-
   .88b  d88.  .d8b.  db    db d8888b. d88888b
   88'YbdP`88 d8' `8b `8b  d8' 88  `8D 88'
   88  88  88 88ooo88  `8bd8'  88oooY' 88ooooo
   88  88  88 88~~~88    88    88~~~b. 88~~~~~
   88  88  88 88   88    88    88   8D 88.
   YP  YP  YP YP   YP    YP    Y8888P' Y88888P
-}


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
maybe control =
    customType
        (\nothing just tag ->
            case tag of
                Nothing ->
                    nothing

                Just a ->
                    just a
        )
        |> tag0 "Nothing" Nothing
        |> tag1 "Just" Just control
        |> end



{-
   d8888b. d88888b .d8888. db    db db      d888888b
   88  `8D 88'     88'  YP 88    88 88      `~~88~~'
   88oobY' 88ooooo `8bo.   88    88 88         88
   88`8b   88~~~~~   `Y8b. 88    88 88         88
   88 `88. 88.     db   8D 88b  d88 88booo.    88
   88   YD Y88888P `8888Y' ~Y8888P' Y88888P    YP
-}


{-| A combinator that produces a `Result` with a given error and
success type.

    myResultControl =
        result string int

-}
result :
    Control failureState failureDelta failureOutput
    -> Control successState successDelta successOutput
    ->
        Control
            ( State ( State failureState, End ), ( State ( State successState, End ), End ) )
            ( Delta ( Delta failureDelta, End ), ( Delta ( Delta successDelta, End ), End ) )
            (Result failureOutput successOutput)
result failureControl successControl =
    customType
        (\err ok tag ->
            case tag of
                Err failure ->
                    err failure

                Ok success ->
                    ok success
        )
        |> tag1 "Err" Err failureControl
        |> tag1 "Ok" Ok successControl
        |> end



{-
   d888888b db    db d8888b. db      d88888b
   `~~88~~' 88    88 88  `8D 88      88'
      88    88    88 88oodD' 88      88ooooo
      88    88    88 88~~~   88      88~~~~~
      88    88b  d88 88      88booo. 88.
      YP    ~Y8888P' 88      Y88888P Y88888P
-}


{-| A combinator that produces a tuple of two controls of given types.

    myTupleControl =
        tuple "First" string "Second" int

-}
tuple :
    ( String, Control state1 delta1 output1 )
    -> ( String, Control state2 delta2 output2 )
    -> Control ( State state1, ( State state2, End ) ) ( Delta delta1, ( Delta delta2, End ) ) ( output1, output2 )
tuple ( firstLabel, first ) ( secondLabel, second ) =
    record Tuple.pair
        |> field firstLabel Tuple.first first
        |> field secondLabel Tuple.second second
        |> end



{-
   d888888b d8888b. d888888b d8888b. db      d88888b
   `~~88~~' 88  `8D   `88'   88  `8D 88      88'
      88    88oobY'    88    88oodD' 88      88ooooo
      88    88`8b      88    88~~~   88      88~~~~~
      88    88 `88.   .88.   88      88booo. 88.
      YP    88   YD Y888888P 88      Y88888P Y88888P
-}


{-| A combinator that produces a triple of three controls of given types.

    myTripleControl =
        triple ( "First", string ) ( "Second", int ) ( "Third", float )

-}
triple :
    ( String, Control state1 delta1 output1 )
    -> ( String, Control state2 delta2 output2 )
    -> ( String, Control state3 delta3 output3 )
    ->
        Control
            ( State state1, ( State state2, ( State state3, End ) ) )
            ( Delta delta1, ( Delta delta2, ( Delta delta3, End ) ) )
            ( output1, output2, output3 )
triple ( firstLabel, first ) ( secondLabel, second ) ( thirdLabel, third ) =
    record (\a b c -> ( a, b, c ))
        |> field firstLabel (\( a, _, _ ) -> a) first
        |> field secondLabel (\( _, b, _ ) -> b) second
        |> field thirdLabel (\( _, _, c ) -> c) third
        |> end



{-
   db      d888888b .d8888. d888888b
   88        `88'   88'  YP `~~88~~'
   88         88    `8bo.      88
   88         88      `Y8b.    88
   88booo.   .88.   db   8D    88
   Y88888P Y888888P `8888Y'    YP
-}


{-| A combinator that produces a `List` of controls of a given type.

    myListControl =
        list int

-}
list : Control state delta output -> Control (List (State state)) (ListDelta delta) (List output)
list (Control ctrl) =
    Control
        (\path ->
            let
                update =
                    wrapUpdate listUpdate

                listUpdate delta state =
                    case delta of
                        InsertItem idx ->
                            let
                                itemControl =
                                    ctrl path

                                before =
                                    List.take idx state

                                after =
                                    List.drop idx state
                            in
                            ( before ++ itemControl.init :: after, Cmd.none )

                        ChangeItem idx itemDelta ->
                            let
                                ( newState, cmds ) =
                                    List.foldr
                                        (\( i, item ) ( items, cmds_ ) ->
                                            if i == idx then
                                                let
                                                    itemControl =
                                                        ctrl (Path.add (String.fromInt i) path)

                                                    ( newItem, newCmd ) =
                                                        itemControl.update itemDelta item
                                                in
                                                ( newItem :: items, newCmd :: cmds_ )

                                            else
                                                ( item :: items, cmds_ )
                                        )
                                        ( [], [] )
                                        (List.indexedMap Tuple.pair state)
                            in
                            ( newState, Cmd.batch cmds |> Cmd.map (ChangeItem idx) )

                        DeleteItem idx ->
                            ( List.Extra.removeAt idx state, Cmd.none )

                parse =
                    \(State _ state) ->
                        List.foldr
                            (\( idx, item ) res ->
                                let
                                    identifyErrors e =
                                        List.map (\( _, feedback ) -> "item #" ++ String.fromInt idx ++ ": " ++ feedback) e

                                    itemControl =
                                        ctrl (Path.add (String.fromInt idx) path)
                                in
                                case res of
                                    Ok outputs ->
                                        case itemControl.parse item of
                                            Ok output ->
                                                Ok (output :: outputs)

                                            Err errs ->
                                                Err (identifyErrors errs)

                                    Err errs ->
                                        case itemControl.parse item of
                                            Ok _ ->
                                                Err errs

                                            Err newErrs ->
                                                Err (identifyErrors newErrs ++ errs)
                            )
                            (Ok [])
                            (List.indexedMap Tuple.pair state)
                            |> Result.mapError (List.map (\feedback -> ( Path.toString path, feedback )))

                collectDebouncingReceivers (State _ listState) =
                    List.indexedMap
                        (\idx itemState ->
                            let
                                itemControl =
                                    ctrl (Path.add (String.fromInt idx) path)
                            in
                            itemControl.collectDebouncingReceivers itemState
                        )
                        listState
                        |> List.concat
            in
            { path = path
            , index = 0
            , initWith =
                \input ->
                    List.indexedMap
                        (\idx item ->
                            let
                                itemControl =
                                    ctrl (Path.add (String.fromInt idx) path)
                            in
                            itemControl.initWith item
                        )
                        input
                        |> State { status = Intact_, selected = 0 }
            , init = State { status = Intact_, selected = 0 } []
            , baseUpdate = update
            , update = update 0
            , childViews = \_ _ -> []
            , view =
                \staticConfig dynamicConfig ->
                    let
                        debouncingReceivers =
                            -- this is a total hack!
                            collectDebouncingReceivers (State { status = Intact_, selected = dynamicConfig.selected } dynamicConfig.state)
                    in
                    listView path staticConfig dynamicConfig debouncingReceivers ctrl
            , parse = parse
            , setAllIdle =
                \(State i s) ->
                    State { i | status = Idle_ }
                        (List.indexedMap
                            (\idx item ->
                                let
                                    itemControl =
                                        ctrl (Path.add (String.fromInt idx) path)
                                in
                                itemControl.setAllIdle item
                            )
                            s
                        )
            , emitFlags =
                \(State _ s) ->
                    List.indexedMap
                        (\idx item ->
                            let
                                itemControl =
                                    ctrl (Path.add (String.fromInt idx) path)
                            in
                            itemControl.emitFlags item
                        )
                        s
                        |> List.concat
            , collectErrors =
                \(State _ listState) flags ->
                    List.indexedMap
                        (\idx item ->
                            let
                                itemControl =
                                    ctrl (Path.add (String.fromInt idx) path)

                                filteredFlags =
                                    List.filterMap
                                        (\flag ->
                                            case flag of
                                                FlagList flagPath flagLabel flagIndexes ->
                                                    if flagPath == path then
                                                        if List.member idx flagIndexes then
                                                            Just (FlagLabel flagLabel)

                                                        else
                                                            Nothing

                                                    else
                                                        Just flag

                                                _ ->
                                                    Just flag
                                        )
                                        flags
                            in
                            itemControl.collectErrors item filteredFlags
                        )
                        listState
                        |> List.concat
            , receiverCount = 0
            , collectDebouncingReceivers = collectDebouncingReceivers
            , label = Nothing
            , id = Nothing
            , name = Nothing
            , class = []
            }
        )



{-
   d8888b. d888888b  .o88b. d888888b
   88  `8D   `88'   d8P  Y8 `~~88~~'
   88   88    88    8P         88
   88   88    88    8b         88
   88  .8D   .88.   Y8b  d8    88
   Y8888D' Y888888P  `Y88P'    YP
-}


{-| A combinator that produces a `Dict` from controls of given key and value
types. The key type must be `comparable`.

    myDictControl =
        dict ( "Key", int ) ( "Value", string )

-}
dict :
    ( String, Control keyState keyDelta comparable )
    -> ( String, Control valueState valueDelta value )
    ->
        Control
            ( State (List (State ( State keyState, ( State valueState, End ) ))), End )
            ( Delta (ListDelta ( Delta keyDelta, ( Delta valueDelta, End ) )), End )
            (Dict.Dict comparable value)
dict ( keyLabel, keyControl ) ( valueLabel, valueControl ) =
    list
        (tuple
            ( keyLabel, keyControl |> catchFlag "@@dict-unique-keys" "Keys must be unique" )
            ( valueLabel, valueControl )
        )
        |> throwFlagsAt (List.map Tuple.first >> nonUniqueIndexes) "@@dict-unique-keys"
        |> wrapper { label = "dict", wrap = Dict.fromList, unwrap = Dict.toList }


nonUniqueIndexes : List comparable -> List Int
nonUniqueIndexes listState =
    let
        duplicates =
            List.Extra.frequencies listState
                |> List.filterMap
                    (\( item, count ) ->
                        if count > 1 then
                            Just item

                        else
                            Nothing
                    )
    in
    List.indexedMap
        (\idx item ->
            if List.member item duplicates then
                Just idx

            else
                Nothing
        )
        listState
        |> List.filterMap identity



{-
   .d8888. d88888b d888888b
   88'  YP 88'     `~~88~~'
   `8bo.   88ooooo    88
     `Y8b. 88~~~~~    88
   db   8D 88.        88
   `8888Y' Y88888P    YP
-}


{-| A combinator that produces a `Set` from controls of a given member
types. The member type must be `comparable`.

    mySetControl =
        set string

-}
set :
    Control state delta comparable
    -> Control ( State (List (State state)), End ) ( Delta (ListDelta delta), End ) (Set.Set comparable)
set memberControl =
    list
        (memberControl |> catchFlag "@@set-unique-keys" "Set members must be unique")
        |> throwFlagsAt nonUniqueIndexes "@@set-unique-keys"
        |> wrapper { label = "set", wrap = Set.fromList, unwrap = Set.toList }



{-
    .d8b.  d8888b. d8888b.  .d8b.  db    db
   d8' `8b 88  `8D 88  `8D d8' `8b `8b  d8'
   88ooo88 88oobY' 88oobY' 88ooo88  `8bd8'
   88~~~88 88`8b   88`8b   88~~~88    88
   88   88 88 `88. 88 `88. 88   88    88
   YP   YP 88   YD 88   YD YP   YP    YP
-}


{-| A combinator that produces an `Array` from a control of a given type.

    myArrayControl =
        array int

-}
array :
    Control state delta output
    ->
        Control
            ( State (List (State state)), End )
            ( Delta (ListDelta delta), End )
            (Array.Array output)
array itemControl =
    list itemControl
        |> wrapper { label = "array", wrap = Array.fromList, unwrap = Array.toList }



{-
   d8888b. d88888b  .o88b.  .d88b.  d8888b. d8888b.
   88  `8D 88'     d8P  Y8 .8P  Y8. 88  `8D 88  `8D
   88oobY' 88ooooo 8P      88    88 88oobY' 88   88
   88`8b   88~~~~~ 8b      88    88 88`8b   88   88
   88 `88. 88.     Y8b  d8 `8b  d8' 88 `88. 88  .8D
   88   YD Y88888P  `Y88P'  `Y88P'  88   YD Y8888D'

-}


{-| Finalise the construction of a `record` or `customType` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field "Hello" string
            |> end

-}
end :
    Builder
        { d
            | afters : afters1
            , befores : End -> befores1
            , fields : Path -> End -> ( RecordFns input2 state delta output2 recordOutput, restFns1 )
            , flagEmitter : (List Flag -> End -> End -> List Flag) -> List Flag -> ( RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( State state, restStates ) -> List Flag
            , errorCollector : (List Flag -> List ( String, String ) -> End -> End -> List ( String, String )) -> List Flag -> List ( String, String ) -> ( RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( State state, restStates ) -> List ( String, String )
            , idleSetter : (End -> End -> End) -> ( RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( State state, restStates ) -> ( State state, restStates )
            , initialiser : (input -> End -> End) -> input -> ( RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( State state, restStates )
            , labels : List String
            , makeSetters : (End -> End -> End) -> befores1 -> afters1 -> ( Delta delta -> recordDelta, restDeltaSetters )
            , parser : (Result (List ( String, String )) output -> End -> End -> Result (List ( String, String )) output) -> Result (List ( String, String )) output1_1 -> ( RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( State state, restStates ) -> Result (List ( String, String )) output
            , debouncingReceiverCollector : (List Flag -> End -> End -> List Flag) -> List Flag -> ( RecordFns input2 state delta output2 recordOutput, restFns1 ) -> ( State state, restStates ) -> List Flag
            , states : Path -> End -> ( State state, restStates )
            , toOutput : output1_1
            , updater :
                ({ newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : End -> ( State state, restStates ) } -> End -> End -> End -> End -> { newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : End -> ( State state, restStates ) })
                -> { newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : ( State state, restStates ) -> ( State state, restStates ) }
                -> ( RecordFns input2 state delta output2 recordOutput, restFns1 )
                -> ( Delta delta -> recordDelta, restDeltaSetters )
                -> ( Delta delta, restDeltas )
                -> ( State state, restStates )
                -> { newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : End -> ( State state, restStates ) }
            , viewer :
                (List (Html (Delta ( Delta delta, restDeltas ))) -> List Flag -> End -> End -> End -> List (Html (Delta ( Delta delta, restDeltas ))))
                -> List (Html (Delta ( Delta delta, restDeltas )))
                -> List Flag
                -> ( RecordFns input2 state delta output2 recordOutput, restFns1 )
                -> ( Delta delta -> recordDelta, restDeltaSetters )
                -> ( State state, restStates )
                -> List (Html (Delta ( Delta delta, restDeltas )))
        }
        { e
            | applyInputs : (state1 -> End -> state1) -> (stateSetter -> state0) -> ( stateSetter, restStateSetters ) -> input -> State ( State state, restStates )
            , deltaAfters : afters
            , deltaBefores : End -> befores
            , destructor : stateSetter -> state0
            , flagEmitter : (List Flag -> Int -> End -> End -> List Flag) -> List Flag -> Int -> ( ControlFns input1 state delta output1, restFns ) -> ( State state, restStates ) -> List Flag
            , errorCollector : (List Flag -> List ( String, String ) -> End -> End -> List ( String, String )) -> List Flag -> List ( String, String ) -> ( ControlFns input1 state delta output1, restFns ) -> ( State state, restStates ) -> List ( String, String )
            , fns : Path -> End -> ( ControlFns input1 state delta output1, restFns )
            , idleSetter : (Int -> End -> End -> End) -> Int -> ( ControlFns input1 state delta output1, restFns ) -> ( State state, restStates ) -> ( State state, restStates )
            , labels : List String
            , makeDeltaSetters : (End -> End -> End) -> befores -> afters -> ( Delta delta -> ( Delta delta, restDeltas ), restSetters )
            , makeStateSetters : (a1 -> b1 -> End -> End -> End -> End -> End) -> c -> ( State state, restStates ) -> ( ControlFns input1 state delta output1, restFns ) -> f -> g -> h -> ( stateSetter, restStateSetters )
            , parser : (a -> b -> End -> End -> a) -> Result (List ( String, String )) value -> Int -> ( ControlFns input1 state delta output1, restFns ) -> ( State state, restStates ) -> Result (List ( String, String )) output
            , debouncingReceiverCollector : (List Flag -> End -> End -> List Flag) -> List Flag -> ( ControlFns input1 state delta output1, restFns ) -> ( State state, restStates ) -> List Flag
            , stateAfters : h
            , stateBefores : End -> g
            , stateInserter : c
            , states : Path -> End -> ( State state, restStates )
            , toArgStates : End -> f
            , updater :
                ({ newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : End -> ( State state, restStates ) }
                 -> End
                 -> End
                 -> End
                 -> End
                 -> { newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : End -> ( State state, restStates ) }
                )
                -> { newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : ( State state, restStates ) -> ( State state, restStates ) }
                -> ( ControlFns input1 state delta output1, restFns )
                -> ( Delta delta -> ( Delta delta, restDeltas ), restSetters )
                -> ( Delta delta, restDeltas )
                -> ( State state, restStates )
                -> { newCmds : List (Cmd ( Delta delta, restDeltas )), newStates : End -> ( State state, restStates ) }
            , viewer :
                (Maybe (Html ( Delta delta, restDeltas ))
                 -> List Flag
                 -> Int
                 -> End
                 -> End
                 -> End
                 -> Maybe (Html ( Delta delta, restDeltas ))
                )
                -> Maybe (Html ( Delta delta, restDeltas ))
                -> List Flag
                -> Int
                -> ( ControlFns input1 state delta output1, restFns )
                -> ( Delta delta -> ( Delta delta, restDeltas ), restSetters )
                -> ( State state, restStates )
                -> Maybe (Html ( Delta delta, restDeltas ))
        }
    ->
        AdvancedControl
            input
            ( State state, restStates )
            ( Delta delta, restDeltas )
            output
end builder =
    case builder of
        Rec r ->
            endRecord r

        Cus c ->
            endCustomType c


{-| An internal type used to differentiate between builders for record and custom type combinators
-}
type Builder r c
    = Rec r
    | Cus c


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
        Builder
            { index : Int
            , labels : List String
            , toOutput : constructor
            , fields : c -> d -> d
            , states : e -> f -> f
            , updater : g -> g
            , viewer : h -> h
            , parser : i -> i
            , idleSetter : j -> j
            , initialiser : k -> k
            , before : l -> l
            , befores : m -> m
            , after : End
            , afters : End
            , makeSetters : n -> n
            , flagEmitter : o -> o
            , errorCollector : p -> p
            , debouncingReceiverCollector : q -> q
            }
            cus
record toOutput =
    Rec
        { index = 0
        , labels = []
        , toOutput = toOutput
        , fields = \_ x -> x
        , states = \_ x -> x
        , updater = identity
        , viewer = identity
        , parser = identity
        , idleSetter = identity
        , initialiser = identity
        , before = identity
        , befores = identity
        , after = End
        , afters = End
        , makeSetters = identity
        , flagEmitter = identity
        , errorCollector = identity
        , debouncingReceiverCollector = identity
        }


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
    -> AdvancedControl input state delta output
    ->
        Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path -> ( { field : ControlFns input state delta output, fromInput : c, access : Access }, d ) -> e
            , states : Path -> ( State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html (Delta j)) -> List Flag -> k -> l -> m -> List (Html (Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Delta w, x ) -> y
            , befores : ( ( Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Flag -> f1 -> g1 -> List Flag
            , errorCollector : h1 -> List Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Flag -> l1 -> m1 -> List Flag
            }
            n1
    ->
        Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path -> d -> e
            , states : Path -> f -> g
            , updater : h -> { newStates : ( State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Delta q1 -> recordDelta, restDeltaSetters ) -> ( Delta q1, restDeltas ) -> ( State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html (Delta j)) -> List Flag -> ( RecordFns s1 t1 u1 v1 w1, k ) -> ( Delta u1 -> j, l ) -> ( State t1, m ) -> List (Html (Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( RecordFns x1 y1 z1 output0 a2, o ) -> ( State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( RecordFns b2 c2 d2 e2 f2, r ) -> ( State c2, s ) -> ( State c2, s )
            , initialiser : t -> recordInput -> ( RecordFns g2 h2 i2 g2 recordInput, u ) -> ( State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Flag -> ( RecordFns l2 m2 n2 o2 p2, f1 ) -> ( State m2, g1 ) -> List Flag
            , errorCollector : h1 -> List Flag -> List ( String, String ) -> ( RecordFns q2 r2 s2 t2 u2, i1 ) -> ( State r2, j1 ) -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Flag -> ( RecordFns v2 w2 x2 y2 z2, l1 ) -> ( State w2, m1 ) -> List Flag
            }
            n1
field =
    fieldHelper Open


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
    -> AdvancedControl input state delta output
    ->
        Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path -> ( { field : ControlFns input state delta output, fromInput : c, access : Access }, d ) -> e
            , states : Path -> ( State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html (Delta j)) -> List Flag -> k -> l -> m -> List (Html (Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Delta w, x ) -> y
            , befores : ( ( Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Flag -> f1 -> g1 -> List Flag
            , errorCollector : h1 -> List Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Flag -> l1 -> m1 -> List Flag
            }
            n1
    ->
        Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path -> d -> e
            , states : Path -> f -> g
            , updater : h -> { newStates : ( State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Delta q1 -> recordDelta, restDeltaSetters ) -> ( Delta q1, restDeltas ) -> ( State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html (Delta j)) -> List Flag -> ( RecordFns s1 t1 u1 v1 w1, k ) -> ( Delta u1 -> j, l ) -> ( State t1, m ) -> List (Html (Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( RecordFns x1 y1 z1 output0 a2, o ) -> ( State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( RecordFns b2 c2 d2 e2 f2, r ) -> ( State c2, s ) -> ( State c2, s )
            , initialiser : t -> recordInput -> ( RecordFns g2 h2 i2 g2 recordInput, u ) -> ( State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Flag -> ( RecordFns l2 m2 n2 o2 p2, f1 ) -> ( State m2, g1 ) -> List Flag
            , errorCollector : h1 -> List Flag -> List ( String, String ) -> ( RecordFns q2 r2 s2 t2 u2, i1 ) -> ( State r2, j1 ) -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Flag -> ( RecordFns v2 w2 x2 y2 z2, l1 ) -> ( State w2, m1 ) -> List Flag
            }
            n1
hiddenField =
    fieldHelper Hidden


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
    -> AdvancedControl input state delta output
    ->
        Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path -> ( { field : ControlFns input state delta output, fromInput : c, access : Access }, d ) -> e
            , states : Path -> ( State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html (Delta j)) -> List Flag -> k -> l -> m -> List (Html (Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Delta w, x ) -> y
            , befores : ( ( Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Flag -> f1 -> g1 -> List Flag
            , errorCollector : h1 -> List Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Flag -> l1 -> m1 -> List Flag
            }
            n1
    ->
        Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path -> d -> e
            , states : Path -> f -> g
            , updater : h -> { newStates : ( State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Delta q1 -> recordDelta, restDeltaSetters ) -> ( Delta q1, restDeltas ) -> ( State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html (Delta j)) -> List Flag -> ( RecordFns s1 t1 u1 v1 w1, k ) -> ( Delta u1 -> j, l ) -> ( State t1, m ) -> List (Html (Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( RecordFns x1 y1 z1 output0 a2, o ) -> ( State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( RecordFns b2 c2 d2 e2 f2, r ) -> ( State c2, s ) -> ( State c2, s )
            , initialiser : t -> recordInput -> ( RecordFns g2 h2 i2 g2 recordInput, u ) -> ( State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Flag -> ( RecordFns l2 m2 n2 o2 p2, f1 ) -> ( State m2, g1 ) -> List Flag
            , errorCollector : h1 -> List Flag -> List ( String, String ) -> ( RecordFns q2 r2 s2 t2 u2, i1 ) -> ( State r2, j1 ) -> List ( String, String )
            , debouncingReceiverCollector : k1 -> List Flag -> ( RecordFns v2 w2 x2 y2 z2, l1 ) -> ( State w2, m1 ) -> List Flag
            }
            n1
readOnlyField =
    fieldHelper ReadOnly


{-| An internal type used to represent whether a record field is editable, read-only, or hidden
-}
type Access
    = Open
    | ReadOnly
    | Hidden


fieldHelper access label_ fromInput (Control control) builder =
    case builder of
        Cus c ->
            Cus c

        Rec rec ->
            Rec
                { index = rec.index + 1
                , labels = rec.labels ++ [ label_ ]
                , toOutput = rec.toOutput
                , fields =
                    \path ->
                        rec.fields path
                            << Tuple.pair
                                { field = control (Path.add label_ path)
                                , fromInput = fromInput
                                , access = access
                                }
                , states =
                    \path ->
                        rec.states path
                            << Tuple.pair (control (Path.add label_ path) |> .init)
                , updater = rec.updater >> recordStateUpdater
                , viewer = rec.viewer >> recordStateViewer
                , parser = rec.parser >> recordStateValidator
                , idleSetter = rec.idleSetter >> recordStateIdleSetter
                , initialiser = rec.initialiser >> recordStateInitialiser
                , before = rec.before << Tuple.pair Skip
                , befores = rec.befores << Tuple.pair rec.before
                , after = ( Skip, rec.after )
                , afters = ( rec.after, rec.afters )
                , makeSetters = rec.makeSetters >> deltaSetterMaker
                , flagEmitter = rec.flagEmitter >> recordFlagEmitter
                , errorCollector = rec.errorCollector >> recordErrorCollector
                , debouncingReceiverCollector = rec.debouncingReceiverCollector >> recordDebouncingReceiverCollector
                }


endRecord rec =
    Control
        (\path ->
            let
                fns =
                    rec.fields path End

                inits =
                    rec.states path End

                deltaSetters =
                    makeDeltaSetters rec.makeSetters rec.befores rec.afters

                update delta (State s state) =
                    case delta of
                        Skip ->
                            ( State s state, Cmd.none )

                        ChangeState deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater fns deltaSetters deltas state
                            in
                            ( State s newState
                            , Cmd.map ChangeState cmd
                            )

                        _ ->
                            ( State s state, Cmd.none )

                childViews _ dynamicConfig =
                    viewRecordStates rec.viewer dynamicConfig.flags fns deltaSetters dynamicConfig.state

                view staticConfig dynamicConfig =
                    let
                        childViews_ =
                            viewRecordStates rec.viewer dynamicConfig.flags fns deltaSetters dynamicConfig.state

                        id_ =
                            Maybe.withDefault (Path.toString path) staticConfig.id

                        label_ =
                            Maybe.withDefault (Path.last path) staticConfig.label
                    in
                    case childViews_ of
                        [ onlyChild ] ->
                            onlyChild

                        _ ->
                            H.section [ HA.id id_ ]
                                (H.h2 [] [ H.text label_ ] :: childViews_)

                parse (State _ state) =
                    validateRecordStates rec.parser rec.toOutput fns state

                setAllIdle (State i state) =
                    State { i | status = Idle_ } (setAllRecordStatesToIdle rec.idleSetter fns state)

                emitFlags (State _ state) =
                    emitFlagsForRecord rec.flagEmitter fns state
            in
            { path = path
            , index = 0
            , init = State { status = Intact_, selected = 0 } inits
            , initWith = \output -> initialiseRecordStates rec.initialiser output fns
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \staticConfig dynamicConfig -> childViews staticConfig dynamicConfig
            , view = \staticConfig dynamicConfig -> view staticConfig dynamicConfig
            , parse = parse
            , setAllIdle = setAllIdle
            , emitFlags = emitFlags
            , collectErrors = \(State _ states) flags -> collectErrorsForRecord rec.errorCollector flags fns states
            , receiverCount = 0
            , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForRecord rec.debouncingReceiverCollector fns states
            , label = Nothing
            , id = Nothing
            , name = Nothing
            , class = []
            }
        )



{-
   d8888b. d88888b  .o88b.  .d88b.  d8888b. d8888b.      d888888b d8b   db d888888b d88888b d8888b. d8b   db  .d8b.  db      .d8888.
   88  `8D 88'     d8P  Y8 .8P  Y8. 88  `8D 88  `8D        `88'   888o  88 `~~88~~' 88'     88  `8D 888o  88 d8' `8b 88      88'  YP
   88oobY' 88ooooo 8P      88    88 88oobY' 88   88         88    88V8o 88    88    88ooooo 88oobY' 88V8o 88 88ooo88 88      `8bo.
   88`8b   88~~~~~ 8b      88    88 88`8b   88   88         88    88 V8o88    88    88~~~~~ 88`8b   88 V8o88 88~~~88 88        `Y8b.
   88 `88. 88.     Y8b  d8 `8b  d8' 88 `88. 88  .8D        .88.   88  V888    88    88.     88 `88. 88  V888 88   88 88booo. db   8D
   88   YD Y88888P  `Y88P'  `Y88P'  88   YD Y8888D'      Y888888P VP   V8P    YP    Y88888P 88   YD VP   V8P YP   YP Y88888P `8888Y'
-}


collectDebouncingReceiversForRecord :
    ((List Flag
      -> End
      -> End
      -> List Flag
     )
     -> List Flag
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> List Flag
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Flag
collectDebouncingReceiversForRecord debouncingReceiverCollector_ fns states =
    debouncingReceiverCollector_ (\receivers End End -> receivers) [] fns states


recordDebouncingReceiverCollector :
    (List Flag
     -> restFns
     -> restStates
     -> List Flag
    )
    -> List Flag
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Flag
recordDebouncingReceiverCollector next receivers ( fns, restFns ) ( state, restStates ) =
    next (receivers ++ fns.field.collectDebouncingReceivers state) restFns restStates


collectErrorsForRecord :
    ((List Flag
      -> List ( String, String )
      -> End
      -> End
      -> List ( String, String )
     )
     -> List Flag
     -> List ( String, String )
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> List ( String, String )
    )
    -> List Flag
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List ( String, String )
collectErrorsForRecord errorCollector_ flags fns states =
    errorCollector_ (\_ errors End End -> errors) flags [] fns states


recordErrorCollector :
    (List Flag
     -> List ( String, String )
     -> restFns
     -> restStates
     -> List ( String, String )
    )
    -> List Flag
    -> List ( String, String )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List ( String, String )
recordErrorCollector next flags errors ( fns, restFns ) ( state, restStates ) =
    next flags (errors ++ fns.field.collectErrors state flags) restFns restStates


emitFlagsForRecord :
    ((List Flag
      -> End
      -> End
      -> List Flag
     )
     -> List Flag
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> List Flag
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Flag
emitFlagsForRecord flagEmitter_ fns states =
    flagEmitter_ (\flags End End -> flags) [] fns states


recordFlagEmitter :
    (List Flag
     -> restFns
     -> restStates
     -> List Flag
    )
    -> List Flag
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Flag
recordFlagEmitter next flags ( fns, restFns ) ( state, restStates ) =
    let
        newFlags =
            fns.field.emitFlags state
    in
    next (flags ++ newFlags) restFns restStates


makeDeltaSetters :
    ((End -> End -> End)
     -> befores
     -> afters
     -> deltaSetters
    )
    -> (End -> befores)
    -> afters
    -> deltaSetters
makeDeltaSetters makeSetters_ befores afters =
    makeSetters_ (\End End -> End) (befores End) afters


deltaSetterMaker :
    (befores
     -> afters
     -> next
    )
    -> ( ( value, after ) -> delta, befores )
    -> ( after, afters )
    -> ( value -> delta, next )
deltaSetterMaker next ( before, befores ) ( after, afters ) =
    ( \value -> before ( value, after )
    , next befores afters
    )


initialiseRecordStates :
    ((input -> End -> End)
     -> input
     -> fns
     -> state
    )
    -> input
    -> fns
    -> State state
initialiseRecordStates initialiser input fns =
    initialiser (\_ End -> End) input fns
        |> State { status = Intact_, selected = 0 }


recordStateInitialiser :
    (recordInput
     -> restFns
     -> restStates
    )
    -> recordInput
    -> ( RecordFns output state delta output recordInput, restFns )
    -> ( State state, restStates )
recordStateInitialiser next recordInput ( fns, restFns ) =
    ( fns.field.initWith (fns.fromInput recordInput)
    , next recordInput restFns
    )


validateRecordStates :
    ((Result (List ( String, String )) output2
      -> End
      -> End
      -> Result (List ( String, String )) output2
     )
     -> Result (List ( String, String )) output1
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> Result (List ( String, String )) output2
    )
    -> output1
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> Result (List ( String, String )) output2
validateRecordStates parser toOutput fns states =
    parser (\output End End -> output) (Ok toOutput) fns states


recordStateValidator :
    (Result (List ( String, String )) output1
     -> restFns
     -> restStates
     -> Result (List ( String, String )) output2
    )
    -> Result (List ( String, String )) (output0 -> output1)
    -> ( RecordFns input state delta output0 recordOutput, restFns )
    -> ( State state, restStates )
    -> Result (List ( String, String )) output2
recordStateValidator next toOutputResult ( fns, restFns ) ( state, restStates ) =
    next
        (case ( toOutputResult, fns.field.parse state ) of
            ( Ok toOutput, Ok parsed ) ->
                Ok (toOutput parsed)

            ( Ok _, Err es ) ->
                Err es

            ( Err es, Ok _ ) ->
                Err es

            ( Err es, Err es2 ) ->
                Err (es ++ es2)
        )
        restFns
        restStates


setAllRecordStatesToIdle :
    ((End -> End -> End)
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> ( State state, restStates )
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> ( State state, restStates )
setAllRecordStatesToIdle idleSetter_ fns states =
    idleSetter_ (\End End -> End) fns states


recordStateIdleSetter :
    (restFns
     -> restStates
     -> restStates
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> ( State state, restStates )
recordStateIdleSetter next ( fns, restFns ) ( state, restStates ) =
    ( fns.field.setAllIdle state
    , next restFns restStates
    )


viewRecordStates :
    ((List (Html msg)
      -> List Flag
      -> End
      -> End
      -> End
      -> List (Html msg)
     )
     -> List (Html msg)
     -> List Flag
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( Delta delta -> recordDelta, restDeltaSetters )
     -> ( State state, restStates )
     -> List (Html msg)
    )
    -> List Flag
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( State state, restStates )
    -> List (Html msg)
viewRecordStates viewer flags fns setters states =
    viewer (\views _ End End End -> views) [] flags fns setters states
        |> List.reverse


recordStateViewer :
    (List (Html (Delta recordDelta))
     -> List Flag
     -> restFns
     -> restDeltaSetters
     -> restStates
     -> List (Html (Delta recordDelta))
    )
    -> List (Html (Delta recordDelta))
    -> List Flag
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( State state, restStates )
    -> List (Html (Delta recordDelta))
recordStateViewer next views flags ( fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    let
        view =
            fns.field.view
                { id = fns.field.id
                , name = fns.field.name
                , label = fns.field.label
                , class = fns.field.class
                }
                { state = state
                , status = getStatus fns.field.parse fns.field.collectErrors flags (State internalState state)
                , flags = flags
                , selected = internalState.selected
                }
                |> H.map (\delta -> ChangeState (setter delta))
    in
    next
        ((case fns.access of
            Open ->
                view

            Hidden ->
                H.text ""

            ReadOnly ->
                H.div [ HA.disabled True ] [ view ]
         )
            :: views
        )
        flags
        restFns
        restSetters
        restStates


getStatus :
    (State state -> Result (List ( String, String )) output)
    -> (State state -> List Flag -> List ( String, String ))
    -> List Flag
    -> State state
    -> Status
getStatus parse collectErrors flags ((State internalState _) as state) =
    case internalState.status of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                parsedErrors =
                    case parse state of
                        Ok _ ->
                            []

                        Err errs ->
                            errs

                flaggedErrors =
                    collectErrors state flags
            in
            Idle (List.map Err (parsedErrors ++ flaggedErrors))


updateRecordStates :
    (({ newStates : End -> states, newCmds : List (Cmd msg) }
      -> End
      -> End
      -> End
      -> End
      -> { newStates : End -> states, newCmds : List (Cmd msg) }
     )
     -> { newStates : states -> states, newCmds : List (Cmd msg) }
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( setter, restSetters )
     -> ( Delta delta, restDeltas )
     -> ( State state, restStates )
     -> { newStates : End -> states, newCmds : List (Cmd msg) }
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( setter, restSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> ( states, Cmd msg )
updateRecordStates updater fields setters deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output End End End End -> output)
                { newStates = identity, newCmds = [] }
                fields
                setters
                deltas
                states
    in
    ( newStates End, Cmd.batch newCmds )


recordStateUpdater :
    ({ newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) }
     -> restFns
     -> restDeltaSetters
     -> restDeltas
     -> restStates
     -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
    )
    -> { newStates : ( State state, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) }
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
recordStateUpdater next { newStates, newCmds } ( fns, restFns ) ( deltaSetter, restDeltaSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        ( newState, newCmd ) =
            fns.field.update delta state

        cmd2 =
            newCmd
                |> Cmd.map deltaSetter
    in
    next
        { newStates = newStates << Tuple.pair newState
        , newCmds = cmd2 :: newCmds
        }
        restFns
        restDeltaSetters
        restDeltas
        restStates



{-
    .o88b. db    db .d8888. d888888b  .d88b.  .88b  d88.      d888888b db    db d8888b. d88888b
   d8P  Y8 88    88 88'  YP `~~88~~' .8P  Y8. 88'YbdP`88      `~~88~~' `8b  d8' 88  `8D 88'
   8P      88    88 `8bo.      88    88    88 88  88  88         88     `8bd8'  88oodD' 88ooooo
   8b      88    88   `Y8b.    88    88    88 88  88  88         88       88    88~~~   88~~~~~
   Y8b  d8 88b  d88 db   8D    88    `8b  d8' 88  88  88         88       88    88      88.
    `Y88P' ~Y8888P' `8888Y'    YP     `Y88P'  YP  YP  YP         YP       YP    88      Y88888P
-}


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
            |> tag2 "TwoArgs" TwoArgs ( "Int", int ) ( "Float", float )
            |> end

-}
customType :
    b
    ->
        Builder
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
customType destructor =
    Cus
        { index = 0
        , labels = []
        , fns = \_ x -> x
        , states = \_ x -> x
        , updater = identity
        , viewer = identity
        , parser = identity
        , idleSetter = identity
        , deltaBefore = identity
        , deltaBefores = identity
        , deltaAfter = End
        , deltaAfters = End
        , makeDeltaSetters = identity
        , stateBefore = identity
        , stateBefores = identity
        , toArgStates = identity
        , stateAfter = End
        , stateAfters = End
        , makeStateSetters = identity
        , stateInserter = identity
        , applyInputs = identity
        , flagEmitter = identity
        , errorCollector = identity
        , debouncingReceiverCollector = identity
        , destructor = destructor
        }


tagHelper label_ (Control control) toArgState builder =
    case builder of
        Rec r ->
            Rec r

        Cus rec ->
            Cus
                { index = rec.index + 1
                , labels = label_ :: rec.labels
                , fns =
                    \path ->
                        let
                            control_ =
                                control (Path.add label_ path)
                        in
                        rec.fns path
                            << Tuple.pair
                                { control_ | index = rec.index }
                , states =
                    \path ->
                        rec.states path
                            << Tuple.pair (control (Path.add label_ path) |> .init)
                , updater = rec.updater >> customTypeStateUpdater
                , viewer = rec.viewer >> selectedTagViewer
                , parser = rec.parser >> selectedTagParser
                , idleSetter = rec.idleSetter >> selectedTagIdleSetter
                , deltaBefore = rec.deltaBefore << Tuple.pair Skip
                , deltaBefores = rec.deltaBefores << Tuple.pair rec.deltaBefore
                , deltaAfter = ( Skip, rec.deltaAfter )
                , deltaAfters = ( rec.deltaAfter, rec.deltaAfters )
                , makeDeltaSetters = rec.makeDeltaSetters >> deltaSetterMaker
                , stateBefore = rec.stateBefore << Tuple.pair Nothing
                , stateBefores = rec.stateBefores << Tuple.pair rec.stateBefore
                , toArgStates = rec.toArgStates << Tuple.pair toArgState
                , stateAfter = ( Nothing, rec.stateAfter )
                , stateAfters = ( rec.stateAfter, rec.stateAfters )
                , makeStateSetters = rec.makeStateSetters >> stateSetterMaker
                , stateInserter = rec.stateInserter >> argStateIntoTagStateInserter
                , applyInputs = rec.applyInputs >> stateSetterToInitialiserApplier
                , flagEmitter = rec.flagEmitter >> customTypeFlagEmitter
                , errorCollector = rec.errorCollector >> customTypeErrorCollector
                , debouncingReceiverCollector = rec.debouncingReceiverCollector >> customTypeDebouncingReceiverCollector
                , destructor = rec.destructor
                }


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
tag0 label_ tag =
    tagHelper
        label_
        (null tag)
        (\insertArgStateIntoTagStates ->
            insertArgStateIntoTagStates tag
        )


null : tag -> Control () () tag
null tag =
    create
        { initEmpty = ()
        , initWith = \_ -> ()
        , update = \() () -> ()
        , view = \_ -> H.text ""
        , parse = \() -> Ok tag
        }


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
tag1 label_ tag control =
    tagHelper
        label_
        (record tag
            |> field "" Tuple.first control
            |> end
        )
        (\insertArgStateIntoTagStates arg1 ->
            insertArgStateIntoTagStates ( arg1, End )
        )


{-| Add a tag with two arguments to a custom type.

    type Point
        = Point Float Float

    pointControl =
        customType
            (\point tag ->
                case tag of
                    Point x y ->
                        point x y
            )
            |> tag2 Point ( "X", float ) ( "Y", float )

-}
tag2 label_ tag ( label1, control1 ) ( label2, control2 ) =
    tagHelper
        label_
        (record tag
            |> field label1 Tuple.first control1
            |> field label2 (Tuple.second >> Tuple.first) control2
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, End ) )
        )


{-| Add a tag with three arguments to a custom type.

    type Point3D
        = Point3D Float Float Float

    point3DControl =
        customType
            (\point3D tag ->
                case tag of
                    Point3D x y z ->
                        point3D x y z
            )
            |> tag3 Point3D ( "X", float ) ( "Y", float ) ( "Z", float )

-}
tag3 label_ tag ( label1, control1 ) ( label2, control2 ) ( label3, control3 ) =
    tagHelper
        label_
        (record tag
            |> field label1 Tuple.first control1
            |> field label2 (Tuple.second >> Tuple.first) control2
            |> field label3 (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, End ) ) )
        )


{-| Add a tag with four arguments to a custom type.
-}
tag4 label_ tag ( label1, control1 ) ( label2, control2 ) ( label3, control3 ) ( label4, control4 ) =
    tagHelper
        label_
        (record tag
            |> field label1 Tuple.first control1
            |> field label2 (Tuple.second >> Tuple.first) control2
            |> field label3 (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> field label4 (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 arg4 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, ( arg4, End ) ) ) )
        )


{-| Add a tag with five arguments to a custom type.
-}
tag5 label_ tag ( label1, control1 ) ( label2, control2 ) ( label3, control3 ) ( label4, control4 ) ( label5, control5 ) =
    tagHelper
        label_
        (record tag
            |> field label1 Tuple.first control1
            |> field label2 (Tuple.second >> Tuple.first) control2
            |> field label3 (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> field label4 (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> field label5 (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control5
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 arg4 arg5 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, ( arg4, ( arg5, End ) ) ) ) )
        )


endCustomType rec =
    Control
        (\path ->
            let
                fns =
                    rec.fns path End

                inits =
                    rec.states path End

                deltaSetters =
                    makeDeltaSetters rec.makeDeltaSetters rec.deltaBefores rec.deltaAfters

                stateSetters =
                    makeStateSetters rec.makeStateSetters rec.stateInserter inits fns rec.toArgStates rec.stateBefores rec.stateAfters

                labels =
                    List.reverse rec.labels

                update =
                    \delta (State i state) ->
                        case delta of
                            Skip ->
                                ( State i state, Cmd.none )

                            TagSelected idx ->
                                ( State { i | selected = idx } state, Cmd.none )

                            ChangeState tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates rec.updater fns deltaSetters tagDelta state
                                in
                                ( State i newTagStates
                                , Cmd.map ChangeState cmd
                                )

                            _ ->
                                ( State i state, Cmd.none )

                childView _ dynamicConfig =
                    viewSelectedTagState rec.viewer dynamicConfig.flags dynamicConfig.selected fns deltaSetters dynamicConfig.state

                view staticConfig dynamicConfig =
                    let
                        options =
                            List.indexedMap Tuple.pair labels
                    in
                    customTypeView path staticConfig options dynamicConfig.selected (childView staticConfig dynamicConfig)

                parse =
                    \(State i state) -> validateSelectedTagState rec.parser i.selected fns state

                setAllIdle =
                    \(State i state) -> State { i | status = Idle_ } (setSelectedTagStateIdle rec.idleSetter i.selected fns state)

                emitFlags (State i state) =
                    emitFlagsForCustomType rec.flagEmitter i.selected fns state
            in
            { path = path
            , index = 0
            , init = State { status = Intact_, selected = 0 } inits
            , initWith = applyStateSettersToInitialiser rec.applyInputs rec.destructor stateSetters
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \staticConfig dynamicConfig -> [ childView staticConfig dynamicConfig ]
            , view = \staticConfig dynamicConfig -> view staticConfig dynamicConfig
            , parse = parse
            , setAllIdle = setAllIdle
            , emitFlags = emitFlags
            , collectErrors = \(State _ states) flags -> collectErrorsForCustomType rec.errorCollector flags fns states
            , receiverCount = 0
            , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForCustomType rec.debouncingReceiverCollector fns states
            , label = Nothing
            , id = Nothing
            , name = Nothing
            , class = []
            }
        )



{-
    .o88b. db    db .d8888. d888888b  .d88b.  .88b  d88.      d888888b d8b   db d888888b d88888b d8888b. d8b   db  .d8b.  db      .d8888.
   d8P  Y8 88    88 88'  YP `~~88~~' .8P  Y8. 88'YbdP`88        `88'   888o  88 `~~88~~' 88'     88  `8D 888o  88 d8' `8b 88      88'  YP
   8P      88    88 `8bo.      88    88    88 88  88  88         88    88V8o 88    88    88ooooo 88oobY' 88V8o 88 88ooo88 88      `8bo.
   8b      88    88   `Y8b.    88    88    88 88  88  88         88    88 V8o88    88    88~~~~~ 88`8b   88 V8o88 88~~~88 88        `Y8b.
   Y8b  d8 88b  d88 db   8D    88    `8b  d8' 88  88  88        .88.   88  V888    88    88.     88 `88. 88  V888 88   88 88booo. db   8D
    `Y88P' ~Y8888P' `8888Y'    YP     `Y88P'  YP  YP  YP      Y888888P VP   V8P    YP    Y88888P 88   YD VP   V8P YP   YP Y88888P `8888Y'
-}


collectDebouncingReceiversForCustomType :
    ((List Flag
      -> End
      -> End
      -> List Flag
     )
     -> List Flag
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> List Flag
    )
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Flag
collectDebouncingReceiversForCustomType debouncingReceiverCollector_ fns states =
    debouncingReceiverCollector_ (\receivers End End -> receivers) [] fns states


customTypeDebouncingReceiverCollector :
    (List Flag
     -> restFns
     -> restStates
     -> List Flag
    )
    -> List Flag
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Flag
customTypeDebouncingReceiverCollector next receivers ( fns, restFns ) ( state, restStates ) =
    next (receivers ++ fns.collectDebouncingReceivers state) restFns restStates


updateCustomTypeStates :
    (({ newStates : End -> states, newCmds : List (Cmd msg) }
      -> End
      -> End
      -> End
      -> End
      -> { newStates : End -> states, newCmds : List (Cmd msg) }
     )
     -> { newStates : states -> states, newCmds : List (Cmd msg) }
     -> ( ControlFns input state delta output, restFns )
     -> ( setter, restSetters )
     -> ( Delta delta, restDeltas )
     -> ( State state, restStates )
     -> { newStates : End -> states, newCmds : List (Cmd msg) }
    )
    -> ( ControlFns input state delta output, restFns )
    -> ( setter, restSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> ( states, Cmd msg )
updateCustomTypeStates updater fields setters deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output End End End End -> output)
                { newStates = identity, newCmds = [] }
                fields
                setters
                deltas
                states
    in
    ( newStates End, Cmd.batch newCmds )


customTypeStateUpdater :
    ({ newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) }
     -> restFns
     -> restDeltaSetters
     -> restDeltas
     -> restStates
     -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
    )
    -> { newStates : ( State state, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) }
    -> ( ControlFns input state delta output, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
customTypeStateUpdater next { newStates, newCmds } ( fns, restFns ) ( deltaSetter, restDeltaSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        ( newState, newCmd ) =
            fns.update delta state

        cmd2 =
            newCmd
                |> Cmd.map deltaSetter
    in
    next
        { newStates = newStates << Tuple.pair newState
        , newCmds = cmd2 :: newCmds
        }
        restFns
        restDeltaSetters
        restDeltas
        restStates


collectErrorsForCustomType :
    ((List Flag
      -> List ( String, String )
      -> End
      -> End
      -> List ( String, String )
     )
     -> List Flag
     -> List ( String, String )
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> List ( String, String )
    )
    -> List Flag
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List ( String, String )
collectErrorsForCustomType errorCollector_ flags fns states =
    errorCollector_ (\_ errors End End -> errors) flags [] fns states


customTypeErrorCollector :
    (List Flag
     -> List ( String, String )
     -> restFns
     -> restStates
     -> List ( String, String )
    )
    -> List Flag
    -> List ( String, String )
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List ( String, String )
customTypeErrorCollector next flags errors ( fns, restFns ) ( state, restStates ) =
    next flags (errors ++ fns.collectErrors state flags) restFns restStates


emitFlagsForCustomType :
    ((List Flag
      -> Int
      -> End
      -> End
      -> List Flag
     )
     -> List Flag
     -> Int
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> List Flag
    )
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Flag
emitFlagsForCustomType flagEmitter_ selectedTag fns tagStates =
    flagEmitter_ (\flags _ End End -> flags) [] selectedTag fns tagStates


customTypeFlagEmitter :
    (List Flag
     -> Int
     -> restFns
     -> restStates
     -> List Flag
    )
    -> List Flag
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Flag
customTypeFlagEmitter next flags selectedTag ( fns, restFns ) ( tagState, restTagStates ) =
    let
        newFlags =
            if fns.index == selectedTag then
                fns.emitFlags tagState

            else
                []
    in
    next (flags ++ newFlags) selectedTag restFns restTagStates


applyStateSettersToInitialiser :
    ((state1
      -> End
      -> state1
     )
     -> (stateSetter -> state0)
     -> ( stateSetter, restStateSetters )
     -> tag
     -> state2
    )
    -> (stateSetter -> state0)
    -> ( stateSetter, restStateSetters )
    -> tag
    -> state2
applyStateSettersToInitialiser stateSetterToInitialiserApplier_ initialiser stateSetters tag =
    stateSetterToInitialiserApplier_
        (\appliedInitialiser End -> appliedInitialiser)
        initialiser
        stateSetters
        tag


stateSetterToInitialiserApplier :
    (state0
     -> restStateSetters
     -> state1
    )
    -> (stateSetter -> state0)
    -> ( stateSetter, restStateSetters )
    -> state1
stateSetterToInitialiserApplier next initialiser ( stateSetter, stateSetters ) =
    next (initialiser stateSetter) stateSetters


makeStateSetters :
    ((a
      -> b
      -> End
      -> End
      -> End
      -> End
      -> End
     )
     -> c
     -> d
     -> e
     -> f
     -> g
     -> h
     -> i
    )
    -> c
    -> d
    -> e
    -> (End -> f)
    -> (End -> g)
    -> h
    -> i
makeStateSetters makeSetters_ argStateIntoTagStateInserter_ inits fns toArgStates befores afters =
    makeSetters_ (\_ _ End End End End -> End) argStateIntoTagStateInserter_ inits fns (toArgStates End) (befores End) afters


stateSetterMaker :
    (((Int
       -> Int
       -> (End -> state)
       -> End
       -> End
       -> State state
      )
      -> Int
      -> Int
      -> (c -> c)
      -> ( Maybe (State argState), restMaybeArgStates )
      -> ( State argState, restInits )
      -> State tagStates
     )
     -> ( State argState, restInits )
     -> restFns
     -> restToArgStates
     -> befores
     -> afters
     -> restStateSetters
    )
    ->
        ((Int
          -> Int
          -> (End -> state)
          -> End
          -> End
          -> State state
         )
         -> Int
         -> Int
         -> (c -> c)
         -> ( Maybe (State argState), restMaybeArgStates )
         -> ( State argState, restInits )
         -> State tagStates
        )
    -> ( State argState, restInits )
    -> ( ControlFns fieldInput fieldState delta output, restFns )
    -> ( (fieldInput -> State tagStates) -> stateSetter, restToArgStates )
    -> ( ( Maybe (State fieldState), after ) -> ( Maybe (State argState), restMaybeArgStates ), befores )
    -> ( after, afters )
    -> ( stateSetter, restStateSetters )
stateSetterMaker next argStateIntoTagStateInserter_ inits ( fns, restFns ) ( toArgState, restToArgStates ) ( before, befores ) ( after, afters ) =
    ( toArgState
        (\argState ->
            let
                maybes =
                    before ( Just (fns.initWith argState), after )
            in
            insertArgStateIntoTagState argStateIntoTagStateInserter_ maybes inits
        )
    , next argStateIntoTagStateInserter_ inits restFns restToArgStates befores afters
    )


insertArgStateIntoTagState :
    ((Int
      -> Int
      -> (End -> argStates)
      -> End
      -> End
      -> State argStates
     )
     -> Int
     -> Int
     -> (c -> c)
     -> ( Maybe (State argState), restMaybeArgStates )
     -> ( State argState, restInits )
     -> State tagStates
    )
    -> ( Maybe (State argState), restMaybeArgStates )
    -> ( State argState, restInits )
    -> State tagStates
insertArgStateIntoTagState stateInserter_ maybeArgStates inits =
    stateInserter_
        (\_ selectedTag tagStates End End -> State { status = Intact_, selected = selectedTag } (tagStates End))
        0
        0
        identity
        maybeArgStates
        inits


argStateIntoTagStateInserter :
    (Int
     -> Int
     -> (restArgStates -> tagStates)
     -> restMaybeArgStates
     -> restArgStates
     -> State tagStates
    )
    -> Int
    -> Int
    -> (( State argState, restArgStates ) -> tagStates)
    -> ( Maybe (State argState), restMaybeArgStates )
    -> ( State argState, restArgStates )
    -> State tagStates
argStateIntoTagStateInserter next thisTagIndex currentlySelectedTagIndex tagStates ( maybeArgState, restMaybeArgStates ) ( init, restInits ) =
    let
        ( selectedTag, tagArgState ) =
            case maybeArgState of
                Just state ->
                    ( thisTagIndex, state )

                Nothing ->
                    ( currentlySelectedTagIndex, init )
    in
    next (thisTagIndex + 1) selectedTag (tagStates << Tuple.pair tagArgState) restMaybeArgStates restInits


setSelectedTagStateIdle :
    ((Int
      -> End
      -> End
      -> End
     )
     -> Int
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> ( State state, restStates )
    )
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> ( State state, restStates )
setSelectedTagStateIdle idleSetter_ selectedTag fns tagStates =
    idleSetter_
        (\_ End End -> End)
        selectedTag
        fns
        tagStates


selectedTagIdleSetter :
    (Int
     -> restFns
     -> restStates
     -> restStates
    )
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> ( State state, restStates )
selectedTagIdleSetter next selectedTag ( fns, restFns ) ( state, restStates ) =
    ( if fns.index == selectedTag then
        fns.setAllIdle state

      else
        state
    , next selectedTag restFns restStates
    )


validateSelectedTagState :
    ((a
      -> b
      -> End
      -> End
      -> a
     )
     -> Result (List ( String, String )) value
     -> Int
     -> c
     -> d
     -> e
    )
    -> Int
    -> c
    -> d
    -> e
validateSelectedTagState parser selectedTag fns states =
    parser
        (\result_ _ End End -> result_)
        (Err [ ( "FATAL ERROR", "tag index " ++ String.fromInt selectedTag ++ " not found" ) ])
        selectedTag
        fns
        states


selectedTagParser :
    (Result (List ( String, String )) output
     -> Int
     -> restFns
     -> restStates
     -> Result (List ( String, String )) output
    )
    -> Result (List ( String, String )) output
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> Result (List ( String, String )) output
selectedTagParser next result_ selectedTag ( fns, restFns ) ( state, restStates ) =
    next
        (if fns.index == selectedTag then
            fns.parse state

         else
            result_
        )
        selectedTag
        restFns
        restStates


viewSelectedTagState :
    ((Maybe (Html delta1)
      -> List Flag
      -> Int
      -> End
      -> End
      -> End
      -> Maybe (Html delta1)
     )
     -> Maybe (Html delta1)
     -> List Flag
     -> Int
     -> ( ControlFns input state delta0 output, restFns )
     -> ( Delta delta0 -> delta1, restSetters )
     -> ( State state, restStates )
     -> Maybe (Html delta1)
    )
    -> List Flag
    -> Int
    -> ( ControlFns input state delta0 output, restFns )
    -> ( Delta delta0 -> delta1, restSetters )
    -> ( State state, restStates )
    -> Html (Delta delta1)
viewSelectedTagState viewer flags selectedTag fns setters states =
    viewer (\maybeView _ _ End End End -> maybeView) Nothing flags selectedTag fns setters states
        |> Maybe.map (H.map ChangeState)
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer :
    (Maybe (Html delta1)
     -> List Flag
     -> Int
     -> restFns
     -> restSetters
     -> restStates
     -> Maybe (Html delta1)
    )
    -> Maybe (Html delta1)
    -> List Flag
    -> Int
    -> ( ControlFns input state delta0 output, restFns )
    -> ( Delta delta0 -> delta1, restSetters )
    -> ( State state, restStates )
    -> Maybe (Html delta1)
selectedTagViewer next maybeView flags selectedTag ( fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    next
        (if fns.index == selectedTag then
            Just
                (fns.view
                    { id = fns.id
                    , label = fns.label
                    , name = fns.name
                    , class = fns.class
                    }
                    { state = state
                    , status = Intact
                    , flags = flags
                    , selected = internalState.selected
                    }
                    |> H.map setter
                )

         else
            maybeView
        )
        flags
        selectedTag
        restFns
        restSetters
        restStates



{-
   db    db d888888b d88888b db   d8b   db .d8888.
   88    88   `88'   88'     88   I8I   88 88'  YP
   Y8    8P    88    88ooooo 88   I8I   88 `8bo.
   `8b  d8'    88    88~~~~~ Y8   I8I   88   `Y8b.
    `8bd8'    .88.   88.     `8b d8'8b d8' db   8D
      YP    Y888888P Y88888P  `8b8' `8d8'  `8888Y'
-}


wrappedView : Status -> Html delta -> Html delta
wrappedView status innerView =
    H.div
        [ HA.class
            (case status of
                Intact ->
                    "control-intact"

                Debouncing ->
                    "control-debouncing"

                Idle (_ :: _) ->
                    "control-invalid"

                Idle _ ->
                    "control-valid"
            )
        ]
        [ innerView
        , case status of
            Idle [] ->
                H.text ""

            Idle feedback ->
                H.div []
                    (List.map
                        (\f ->
                            let
                                ( icon, txt ) =
                                    case f of
                                        Ok ( _, t ) ->
                                            ( "💬", t )

                                        Err ( _, t ) ->
                                            ( "❌", t )
                            in
                            H.div [] [ H.text (icon ++ " " ++ txt) ]
                        )
                        feedback
                    )

            _ ->
                H.text ""
        ]


customTypeView :
    Path
    -> ViewConfigStatic
    -> List ( Int, String )
    -> Int
    -> Html (Delta variants)
    -> Html (Delta variants)
customTypeView path staticConfig options selectedTag selectedTagView =
    if List.length options > 1 then
        H.div []
            [ radioView
                { options = options
                , selectedOption = selectedTag
                , toMsg = TagSelected
                , id = Maybe.withDefault (Path.toString path) staticConfig.id
                , name = Maybe.withDefault (Path.toString path) staticConfig.name
                , label = Maybe.withDefault (Path.last path) staticConfig.label
                }
            , selectedTagView
            ]

    else
        selectedTagView


button : msg -> String -> Html msg
button msg text =
    H.button
        [ HA.type_ "button"
        , HE.onClick msg
        ]
        [ H.text text ]


listView :
    Path
    -> ViewConfigStatic
    -> ViewConfigDynamic (List (State state))
    -> List Flag
    -> (Path -> ControlFns input state delta output)
    -> Html (Delta (ListDelta delta))
listView path staticConfig dynamicConfig debouncingReceivers ctrl =
    let
        id_ =
            Maybe.withDefault (Path.toString path) staticConfig.id
    in
    H.div
        [ HA.id id_ ]
        [ H.label
            [ HA.for id_ ]
            [ H.text (Maybe.withDefault (Path.last path) staticConfig.label) ]
        , if List.isEmpty dynamicConfig.state then
            button (ChangeState (InsertItem 0)) "Add item"

          else
            H.ol []
                (List.indexedMap
                    (\idx (State internalState state) ->
                        let
                            control =
                                ctrl (Path.add (String.fromInt idx) path)

                            filteredFlags1 =
                                List.filterMap
                                    (\flag ->
                                        case flag of
                                            FlagList flagPath flagLabel flagIndexes ->
                                                if flagPath == path then
                                                    if List.member idx flagIndexes then
                                                        Just (FlagLabel flagLabel)

                                                    else
                                                        Nothing

                                                else
                                                    Just flag

                                            _ ->
                                                Just flag
                                    )
                                    dynamicConfig.flags

                            filteredFlags2 =
                                List.filter (\f -> not <| List.member f debouncingReceivers) filteredFlags1
                        in
                        H.li []
                            [ control.view
                                { id = control.id
                                , name = control.name
                                , label = control.label
                                , class = control.class
                                }
                                { state = state
                                , status = getStatus control.parse control.collectErrors filteredFlags2 (State internalState state)
                                , flags = filteredFlags2
                                , selected = internalState.selected
                                }
                                |> H.map (ChangeItem idx)
                            , button (DeleteItem idx) "Delete item"
                            , H.div [] [ button (InsertItem (idx + 1)) "Insert item" ]
                            ]
                    )
                    dynamicConfig.state
                )
                |> H.map ChangeState
        ]


textControlView : String -> { state : String, id : String, label : String, name : String, class : String } -> Html String
textControlView type_ config =
    H.div []
        [ H.label [ HA.for config.id ] [ H.text config.label ]
        , H.input
            [ HE.onInput identity
            , HA.name config.name
            , HA.id config.id
            , HA.class config.class
            , HA.type_ type_
            , HA.value config.state
            ]
            [ H.text config.state ]
        ]


enumView : List ( String, enum ) -> { state : enum, id : String, label : String, name : String, class : String } -> Html enum
enumView tags config =
    radioView
        { options = List.map (\( a, b ) -> ( b, a )) tags
        , selectedOption = config.state
        , toMsg = identity
        , id = config.id
        , label = config.label
        , name = config.name
        }


radioView :
    { options : List ( option, String )
    , selectedOption : option
    , toMsg : option -> msg
    , id : String
    , label : String
    , name : String
    }
    -> Html msg
radioView config =
    H.fieldset
        [ HA.id config.id ]
        (H.legend [] [ H.text config.label ]
            :: List.map
                (\( option, optionLabel ) ->
                    H.div []
                        [ H.input
                            [ HA.type_ "radio"
                            , HA.name config.name
                            , HA.id optionLabel
                            , HA.value optionLabel
                            , HA.checked (config.selectedOption == option)
                            , onChecked (config.toMsg option)
                            ]
                            []
                        , H.label [ HA.for optionLabel ] [ H.text optionLabel ]
                        ]
                )
                config.options
        )


onChecked : msg -> H.Attribute msg
onChecked msg =
    HE.on "input"
        (HE.targetChecked
            |> Json.Decode.andThen
                (\checked ->
                    if checked then
                        Json.Decode.succeed msg

                    else
                        Json.Decode.fail ""
                )
        )
