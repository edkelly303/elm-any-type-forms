module Control exposing
    ( Control, Form, sandbox, form
    , bool, int, float, string, char, enum
    , tuple, triple, maybe, result, list, dict, set, array, map
    , ControlConfig, create
    , failIf, noteIf
    , alertIf, respond
    , alertAtIndexes
    , initWith, debounce, id, name, label, class, classList, wrapView
    , record, field, hiddenField, readOnlyField, end, layout
    , customType, tag0, tag1, tag2, tag3, tag4, tag5
    , State, Delta, ListDelta, End
    , Access, AdvancedControl, Builder, ControlFns, Alert, RecordFns, Status, ViewConfigStatic, ViewConfigDynamic, Path, Feedback
    )

{-|


# Creating a form

@docs Control, Form, sandbox, form


# Basic controls

@docs bool, int, float, string, char, enum


# Basic combinators

@docs tuple, triple, maybe, result, list, dict, set, array, map


# Creating a new control

@docs ControlConfig, create


# Validating controls


## Simple validation

@docs failIf, noteIf


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
            |> field
                (string
                    |> respond
                        { alert = "passwords-do-not-match"
                        , fail = True
                        , message = "Must match 'Confirm password' field"
                        }
                )
            |> field
                (string
                    |> respond
                        { alert = "passwords-do-not-match"
                        , fail = True
                        , message = "Must match 'Enter password' field"
                        }
                )
            |> end
            |> alertIf (\{ password, confirmation } -> password /= confirmation) "passwords-do-not-match"

@docs alertIf, respond


## List validation

@docs alertAtIndexes


# Configuring controls

@docs initWith, debounce, id, name, label, class, classList, wrapView


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

@docs Access, AdvancedControl, Builder, ControlFns, Alert, RecordFns, Status, ViewConfigStatic, ViewConfigDynamic, Path, Feedback

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


fixme : String -> a -> a
fixme _ a =
    a



{-
   d888888b db    db d8888b. d88888b .d8888.
   `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
      88     `8bd8'  88oodD' 88ooooo `8bo.
      88       88    88~~~   88~~~~~   `Y8b.
      88       88    88      88.     db   8D
      YP       YP    88      Y88888P `8888Y'
-}


{-| A `Form` is an interface containing functions that you can use in your
program to initialise, view, update, subscribe, and submit your form.
-}
type alias Form state delta output msg =
    { init : ( State state, Cmd msg )
    , initWith : output -> ( State state, Cmd msg )
    , update : Delta delta -> State state -> ( State state, Cmd msg )
    , view : State state -> Html msg
    , subscriptions : State state -> Sub msg
    , submit : State state -> ( State state, Result (List Feedback) output )
    }


{-| Configuration for a custom control. You need to supply the following fields:

  - `initEmpty`: a default initial "empty" value for the `state` of the control. For a control whose `state` is of type `String`, this might be `""`, for a number it might be `0`.
  - `initWith`: a function that initialises the control's `state` from a value of the control's `output` type. For a control that outputs an `Int`, but whose `state` is a `String`, this might be `String.fromInt`.
  - `update`: a function that updates the `state` of the control based on its existing `state` and a `delta` type that it receives. This is exactly analogous to an Elm program's update function, where `state` = `Model` and `delta` = `Msg`.
  - `view`: a function that outputs `Html delta` based on the `state` of the control. This is similar to an Elm program's view function, except instead of just taking the `state` as an argument, it takes a record containing the `state` plus the id, name, class, and label for the input.
  - `subscriptions`: a function that outputs `Sub delta` based on the `state` of the control. This is exactly analogous to an Elm program's subscriptions function.
  - `parse`: a function that attempts to parse the control's state into its output type, producing a result. If parsing fails, it can provide a list of errors.

-}
type alias ControlConfig state delta output =
    { initEmpty : ( state, Cmd delta )
    , initWith : output -> ( state, Cmd delta )
    , update : delta -> state -> ( state, Cmd delta )
    , view : { label : String, id : String, name : String, class : String, state : state } -> List (Html delta)
    , subscriptions : state -> Sub delta
    , parse : state -> Result (List String) output
    , label : String
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
type RecordFns input state delta output recordOutput
    = RecordFns
        { access : Access
        , field : ControlFns input state delta output
        , fromInput : recordOutput -> output
        }


{-| Some internal stuff needed to build up record and custom type controls
-}
type ControlFns input state delta output
    = ControlFns
        { index : Int
        , init : ( State state, Cmd (Delta delta) )
        , initWith : input -> ( State state, Cmd (Delta delta) )
        , baseUpdate : Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
        , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
        , view : ViewConfigStatic -> ViewConfigDynamic state -> List (Html (Delta delta))
        , childViews : ViewConfigStatic -> ViewConfigDynamic state -> List (Html (Delta delta))
        , parse : State state -> Result (List Feedback) output
        , path : Path
        , emitAlerts : State state -> List Alert
        , collectDebouncingReceivers : State state -> List Alert
        , collectErrors : State state -> List Alert -> List Feedback
        , receiverCount : Int
        , setAllIdle : State state -> State state
        , label : String
        , id : Maybe String
        , name : Maybe String
        , class : List String
        , subscriptions : State state -> Sub (Delta delta)
        }


{-| Some internal stuff needed to handle validations
-}
type Alert
    = AlertLabel String
    | AlertPath Path Int
    | AlertList Path String (List Int)


type alias Feedback =
    { path : Path
    , label : String
    , message : String
    , fail : Bool
    }


{-| Some internal stuff needed to view controls
-}
type alias ViewConfigDynamic state =
    { state : state
    , status : Status
    , alerts : List Alert
    , selected : Int
    }


{-| Some internal stuff needed to view controls
-}
type alias ViewConfigStatic =
    { name : Maybe String
    , id : Maybe String
    , label : String
    , class : List String
    }


{-| an internal type needed to represent the validation state of a control
-}
type Status
    = Intact
    | Debouncing
    | Idle (List Feedback)


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
    | ChangeStateOnInput delta
    | ChangeStateInternally delta
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


{-| Convert a `Control` into a `Form`, which you can then plumb into your Elm application. You will need to supply a
couple of variants from your app's `Msg` type: one to handle updates of the form's state, and one to handle the
submission of the form.

    type alias Msg
        = FormUpdated (Delta String)
        | FormSubmitted

    myForm : Form (State String) (Delta String) Int Msg
    myForm =
        form
            { control = int
            , onUpdate = FormUpdated
            , onSubmit = FormSubmitted
            }

-}
form : { control : Control state delta output, onUpdate : Delta delta -> msg, onSubmit : msg } -> Form state delta output msg
form { onUpdate, onSubmit, control } =
    let
        (Control c) =
            control

        (ControlFns fns) =
            c Path.root
    in
    { init = fns.init |> Tuple.mapSecond (Cmd.map onUpdate)
    , initWith =
        \output ->
            let
                (Control initialisedControl) =
                    initWith output control

                (ControlFns fns2) =
                    initialisedControl Path.root
            in
            fns2.init
                |> Tuple.mapSecond (Cmd.map onUpdate)
    , update =
        \msg state ->
            fns.update msg state
                |> Tuple.mapSecond (Cmd.map onUpdate)
    , view =
        \((State internalState state) as s) ->
            let
                emittedAlerts =
                    fns.emitAlerts s

                debouncingReceivers =
                    fns.collectDebouncingReceivers s

                alerts =
                    List.filter (\f -> not <| List.member f debouncingReceivers) emittedAlerts
            in
            H.form [ HE.onSubmit onSubmit ]
                ((fns.view
                    { id = fns.id
                    , name = fns.name
                    , label = fns.label
                    , class = fns.class
                    }
                    { state = state
                    , status = getStatus fns.parse fns.collectErrors alerts (State internalState state)
                    , alerts = alerts
                    , selected = internalState.selected
                    }
                    |> List.map (H.map onUpdate)
                 )
                    ++ [ H.input [ HA.type_ "submit", HE.onClick onSubmit ] [ H.text "Submit" ] ]
                )
    , submit =
        \state ->
            let
                parsingResult =
                    fns.parse state

                validationErrors =
                    fns.emitAlerts state
                        |> fns.collectErrors state
                        |> List.filter .fail
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
    , subscriptions =
        \state ->
            fns.subscriptions state
                |> Sub.map onUpdate
    }


{-| Test and debug a `Control` by turning it into a `Program` that you can run as a standalone Elm application.

This is useful when you're rapidly iterating on designing a form, and you don't yet want the hassle of plumbing it into
your main Elm application's `Model`and `Msg` types.

Once you're happy with the form, you can then ask the Elm repl (or your editor's Elm plugin) to tell you the type
signature of the `Program`, which will give you the form's `State` and `Delta` types. You can then plug these into your
main `Model`and `Msg` types wherever appropriate.

    main : Program () (State String) (Delta String)
    main =
        sandbox
            { control = int
            , outputToString = Debug.toString
            }

-}
sandbox : { outputToString : output -> String, control : Control state delta output } -> Program () (State state) (Delta delta)
sandbox { outputToString, control } =
    let
        (Control c) =
            control

        (ControlFns fns) =
            c Path.root
    in
    Browser.element
        { init =
            \() ->
                fns.init
        , update =
            \msg state ->
                fns.update msg state
        , view =
            \((State internalState state) as s) ->
                let
                    emittedAlerts =
                        fns.emitAlerts s

                    debouncingReceivers =
                        fns.collectDebouncingReceivers s

                    alerts =
                        List.filter (\f -> not <| List.member f debouncingReceivers) emittedAlerts

                    parsingResult =
                        fns.parse s

                    validationErrors =
                        fns.emitAlerts s
                            |> fns.collectErrors s
                            |> List.filter .fail

                    failView errs =
                        H.div []
                            [ H.p [] [ H.text "Failure! Your form has errors on the following fields:" ]
                            , H.ul [] (List.map (\{ path, message } -> H.li [] [ H.text (Path.toString path ++ ": " ++ message) ]) errs)
                            ]
                in
                H.div []
                    [ H.h1 [] [ H.text "Form" ]
                    , H.form []
                        (fns.view
                            { id = fns.id
                            , name = fns.name
                            , label = fns.label
                            , class = fns.class
                            }
                            { state = state
                            , status = getStatus fns.parse fns.collectErrors alerts (State internalState state)
                            , alerts = alerts
                            , selected = internalState.selected
                            }
                        )
                    , H.h2 [] [ H.text "Output" ]
                    , case ( parsingResult, validationErrors ) of
                        ( Ok output, [] ) ->
                            H.div []
                                [ H.p [] [ H.text "Success! Your form produced the following value:" ]
                                , H.pre [] [ H.text (outputToString output) ]
                                ]

                        ( Ok _, vErrs ) ->
                            failView vErrs

                        ( Err errs, vErrs ) ->
                            failView (errs ++ vErrs)
                    ]
        , subscriptions = fns.subscriptions
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
            , update =
                \delta state ->
                    case delta of
                        Increment ->
                            state + 1

                        Decrement ->
                            state - 1
            , view =
                \{ state, name, id, label, class } ->
                    Html.div [ Html.Attributes.class class ]
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
            , subscriptions = \state -> Sub.none
            , parse = Ok
            }

-}
create : ControlConfig state delta output -> Control state delta output
create config =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate config.update

                parse =
                    \(State _ state) ->
                        config.parse state
                            |> Result.mapError
                                (List.map
                                    (\message ->
                                        { message = message
                                        , label = config.label
                                        , path = path
                                        , fail = True
                                        }
                                    )
                                )
            in
            ControlFns
                { path = path
                , index = 0
                , init =
                    config.initEmpty
                        |> Tuple.mapFirst (State { status = Intact_, selected = 0 })
                        |> Tuple.mapSecond (Cmd.map ChangeStateInternally)
                , initWith =
                    \input ->
                        config.initWith input
                            |> Tuple.mapFirst (State { status = Intact_, selected = 0 })
                            |> Tuple.mapSecond (Cmd.map ChangeStateInternally)
                , baseUpdate = preUpdate
                , update = preUpdate 0
                , childViews = \_ _ -> []
                , view =
                    \staticConfig dynamicConfig ->
                        [ config.view
                            { state = dynamicConfig.state
                            , label = staticConfig.label
                            , id = Maybe.withDefault (Path.toString path) staticConfig.id
                            , name = Maybe.withDefault (Path.toString path) staticConfig.name
                            , class = String.join " " staticConfig.class
                            }
                            |> wrappedView dynamicConfig.status
                            |> H.map ChangeStateOnInput
                        ]
                , parse = parse
                , setAllIdle = \(State i s) -> State { i | status = Idle_ } s
                , emitAlerts = \_ -> []
                , collectDebouncingReceivers = \_ -> []
                , collectErrors = \_ _ -> []
                , receiverCount = 0
                , label = config.label
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions =
                    \(State _ s) ->
                        config.subscriptions s
                            |> Sub.map ChangeStateInternally
                }
        )


wrapUpdate : (delta -> state -> ( state, Cmd delta )) -> Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
wrapUpdate innerUpdate debounce_ wrappedDelta (State internalState state) =
    case wrappedDelta of
        Skip ->
            ( State internalState state
            , Cmd.none
            )

        ChangeStateInternally delta ->
            let
                ( newState, cmd ) =
                    innerUpdate delta state
            in
            ( State internalState newState
            , Cmd.map ChangeStateInternally cmd
            )

        ChangeStateOnInput delta ->
            let
                ( newState, cmd ) =
                    innerUpdate delta state
            in
            if debounce_ > 0 then
                ( State internalState newState
                , Cmd.batch
                    [ Task.perform StartDebouncing Time.now
                    , Cmd.map ChangeStateOnInput cmd
                    ]
                )

            else
                ( State { internalState | status = Idle_ } newState
                , Cmd.map ChangeStateOnInput cmd
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


{-| Emit an alert from a `Control` if its output fails to meet the predicate.

This is meant to be used in combination with `respond`, which listens for the
alert and displays an error or notification message on the appropriate
control(s).

-}
alertIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
alertIf when alert (Control control) =
    Control (control >> alertEmitter when (AlertLabel alert))


alertEmitter : (output -> Bool) -> Alert -> ControlFns input state delta output -> ControlFns input state delta output
alertEmitter check alert (ControlFns ctrl) =
    ControlFns
        { ctrl
            | emitAlerts =
                \state ->
                    let
                        oldAlerts =
                            ctrl.emitAlerts state

                        newAlerts =
                            case ctrl.parse state of
                                Ok output ->
                                    if check output then
                                        [ alert ]

                                    else
                                        []

                                Err _ ->
                                    []
                    in
                    List.Extra.unique (oldAlerts ++ newAlerts)
        }


{-| Respond to an alert emitted by another `Control`, display an error or
notification message on this control, and if desired, cause validation to fail.

This is meant to be used in combination with `alertIf`, which emits the alert.

-}
respond : { alert : String, fail : Bool, message : String } -> Control state delta output -> Control state delta output
respond { alert, fail, message } (Control control) =
    Control (control >> alertReceiver (AlertLabel alert) fail message)


alertReceiver : Alert -> Bool -> String -> ControlFns input state delta output -> ControlFns input state delta output
alertReceiver alert fail message (ControlFns ctrl) =
    ControlFns
        { ctrl
            | collectErrors =
                \state alerts ->
                    let
                        oldReceiver =
                            ctrl.collectErrors state alerts

                        newReceiver =
                            if List.member alert alerts then
                                [ { path = ctrl.path
                                  , label = ctrl.label
                                  , message = message
                                  , fail = fail
                                  }
                                ]

                            else
                                []
                    in
                    List.Extra.unique (oldReceiver ++ newReceiver)
            , receiverCount = ctrl.receiverCount + 1
            , collectDebouncingReceivers =
                \((State internalState _) as state) ->
                    case internalState.status of
                        DebouncingSince _ ->
                            alert :: ctrl.collectDebouncingReceivers state

                        _ ->
                            ctrl.collectDebouncingReceivers state
        }


{-| Conditionally display an error on one or more items in a `list` control,
based on the output of the `list` control.

The following example will display errors on the first two items in a list of
strings, if and only if those first two items are "hello" and "world":

    myString =
        string
            |> catch
                { alert = "no-hello-world"
                , fail = True
                , message = "The first two items in the list must not be \"hello\" and \"world\"."
                }

    list myString
        |> alertAtIndexes
            (\list_ ->
                case list of
                    "hello" :: "world" :: _ -> [ 0, 1 ]

                    _ -> []
            )
            "no-hello-world"

-}
alertAtIndexes :
    (output -> List Int)
    -> String
    -> Control (List state) (ListDelta delta) output
    -> Control (List state) (ListDelta delta) output
alertAtIndexes toIndexes alert (Control control) =
    Control (control >> listAlertEmitter toIndexes alert)


listAlertEmitter :
    (output -> List Int)
    -> String
    -> ControlFns input (List state) (ListDelta delta) output
    -> ControlFns input (List state) (ListDelta delta) output
listAlertEmitter check alertLabel (ControlFns ctrl) =
    ControlFns
        { ctrl
            | emitAlerts =
                \state ->
                    let
                        oldAlerts =
                            ctrl.emitAlerts state

                        newAlerts =
                            case ctrl.parse state of
                                Ok output ->
                                    [ AlertList ctrl.path alertLabel (check output) ]

                                Err _ ->
                                    []
                    in
                    List.Extra.unique (oldAlerts ++ newAlerts)
        }


{-| Display an error on a `Control` if its output fails to meet the predicate.

This causes the `Control` to fail validation.

    positiveInt =
        int
            |> failIf
                (\x -> x < 1)
                "Must be greater than zero!"

-}
failIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
failIf check message (Control c) =
    Control
        (\path ->
            let
                (ControlFns control) =
                    c path

                alert =
                    AlertPath path control.receiverCount
            in
            ControlFns control
                |> alertEmitter check alert
                |> alertReceiver alert True message
        )


{-| Display an note on a `Control` if its output fails to meet the predicate.

This just shows the user a message - it doesn't cause the `Control` to fail
validation.

    positiveInt =
        int
            |> failIf
                (\x -> x < 1)
                "Must be greater than zero!"

-}
noteIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
noteIf check message (Control c) =
    Control
        (\path ->
            let
                (ControlFns control) =
                    c path

                alert =
                    AlertPath path control.receiverCount
            in
            ControlFns control
                |> alertEmitter check alert
                |> alertReceiver alert False message
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

    sluggishStringControl =
        string
            |> debounce 2000

-}
debounce : Float -> Control state delta output -> Control state delta output
debounce millis (Control control) =
    let
        debouncer (ControlFns fns) =
            ControlFns { fns | update = fns.baseUpdate millis }
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

    myStringControlHasAnId =
        string
            |> id "my-string"

-}
id : String -> Control state delta output -> Control state delta output
id id_ (Control control) =
    let
        identifier (ControlFns i) =
            ControlFns { i | id = Just id_ }
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

    myStringControlHasAName =
        string
            |> name "My string"

-}
name : String -> Control state delta output -> Control state delta output
name name_ (Control control) =
    let
        namer (ControlFns i) =
            ControlFns { i | name = Just name_ }
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

    myStringControlHasALabel =
        string
            |> label "Enter your name"

-}
label : String -> Control state delta output -> Control state delta output
label label_ (Control control) =
    let
        labeller (ControlFns i) =
            ControlFns { i | label = label_ }
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

    myStringControlHasAClass =
        string
            |> class "important"
            |> class "no-really"

-}
class : String -> Control state delta output -> Control state delta output
class class_ (Control control) =
    let
        classifier (ControlFns i) =
            ControlFns { i | class = class_ :: i.class }
    in
    Control (control >> classifier)


{-| Add a list of HTML class attributes to a `Control`. See docs for Html.Attributes.classList in the `elm/html`
package.

    myStringControlHasClasses =
        string
            |> classList
                [ ( "important", True )
                , ( "no-really", True )
                , ( "ignore-me", False )
                ]

-}
classList : List ( String, Bool ) -> Control state delta output -> Control state delta output
classList classList_ (Control control) =
    let
        classifier (ControlFns i) =
            ControlFns
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
            |> layout (\fieldViews staticConfig dynamicConfig -> Html.div [] fieldViews)

-}
layout :
    (List (Html (Delta delta)) -> ViewConfigStatic -> ViewConfigDynamic state -> List (Html (Delta delta)))
    -> Control state delta output
    -> Control state delta output
layout v (Control control) =
    let
        viewer (ControlFns i) =
            ControlFns { i | view = \staticConfig dynamicConfig -> v (i.childViews staticConfig dynamicConfig) staticConfig dynamicConfig }
    in
    Control (control >> viewer)



{- WRAPVIEW -}


{-| Transform the HTML output of a `Control`'s view function.

    wrappedInt =
        Control.int
            |> Control.wrapView (\\view -> Html.div [] view)

-}
wrapView :
    (List (Html (Delta delta)) -> List (Html (Delta delta)))
    -> Control state delta output
    -> Control state delta output
wrapView wrapper (Control control) =
    let
        viewer (ControlFns i) =
            ControlFns
                { i
                    | view =
                        \staticConfig dynamicConfig ->
                            wrapper (i.view staticConfig dynamicConfig)
                }
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

    oneAndHello =
        tuple ( "Int", int ) ( "String", string )
            |> initWith ( 1, "hello" )

-}
initWith : input -> AdvancedControl input state delta output -> AdvancedControl input state delta output
initWith input (Control control) =
    let
        initialiser (ControlFns i) =
            ControlFns { i | init = i.initWith input }
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


{-| A control that produces an `Int`. Renders as an HTML number input.
-}
int : Control String String Int
int =
    create
        { initEmpty = ( "", Cmd.none )
        , initWith = \s -> ( String.fromInt s, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "number"
        , parse =
            \state ->
                case String.toInt state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "Must be a whole number" ]
        , subscriptions = \state -> Sub.none
        , label = "Int"
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


{-| A control that produces a `Float`. Renders as an HTML number input.
-}
float : Control String String Float
float =
    create
        { initEmpty = ( "", Cmd.none )
        , initWith = \f -> ( String.fromFloat f, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "number"
        , parse =
            \state ->
                case String.toFloat state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "Must be a number" ]
        , subscriptions = \state -> Sub.none
        , label = "Float"
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
        { initEmpty = ( "", Cmd.none )
        , initWith = \s -> ( s, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "text"
        , parse = Ok
        , subscriptions = \state -> Sub.none
        , label = "String"
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
        { initEmpty = ( "", Cmd.none )
        , initWith = \c -> ( String.fromChar c, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "text"
        , parse =
            \str ->
                case String.uncons str of
                    Just ( char_, rest ) ->
                        if String.isEmpty rest then
                            Ok char_

                        else
                            Err [ "Must be exactly one character" ]

                    Nothing ->
                        Err [ "Must not be blank" ]
        , subscriptions = \state -> Sub.none
        , label = "Char"
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
        { initEmpty = ( Tuple.second first, Cmd.none )
        , initWith = \e -> ( e, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = enumView (first :: second :: rest)
        , parse = Ok
        , subscriptions = \state -> Sub.none
        , label = "Enum"
        }



{-
   d8888b.  .d88b.   .d88b.  db
   88  `8D .8P  Y8. .8P  Y8. 88
   88oooY' 88    88 88    88 88
   88~~~b. 88    88 88    88 88
   88   8D `8b  d8' `8b  d8' 88booo.
   Y8888P'  `Y88P'   `Y88P'  Y88888P
-}


{-| A control that produces a `Bool`. Renders as an HTML checkbox.
-}
bool : Control Bool Bool Bool
bool =
    create
        { initEmpty = ( False, Cmd.none )
        , initWith = \b -> ( b, Cmd.none )
        , view =
            \config ->
                [ H.label
                    [ HA.for config.id ]
                    [ H.text config.label ]
                , H.input
                    [ HA.type_ "checkbox"
                    , HA.id config.id
                    , HA.name config.name
                    , HA.class config.class
                    , HA.checked config.state
                    , HE.onClick (not config.state)
                    ]
                    []
                ]
        , update = \delta _ -> ( delta, Cmd.none )
        , parse = Ok
        , subscriptions = \state -> Sub.none
        , label = "Bool"
        }



{-
   .88b  d88.  .d8b.  d8888b.
   88'YbdP`88 d8' `8b 88  `8D
   88  88  88 88ooo88 88oodD'
   88  88  88 88~~~88 88~~~
   88  88  88 88   88 88
   YP  YP  YP YP   YP 88
-}


{-| A combinator that converts a `Control` whose `output` is of type `a` to a `Control` whose `output` is of type `b`.

This is particularly useful for implementing "wrapper" types, such as `Id`s.

Note: with most common map functions, such as `List.map`, we only need to supply a function from `a -> b`. In this case,
however, we need to supply two functions that will allow us to both `convert` the type from `a -> b` and
`revert` it back from `b -> a`.

    type Id
        = Id Int

    idControl =
        map { convert = Id, revert = \(Id i) -> i } int

-}
map :
    { convert : a -> b, revert : b -> a }
    -> Control state delta a
    -> Control ( State state, End ) ( Delta delta, End ) b
map config control =
    Control
        (\path ->
            let
                (Control inner) =
                    record config.convert
                        |> field config.revert control
                        |> end
            in
            inner path
        )



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
        |> label "Maybe"



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
        |> label "Result"



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
        tuple ( "First", string ) ( "Second", int )

-}
tuple :
    Control state1 delta1 output1
    -> Control state2 delta2 output2
    -> Control ( State state1, ( State state2, End ) ) ( Delta delta1, ( Delta delta2, End ) ) ( output1, output2 )
tuple first second =
    record Tuple.pair
        |> field Tuple.first first
        |> field Tuple.second second
        |> end
        |> label "Tuple"
        |> layout
            (\fields staticConfig _ ->
                [ H.fieldset [] (H.legend [] [ H.text staticConfig.label ] :: fields) ]
            )



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
        triple string int float

-}
triple :
    Control state1 delta1 output1
    -> Control state2 delta2 output2
    -> Control state3 delta3 output3
    ->
        Control
            ( State state1, ( State state2, ( State state3, End ) ) )
            ( Delta delta1, ( Delta delta2, ( Delta delta3, End ) ) )
            ( output1, output2, output3 )
triple first second third =
    record (\a b c -> ( a, b, c ))
        |> field (\( a, _, _ ) -> a) first
        |> field (\( _, b, _ ) -> b) second
        |> field (\( _, _, c ) -> c) third
        |> end
        |> label "Triple"
        |> layout
            (\fields staticConfig _ ->
                [ H.fieldset [] (H.legend [] [ H.text staticConfig.label ] :: fields) ]
            )



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
list :
    Control state delta output
    -> Control (List (State state)) (ListDelta delta) (List output)
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
                                (ControlFns fns) =
                                    ctrl path

                                ( initialState, initialCmd ) =
                                    fns.init

                                before =
                                    List.take idx state

                                after =
                                    List.drop idx state
                            in
                            ( before ++ initialState :: after
                            , Cmd.map (ChangeItem idx) initialCmd
                            )

                        ChangeItem idx itemDelta ->
                            let
                                ( newState, cmd ) =
                                    List.Extra.indexedFoldr
                                        (\thisIdx item ( items, prevCmd ) ->
                                            if thisIdx == idx then
                                                let
                                                    (ControlFns itemControl) =
                                                        ctrl (Path.add idx path)

                                                    ( newItem, newCmd ) =
                                                        itemControl.update itemDelta item
                                                in
                                                ( newItem :: items, newCmd )

                                            else
                                                ( item :: items, prevCmd )
                                        )
                                        ( [], Cmd.none )
                                        state
                            in
                            ( newState
                            , Cmd.map (ChangeItem idx) cmd
                            )

                        DeleteItem idx ->
                            ( List.Extra.removeAt idx state, Cmd.none )

                parse =
                    \(State _ state) ->
                        List.foldr
                            (\( idx, item ) res ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                case res of
                                    Ok outputs ->
                                        case itemControl.parse item of
                                            Ok output ->
                                                Ok (output :: outputs)

                                            Err errs ->
                                                Err errs

                                    Err errs ->
                                        case itemControl.parse item of
                                            Ok _ ->
                                                Err errs

                                            Err newErrs ->
                                                Err (newErrs ++ errs)
                            )
                            (Ok [])
                            (List.indexedMap Tuple.pair state)

                collectDebouncingReceivers (State _ listState) =
                    List.indexedMap
                        (\idx itemState ->
                            let
                                (ControlFns itemControl) =
                                    ctrl (Path.add idx path)
                            in
                            itemControl.collectDebouncingReceivers itemState
                        )
                        listState
                        |> List.concat
            in
            ControlFns
                { path = path
                , index = 0
                , initWith =
                    \input ->
                        let
                            ( initialState, initialCmds ) =
                                List.Extra.indexedFoldr
                                    (\idx itemInput ( itemInputs, itemCmds ) ->
                                        let
                                            (ControlFns itemControl) =
                                                ctrl (Path.add idx path)

                                            ( itemState, itemCmd ) =
                                                itemControl.initWith itemInput
                                        in
                                        ( itemState :: itemInputs
                                        , Cmd.map (ChangeItem idx) itemCmd :: itemCmds
                                        )
                                    )
                                    ( [], [] )
                                    input
                        in
                        ( State { status = Intact_, selected = 0 } initialState
                        , Cmd.map ChangeStateInternally (Cmd.batch initialCmds)
                        )
                , init =
                    ( State { status = Intact_, selected = 0 } []
                    , Cmd.none
                    )
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
                                        (ControlFns itemControl) =
                                            ctrl (Path.add idx path)
                                    in
                                    itemControl.setAllIdle item
                                )
                                s
                            )
                , emitAlerts =
                    \(State _ s) ->
                        List.indexedMap
                            (\idx item ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                itemControl.emitAlerts item
                            )
                            s
                            |> List.concat
                , collectErrors =
                    \(State _ listState) alerts ->
                        List.indexedMap
                            (\idx item ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)

                                    filteredAlerts =
                                        List.filterMap
                                            (\alert ->
                                                case alert of
                                                    AlertList alertPath alertLabel alertIndexes ->
                                                        if alertPath == path then
                                                            if List.member idx alertIndexes then
                                                                Just (AlertLabel alertLabel)

                                                            else
                                                                Nothing

                                                        else
                                                            Just alert

                                                    _ ->
                                                        Just alert
                                            )
                                            alerts
                                in
                                itemControl.collectErrors item filteredAlerts
                            )
                            listState
                            |> List.concat
                , receiverCount = 0
                , collectDebouncingReceivers = collectDebouncingReceivers
                , label = "List"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions =
                    \(State _ listState) ->
                        List.indexedMap
                            (\idx itemState ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                itemControl.subscriptions itemState
                                    |> Sub.map (ChangeItem idx)
                            )
                            listState
                            |> Sub.batch
                            |> Sub.map ChangeStateInternally
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
    Control keyState keyDelta comparable
    -> Control valueState valueDelta value
    ->
        Control
            ( State (List (State ( State keyState, ( State valueState, End ) ))), End )
            ( Delta (ListDelta ( Delta keyDelta, ( Delta valueDelta, End ) )), End )
            (Dict.Dict comparable value)
dict keyControl valueControl =
    list
        (tuple
            (keyControl |> respond { alert = "@@dict-unique-keys", fail = True, message = "Keys must be unique" })
            valueControl
            |> layout (\kv _ _ -> kv)
        )
        |> alertAtIndexes
            (List.map Tuple.first >> nonUniqueIndexes)
            "@@dict-unique-keys"
        |> label "Dict"
        |> map { convert = Dict.fromList, revert = Dict.toList }


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
        (memberControl |> respond { alert = "@@set-unique-keys", fail = True, message = "Set members must be unique" })
        |> alertAtIndexes nonUniqueIndexes "@@set-unique-keys"
        |> label "Set"
        |> map { convert = Set.fromList, revert = Set.toList }



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
        |> label "Array"
        |> map { convert = Array.fromList, revert = Array.toList }



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
    b
    ->
        Builder
            { after : End
            , afters : End
            , before : a12 -> a12
            , befores : a11 -> a11
            , debouncingReceiverCollector : a10 -> a10
            , deltaInitialiser : a9 -> a9
            , errorCollector : a8 -> a8
            , alertEmitter : a7 -> a7
            , fns : d -> e -> e
            , idleSetter : a6 -> a6
            , index : Int
            , initialDeltas : f -> g -> g
            , initialStates : h -> i -> i
            , initialiser : a5 -> a5
            , labels : List j
            , makeSetters : a4 -> a4
            , parser : a3 -> a3
            , subscriptionCollector : a2 -> a2
            , toOutput : b
            , updater : a1 -> a1
            , viewer : a -> a
            }
            c
record toOutput =
    Rec
        { index = 0
        , labels = []
        , toOutput = toOutput
        , fns = \_ x -> x
        , initialStates = \_ x -> x
        , initialDeltas = \_ x -> x
        , updater = identity
        , viewer = identity
        , parser = identity
        , idleSetter = identity
        , initialiser = identity
        , deltaInitialiser = identity
        , before = identity
        , befores = identity
        , after = End
        , afters = End
        , makeSetters = identity
        , alertEmitter = identity
        , errorCollector = identity
        , debouncingReceiverCollector = identity
        , subscriptionCollector = identity
        }


{-| Add a field to a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field "Hello" string
            |> end

-}
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
readOnlyField =
    fieldHelper ReadOnly


{-| An internal type used to represent whether a record field is editable, read-only, or hidden
-}
type Access
    = Open
    | ReadOnly
    | Hidden


fieldHelper access fromInput (Control control) builder =
    case builder of
        Cus c ->
            Cus c

        Rec rec ->
            let
                newIndex =
                    rec.index + 1
            in
            Rec
                { index = newIndex
                , labels = rec.labels
                , toOutput = rec.toOutput
                , fns =
                    \path ->
                        let
                            newPath =
                                Path.add newIndex path
                        in
                        rec.fns path
                            << Tuple.pair
                                (RecordFns
                                    { field = control newPath
                                    , fromInput = fromInput
                                    , access = access
                                    }
                                )
                , initialStates =
                    \path ->
                        let
                            newPath =
                                Path.add newIndex path

                            (ControlFns fns) =
                                control newPath
                        in
                        rec.initialStates path
                            << Tuple.pair
                                (fns.init
                                    |> Tuple.first
                                )
                , initialDeltas =
                    \path ->
                        let
                            newPath =
                                Path.add newIndex path

                            (ControlFns fns) =
                                control newPath
                        in
                        rec.initialDeltas path
                            << Tuple.pair
                                (fns.init
                                    |> Tuple.second
                                )
                , updater = rec.updater >> recordStateUpdater
                , viewer = rec.viewer >> recordStateViewer
                , parser = rec.parser >> recordStateValidator
                , idleSetter = rec.idleSetter >> recordStateIdleSetter
                , initialiser = rec.initialiser >> recordStateInitialiser
                , deltaInitialiser = rec.deltaInitialiser >> recordDeltaInitialiser
                , before = rec.before << Tuple.pair Skip
                , befores = rec.befores << Tuple.pair rec.before
                , after = ( Skip, rec.after )
                , afters = ( rec.after, rec.afters )
                , makeSetters = rec.makeSetters >> deltaSetterMaker
                , alertEmitter = rec.alertEmitter >> recordAlertEmitter
                , errorCollector = rec.errorCollector >> recordErrorCollector
                , debouncingReceiverCollector = rec.debouncingReceiverCollector >> recordDebouncingReceiverCollector
                , subscriptionCollector = rec.subscriptionCollector >> recordSubscriptionCollector
                }


endRecord rec =
    Control
        (\path ->
            let
                fns =
                    rec.fns path End

                initialStates =
                    rec.initialStates path End

                initialDeltas =
                    rec.initialDeltas path End

                deltaSetters =
                    makeDeltaSetters rec.makeSetters rec.befores rec.afters

                update delta (State s state) =
                    case delta of
                        Skip ->
                            ( State s state, Cmd.none )

                        ChangeStateOnInput deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater fns deltaSetters deltas state
                            in
                            ( State s newState
                            , Cmd.map ChangeStateOnInput cmd
                            )

                        ChangeStateInternally deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater fns deltaSetters deltas state
                            in
                            ( State s newState
                            , Cmd.map ChangeStateInternally cmd
                            )

                        _ ->
                            ( State s state, Cmd.none )

                childViews _ dynamicConfig =
                    viewRecordStates rec.viewer dynamicConfig.alerts fns deltaSetters dynamicConfig.state

                view staticConfig dynamicConfig =
                    viewRecordStates rec.viewer dynamicConfig.alerts fns deltaSetters dynamicConfig.state

                parse (State _ state) =
                    validateRecordStates rec.parser rec.toOutput fns state

                setAllIdle (State i state) =
                    State { i | status = Idle_ } (setAllRecordStatesToIdle rec.idleSetter fns state)

                emitAlerts (State _ state) =
                    emitAlertsForRecord rec.alertEmitter fns state
            in
            ControlFns
                { path = path
                , index = 0
                , init =
                    ( State { status = Intact_, selected = 0 } initialStates
                    , initialiseRecordDeltas rec.deltaInitialiser deltaSetters initialDeltas
                    )
                , initWith =
                    \output ->
                        ( initialiseRecordStates rec.initialiser output fns
                        , Cmd.none |> fixme "record initWith doesn't send Cmds"
                        )
                , baseUpdate = \_ -> update
                , update = update
                , childViews = \staticConfig dynamicConfig -> childViews staticConfig dynamicConfig
                , view = \staticConfig dynamicConfig -> view staticConfig dynamicConfig
                , parse = parse
                , setAllIdle = setAllIdle
                , emitAlerts = emitAlerts
                , collectErrors = \(State _ states) alerts -> collectErrorsForRecord rec.errorCollector alerts fns states
                , receiverCount = 0
                , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForRecord rec.debouncingReceiverCollector fns states
                , label = "Record"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \(State _ states) -> collectRecordSubscriptions rec.subscriptionCollector deltaSetters fns states
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


collectRecordSubscriptions collector setters fns states =
    collector (\listSubs End End End -> listSubs) [] setters fns states
        |> Sub.batch
        |> Sub.map ChangeStateInternally


recordSubscriptionCollector next listSubs ( setter, restSetters ) ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field
    in
    next ((controlFns.subscriptions state |> Sub.map setter) :: listSubs) restSetters restFns restStates


collectDebouncingReceiversForRecord :
    ((List Alert
      -> End
      -> End
      -> List Alert
     )
     -> List Alert
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> List Alert
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Alert
collectDebouncingReceiversForRecord debouncingReceiverCollector_ fns states =
    debouncingReceiverCollector_ (\receivers End End -> receivers) [] fns states


recordDebouncingReceiverCollector :
    (List Alert
     -> restFns
     -> restStates
     -> List Alert
    )
    -> List Alert
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Alert
recordDebouncingReceiverCollector next receivers ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field
    in
    next (receivers ++ controlFns.collectDebouncingReceivers state) restFns restStates


collectErrorsForRecord :
    ((List Alert
      -> List Feedback
      -> End
      -> End
      -> List Feedback
     )
     -> List Alert
     -> List Feedback
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> List Feedback
    )
    -> List Alert
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Feedback
collectErrorsForRecord errorCollector_ alerts fns states =
    errorCollector_ (\_ errors End End -> errors) alerts [] fns states


recordErrorCollector :
    (List Alert
     -> List Feedback
     -> restFns
     -> restStates
     -> List Feedback
    )
    -> List Alert
    -> List Feedback
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Feedback
recordErrorCollector next alerts errors ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field
    in
    next alerts (errors ++ controlFns.collectErrors state alerts) restFns restStates


emitAlertsForRecord :
    ((List Alert
      -> End
      -> End
      -> List Alert
     )
     -> List Alert
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> List Alert
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Alert
emitAlertsForRecord alertEmitter_ fns states =
    alertEmitter_ (\alerts End End -> alerts) [] fns states


recordAlertEmitter :
    (List Alert
     -> restFns
     -> restStates
     -> List Alert
    )
    -> List Alert
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Alert
recordAlertEmitter next alerts ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        newAlerts =
            controlFns.emitAlerts state

        (ControlFns controlFns) =
            fns.field
    in
    next (alerts ++ newAlerts) restFns restStates


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
recordStateInitialiser next recordInput ( RecordFns fns, restFns ) =
    let
        (ControlFns controlFns) =
            fns.field
    in
    ( recordInput
        |> fns.fromInput
        |> controlFns.initWith
        |> Tuple.first
        |> fixme "Need to initialise record deltas like this too for initWith"
    , next recordInput restFns
    )


initialiseRecordDeltas deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_ (\cmdList End End -> cmdList) [] deltaSetters deltas
        |> Cmd.batch
        |> Cmd.map ChangeStateInternally


recordDeltaInitialiser next cmdList ( setter, restSetters ) ( delta, restDeltas ) =
    next (Cmd.map setter delta :: cmdList) restSetters restDeltas


validateRecordStates :
    ((Result (List Feedback) output2
      -> End
      -> End
      -> Result (List Feedback) output2
     )
     -> Result (List Feedback) output1
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( State state, restStates )
     -> Result (List Feedback) output2
    )
    -> output1
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> Result (List Feedback) output2
validateRecordStates parser toOutput fns states =
    parser (\output End End -> output) (Ok toOutput) fns states


recordStateValidator :
    (Result (List Feedback) output1
     -> restFns
     -> restStates
     -> Result (List Feedback) output2
    )
    -> Result (List Feedback) (output0 -> output1)
    -> ( RecordFns input state delta output0 recordOutput, restFns )
    -> ( State state, restStates )
    -> Result (List Feedback) output2
recordStateValidator next toOutputResult ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field
    in
    next
        (case ( toOutputResult, controlFns.parse state ) of
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
recordStateIdleSetter next ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field
    in
    ( controlFns.setAllIdle state
    , next restFns restStates
    )


viewRecordStates :
    ((List (Html msg)
      -> List Alert
      -> End
      -> End
      -> End
      -> List (Html msg)
     )
     -> List (Html msg)
     -> List Alert
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( Delta delta -> recordDelta, restDeltaSetters )
     -> ( State state, restStates )
     -> List (Html msg)
    )
    -> List Alert
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( State state, restStates )
    -> List (Html msg)
viewRecordStates viewer alerts fns setters states =
    viewer (\views _ End End End -> views) [] alerts fns setters states


recordStateViewer :
    (List (Html (Delta recordDelta))
     -> List Alert
     -> restFns
     -> restDeltaSetters
     -> restStates
     -> List (Html (Delta recordDelta))
    )
    -> List (Html (Delta recordDelta))
    -> List Alert
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( State state, restStates )
    -> List (Html (Delta recordDelta))
recordStateViewer next views alerts ( RecordFns fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field

        view =
            controlFns.view
                { id = controlFns.id
                , name = controlFns.name
                , label = controlFns.label
                , class = controlFns.class
                }
                { state = state
                , status = getStatus controlFns.parse controlFns.collectErrors alerts (State internalState state)
                , alerts = alerts
                , selected = internalState.selected
                }
                |> List.map (H.map (\delta -> ChangeStateOnInput (setter delta)))
    in
    next
        (views
            ++ (case fns.access of
                    Open ->
                        view

                    Hidden ->
                        []

                    ReadOnly ->
                        [ H.div [ HA.disabled True ] view ]
               )
        )
        alerts
        restFns
        restSetters
        restStates


getStatus :
    (State state -> Result (List Feedback) output)
    -> (State state -> List Alert -> List Feedback)
    -> List Alert
    -> State state
    -> Status
getStatus parse collectErrors alerts ((State internalState _) as state) =
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
                    collectErrors state alerts
            in
            Idle (parsedErrors ++ flaggedErrors)


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
recordStateUpdater next { newStates, newCmds } ( RecordFns fns, restFns ) ( deltaSetter, restDeltaSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.field

        ( newState, newCmd ) =
            controlFns.update delta state

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
            { applyInputs : a17 -> a17
            , debouncingReceiverCollector : a16 -> a16
            , deltaAfter : End
            , deltaAfters : End
            , deltaBefore : a15 -> a15
            , deltaBefores : a14 -> a14
            , destructor : b
            , errorCollector : a13 -> a13
            , alertEmitter : a12 -> a12
            , fns : c -> d -> d
            , idleSetter : a11 -> a11
            , index : Int
            , initialDeltas : e -> f -> f
            , initialStates : g -> h -> h
            , initialiseDeltas : a10 -> a10
            , labels : List i
            , makeDeltaSetters : a9 -> a9
            , makeStateSetters : a8 -> a8
            , parser : a7 -> a7
            , stateAfter : End
            , stateAfters : End
            , stateBefore : a6 -> a6
            , stateBefores : a5 -> a5
            , stateInserter : a4 -> a4
            , subscriptionCollector : a3 -> a3
            , toArgStates : a2 -> a2
            , updater : a1 -> a1
            , viewer : a -> a
            }
customType destructor =
    Cus
        { index = 0
        , labels = []
        , fns = \_ x -> x
        , initialStates = \_ x -> x
        , initialDeltas = \_ x -> x
        , updater = identity
        , viewer = identity
        , parser = identity
        , idleSetter = identity
        , deltaBefore = identity
        , deltaBefores = identity
        , deltaAfter = End
        , deltaAfters = End
        , makeDeltaSetters = identity
        , initialiseDeltas = identity
        , stateBefore = identity
        , stateBefores = identity
        , toArgStates = identity
        , stateAfter = End
        , stateAfters = End
        , makeStateSetters = identity
        , stateInserter = identity
        , applyInputs = identity
        , alertEmitter = identity
        , errorCollector = identity
        , debouncingReceiverCollector = identity
        , subscriptionCollector = identity
        , destructor = destructor
        }


tagHelper label_ (Control control) toArgState builder =
    case builder of
        Rec r ->
            Rec r

        Cus rec ->
            let
                newIndex =
                    rec.index + 1
            in
            Cus
                { index = newIndex
                , labels = label_ :: rec.labels
                , fns =
                    \path ->
                        let
                            (ControlFns control_) =
                                control (Path.add newIndex path)
                        in
                        rec.fns path
                            << Tuple.pair
                                (ControlFns { control_ | index = rec.index })
                , initialStates =
                    \path ->
                        let
                            (ControlFns controlFns) =
                                control (Path.add newIndex path)
                        in
                        rec.initialStates path
                            << Tuple.pair
                                (controlFns.init
                                    |> Tuple.first
                                )
                , initialDeltas =
                    \path ->
                        let
                            (ControlFns controlFns) =
                                control (Path.add newIndex path)
                        in
                        rec.initialDeltas path
                            << Tuple.pair
                                (controlFns.init
                                    |> Tuple.second
                                )
                , updater = rec.updater >> customTypeStateUpdater
                , viewer = rec.viewer >> selectedTagViewer
                , parser = rec.parser >> selectedTagParser
                , idleSetter = rec.idleSetter >> selectedTagIdleSetter
                , deltaBefore = rec.deltaBefore << Tuple.pair Skip
                , deltaBefores = rec.deltaBefores << Tuple.pair rec.deltaBefore
                , deltaAfter = ( Skip, rec.deltaAfter )
                , deltaAfters = ( rec.deltaAfter, rec.deltaAfters )
                , makeDeltaSetters = rec.makeDeltaSetters >> deltaSetterMaker
                , initialiseDeltas = rec.initialiseDeltas >> customTypeDeltaInitialiser
                , stateBefore = rec.stateBefore << Tuple.pair Nothing
                , stateBefores = rec.stateBefores << Tuple.pair rec.stateBefore
                , toArgStates = rec.toArgStates << Tuple.pair toArgState
                , stateAfter = ( Nothing, rec.stateAfter )
                , stateAfters = ( rec.stateAfter, rec.stateAfters )
                , makeStateSetters = rec.makeStateSetters >> stateSetterMaker
                , stateInserter = rec.stateInserter >> argStateIntoTagStateInserter
                , applyInputs = rec.applyInputs >> stateSetterToInitialiserApplier
                , alertEmitter = rec.alertEmitter >> customTypeAlertEmitter
                , errorCollector = rec.errorCollector >> customTypeErrorCollector
                , debouncingReceiverCollector = rec.debouncingReceiverCollector >> customTypeDebouncingReceiverCollector
                , subscriptionCollector = rec.subscriptionCollector >> customTypeSubscriptionCollector
                , destructor = rec.destructor
                }


endCustomType rec =
    Control
        (\path ->
            let
                fns =
                    rec.fns path End

                initialStates =
                    rec.initialStates path End

                initialDeltas =
                    rec.initialDeltas path End

                deltaSetters =
                    makeDeltaSetters rec.makeDeltaSetters rec.deltaBefores rec.deltaAfters

                stateSetters =
                    makeStateSetters rec.makeStateSetters rec.stateInserter initialStates fns rec.toArgStates rec.stateBefores rec.stateAfters

                labels =
                    List.reverse rec.labels

                update =
                    \delta (State internalState state) ->
                        case delta of
                            Skip ->
                                ( State internalState state, Cmd.none )

                            TagSelected idx ->
                                ( State { internalState | selected = idx } state, Cmd.none )

                            ChangeStateOnInput tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates rec.updater fns deltaSetters tagDelta state
                                in
                                ( State internalState newTagStates
                                , Cmd.map ChangeStateOnInput cmd
                                )

                            ChangeStateInternally tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates rec.updater fns deltaSetters tagDelta state
                                in
                                ( State internalState newTagStates
                                , Cmd.map ChangeStateInternally cmd
                                )

                            _ ->
                                ( State internalState state, Cmd.none )

                childView _ dynamicConfig =
                    viewSelectedTagState rec.viewer dynamicConfig.alerts dynamicConfig.selected fns deltaSetters dynamicConfig.state

                view staticConfig dynamicConfig =
                    let
                        options =
                            List.indexedMap Tuple.pair labels
                    in
                    customTypeView path staticConfig options dynamicConfig.selected (childView staticConfig dynamicConfig)

                parse =
                    \(State internalState state) -> validateSelectedTagState rec.parser internalState.selected fns state

                setAllIdle =
                    \(State internalState state) ->
                        State
                            { internalState | status = Idle_ }
                            (setSelectedTagStateIdle rec.idleSetter internalState.selected fns state)

                emitAlerts (State internalState state) =
                    emitAlertsForCustomType rec.alertEmitter internalState.selected fns state
            in
            ControlFns
                { path = path
                , index = 0
                , init =
                    ( State { status = Intact_, selected = 0 } initialStates
                    , initialiseCustomTypeDeltas rec.initialiseDeltas deltaSetters initialDeltas
                    )
                , initWith =
                    \tag ->
                        ( applyStateSettersToInitialiser rec.applyInputs rec.destructor stateSetters tag
                        , Cmd.none |> fixme "initWith for custom types doesn't run Cmds of child controls"
                        )
                , baseUpdate = \_ -> update
                , update = update
                , childViews = \staticConfig dynamicConfig -> childView staticConfig dynamicConfig
                , view = \staticConfig dynamicConfig -> view staticConfig dynamicConfig
                , parse = parse
                , setAllIdle = setAllIdle
                , emitAlerts = emitAlerts
                , collectErrors = \(State _ states) alerts -> collectErrorsForCustomType rec.errorCollector alerts fns states
                , receiverCount = 0
                , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForCustomType rec.debouncingReceiverCollector fns states
                , label = "Custom Type"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \(State _ states) -> collectCustomTypeSubscriptions rec.subscriptionCollector deltaSetters fns states
                }
        )



{-
   d888888b  .d8b.   d888b  .d8888.
   `~~88~~' d8' `8b 88' Y8b 88'  YP
      88    88ooo88 88      `8bo.
      88    88~~~88 88  ooo   `Y8b.
      88    88   88 88. ~8~ db   8D
      YP    YP   YP  Y888P  `8888Y'
-}


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
        { initEmpty = ( (), Cmd.none )
        , initWith = \_ -> ( (), Cmd.none )
        , update = \() () -> ( (), Cmd.none )
        , view = \_ -> []
        , parse = \() -> Ok tag
        , subscriptions = \() -> Sub.none
        , label = ""
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
            |> field Tuple.first control
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
tag2 label_ tag control1 control2 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
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
tag3 label_ tag control1 control2 control3 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> field (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, End ) ) )
        )


{-| Add a tag with four arguments to a custom type.
-}
tag4 label_ tag control1 control2 control3 control4 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> field (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> field (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 arg4 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, ( arg4, End ) ) ) )
        )


{-| Add a tag with five arguments to a custom type.
-}
tag5 label_ tag control1 control2 control3 control4 control5 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> field (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> field (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> field (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control5
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 arg4 arg5 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, ( arg4, ( arg5, End ) ) ) ) )
        )



{-
    .o88b. db    db .d8888. d888888b  .d88b.  .88b  d88.      d888888b d8b   db d888888b d88888b d8888b. d8b   db  .d8b.  db      .d8888.
   d8P  Y8 88    88 88'  YP `~~88~~' .8P  Y8. 88'YbdP`88        `88'   888o  88 `~~88~~' 88'     88  `8D 888o  88 d8' `8b 88      88'  YP
   8P      88    88 `8bo.      88    88    88 88  88  88         88    88V8o 88    88    88ooooo 88oobY' 88V8o 88 88ooo88 88      `8bo.
   8b      88    88   `Y8b.    88    88    88 88  88  88         88    88 V8o88    88    88~~~~~ 88`8b   88 V8o88 88~~~88 88        `Y8b.
   Y8b  d8 88b  d88 db   8D    88    `8b  d8' 88  88  88        .88.   88  V888    88    88.     88 `88. 88  V888 88   88 88booo. db   8D
    `Y88P' ~Y8888P' `8888Y'    YP     `Y88P'  YP  YP  YP      Y888888P VP   V8P    YP    Y88888P 88   YD VP   V8P YP   YP Y88888P `8888Y'
-}


collectCustomTypeSubscriptions collector setters fns states =
    collector (\listSubs End End End -> listSubs) [] setters fns states
        |> Sub.batch
        |> Sub.map ChangeStateInternally


customTypeSubscriptionCollector next listSubs ( setter, restSetters ) ( ControlFns fns, restFns ) ( state, restStates ) =
    next ((fns.subscriptions state |> Sub.map setter) :: listSubs) restSetters restFns restStates


collectDebouncingReceiversForCustomType :
    ((List Alert
      -> End
      -> End
      -> List Alert
     )
     -> List Alert
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> List Alert
    )
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
collectDebouncingReceiversForCustomType debouncingReceiverCollector_ fns states =
    debouncingReceiverCollector_ (\receivers End End -> receivers) [] fns states


customTypeDebouncingReceiverCollector :
    (List Alert
     -> restFns
     -> restStates
     -> List Alert
    )
    -> List Alert
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
customTypeDebouncingReceiverCollector next receivers ( ControlFns fns, restFns ) ( state, restStates ) =
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
customTypeStateUpdater next { newStates, newCmds } ( ControlFns fns, restFns ) ( deltaSetter, restDeltaSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        ( newState, newCmd ) =
            fns.update delta state
    in
    next
        { newStates = newStates << Tuple.pair newState
        , newCmds = Cmd.map deltaSetter newCmd :: newCmds
        }
        restFns
        restDeltaSetters
        restDeltas
        restStates


initialiseCustomTypeDeltas deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_ (\cmdList End End -> cmdList) [] deltaSetters deltas
        |> List.map (Cmd.map ChangeStateInternally)
        |> Cmd.batch


customTypeDeltaInitialiser next cmdList ( setter, restSetters ) ( delta, restDeltas ) =
    next (Cmd.map setter delta :: cmdList) restSetters restDeltas


collectErrorsForCustomType :
    ((List Alert
      -> List Feedback
      -> End
      -> End
      -> List Feedback
     )
     -> List Alert
     -> List Feedback
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> List Feedback
    )
    -> List Alert
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Feedback
collectErrorsForCustomType errorCollector_ alerts fns states =
    errorCollector_ (\_ errors End End -> errors) alerts [] fns states


customTypeErrorCollector :
    (List Alert
     -> List Feedback
     -> restFns
     -> restStates
     -> List Feedback
    )
    -> List Alert
    -> List Feedback
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Feedback
customTypeErrorCollector next alerts errors ( ControlFns fns, restFns ) ( state, restStates ) =
    next alerts (errors ++ fns.collectErrors state alerts) restFns restStates


emitAlertsForCustomType :
    ((List Alert
      -> Int
      -> End
      -> End
      -> List Alert
     )
     -> List Alert
     -> Int
     -> ( ControlFns input state delta output, restFns )
     -> ( State state, restStates )
     -> List Alert
    )
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
emitAlertsForCustomType alertEmitter_ selectedTag fns tagStates =
    alertEmitter_ (\alerts _ End End -> alerts) [] selectedTag fns tagStates


customTypeAlertEmitter :
    (List Alert
     -> Int
     -> restFns
     -> restStates
     -> List Alert
    )
    -> List Alert
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
customTypeAlertEmitter next alerts selectedTag ( ControlFns fns, restFns ) ( tagState, restTagStates ) =
    let
        newAlerts =
            if fns.index == selectedTag then
                fns.emitAlerts tagState

            else
                []
    in
    next (alerts ++ newAlerts) selectedTag restFns restTagStates


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
stateSetterMaker next argStateIntoTagStateInserter_ inits ( ControlFns fns, restFns ) ( toArgState, restToArgStates ) ( before, befores ) ( after, afters ) =
    ( toArgState
        (\argState ->
            let
                initialState =
                    fns.initWith argState
                        |> Tuple.first

                maybes =
                    before ( Just initialState, after )
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
selectedTagIdleSetter next selectedTag ( ControlFns fns, restFns ) ( state, restStates ) =
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
     -> Result (List Feedback) value
     -> Int
     -> ( ControlFns x y z w, restFns )
     -> d
     -> e
    )
    -> Int
    -> ( ControlFns x y z w, restFns )
    -> d
    -> e
validateSelectedTagState parser selectedTag fns states =
    parser
        (\result_ _ End End -> result_)
        (Err [ { path = Path.root, label = "FATAL ERROR", message = "tag index " ++ String.fromInt selectedTag ++ " not found", fail = True } ])
        selectedTag
        fns
        states


selectedTagParser :
    (Result (List Feedback) output
     -> Int
     -> restFns
     -> restStates
     -> Result (List Feedback) output
    )
    -> Result (List Feedback) output
    -> Int
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> Result (List Feedback) output
selectedTagParser next result_ selectedTag ( ControlFns fns, restFns ) ( state, restStates ) =
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
    ((Maybe (List (Html delta1))
      -> List Alert
      -> Int
      -> End
      -> End
      -> End
      -> Maybe (List (Html delta1))
     )
     -> Maybe (List (Html delta1))
     -> List Alert
     -> Int
     -> ( ControlFns input state delta0 output, restFns )
     -> ( Delta delta0 -> delta1, restSetters )
     -> ( State state, restStates )
     -> Maybe (List (Html delta1))
    )
    -> List Alert
    -> Int
    -> ( ControlFns input state delta0 output, restFns )
    -> ( Delta delta0 -> delta1, restSetters )
    -> ( State state, restStates )
    -> List (Html (Delta delta1))
viewSelectedTagState viewer alerts selectedTag fns setters states =
    viewer (\maybeView _ _ End End End -> maybeView) Nothing alerts selectedTag fns setters states
        |> Maybe.map (List.map (H.map ChangeStateOnInput))
        |> Maybe.withDefault [ H.text "ERROR!" ]


selectedTagViewer :
    (Maybe (List (Html delta1))
     -> List Alert
     -> Int
     -> restFns
     -> restSetters
     -> restStates
     -> Maybe (List (Html delta1))
    )
    -> Maybe (List (Html delta1))
    -> List Alert
    -> Int
    -> ( ControlFns input state delta0 output, restFns )
    -> ( Delta delta0 -> delta1, restSetters )
    -> ( State state, restStates )
    -> Maybe (List (Html delta1))
selectedTagViewer next maybeView alerts selectedTag ( ControlFns fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
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
                    , alerts = alerts
                    , selected = internalState.selected
                    }
                    |> List.map (H.map setter)
                )

         else
            maybeView
        )
        alerts
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


wrappedView : Status -> List (Html delta) -> Html delta
wrappedView status innerView =
    H.div
        [ HA.class
            (case status of
                Intact ->
                    "control-intact"

                Debouncing ->
                    "control-debouncing"

                Idle feedback ->
                    if List.any (\{ fail } -> fail) feedback then
                        "control-invalid"

                    else
                        "control-valid"
            )
        , HA.class "control-container"
        ]
        (innerView
            ++ [ case status of
                    Idle [] ->
                        H.text ""

                    Idle feedback ->
                        H.div [ HA.class "control-feedback-container" ]
                            (List.map
                                (\f ->
                                    let
                                        ( class_, text_ ) =
                                            if f.fail then
                                                ( "control-feedback-fail", f.message )

                                            else
                                                ( "control-feedback-note", f.message )
                                    in
                                    H.p
                                        [ HA.class class_
                                        , HA.class "control-feedback"
                                        ]
                                        [ H.text text_ ]
                                )
                                feedback
                            )

                    _ ->
                        H.text ""
               ]
        )


customTypeView :
    Path
    -> ViewConfigStatic
    -> List ( Int, String )
    -> Int
    -> List (Html (Delta variants))
    -> List (Html (Delta variants))
customTypeView path staticConfig options selectedTag selectedTagView =
    if List.length options > 1 then
        radioView
            { options = options
            , selectedOption = selectedTag
            , toMsg = TagSelected
            , id = Maybe.withDefault (Path.toString path) staticConfig.id
            , name = Maybe.withDefault (Path.toString path) staticConfig.name
            , label = staticConfig.label
            }
            ++ selectedTagView

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
    -> List Alert
    -> (Path -> ControlFns input state delta output)
    -> List (Html (Delta (ListDelta delta)))
listView path staticConfig dynamicConfig debouncingReceivers ctrl =
    let
        id_ =
            Maybe.withDefault (Path.toString path) staticConfig.id

        view_ =
            H.div
                [ HA.id id_
                , HA.class "control-container"
                ]
                [ H.label
                    [ HA.for id_ ]
                    [ H.text staticConfig.label ]
                , if List.isEmpty dynamicConfig.state then
                    button (ChangeStateOnInput (InsertItem 0)) "Add item"

                  else
                    H.ol []
                        (List.indexedMap
                            (\idx (State internalState state) ->
                                let
                                    (ControlFns control) =
                                        ctrl (Path.add idx path)

                                    filteredAlerts1 =
                                        List.filterMap
                                            (\alert ->
                                                case alert of
                                                    AlertList alertPath alertLabel alertIndexes ->
                                                        if alertPath == path then
                                                            if List.member idx alertIndexes then
                                                                Just (AlertLabel alertLabel)

                                                            else
                                                                Nothing

                                                        else
                                                            Just alert

                                                    _ ->
                                                        Just alert
                                            )
                                            dynamicConfig.alerts

                                    filteredAlerts2 =
                                        List.filter (\f -> not <| List.member f debouncingReceivers) filteredAlerts1
                                in
                                H.li []
                                    [ H.div []
                                        (control.view
                                            { id = control.id
                                            , name = control.name
                                            , label = control.label
                                            , class = control.class
                                            }
                                            { state = state
                                            , status = getStatus control.parse control.collectErrors filteredAlerts2 (State internalState state)
                                            , alerts = filteredAlerts2
                                            , selected = internalState.selected
                                            }
                                        )
                                        |> H.map (ChangeItem idx)
                                    , button (DeleteItem idx) "Delete item"
                                    , H.div [] [ button (InsertItem (idx + 1)) "Insert item" ]
                                    ]
                            )
                            dynamicConfig.state
                        )
                        |> H.map ChangeStateOnInput
                ]
    in
    [ view_ ]


textControlView : String -> { state : String, id : String, label : String, name : String, class : String } -> List (Html String)
textControlView type_ config =
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


enumView : List ( String, enum ) -> { state : enum, id : String, label : String, name : String, class : String } -> List (Html enum)
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
    -> List (Html msg)
radioView config =
    [ H.fieldset
        [ HA.id config.id ]
        (H.legend [] [ H.text config.label ]
            :: List.indexedMap
                (\idx ( option, optionLabel ) ->
                    let
                        optionId =
                            config.id ++ "-" ++ String.fromInt (idx + 1)
                    in
                    H.div []
                        [ H.input
                            [ HA.type_ "radio"
                            , HA.name config.name
                            , HA.id optionId
                            , HA.value optionLabel
                            , HA.checked (config.selectedOption == option)
                            , onChecked (config.toMsg option)
                            ]
                            []
                        , H.label [ HA.for optionId ] [ H.text optionLabel ]
                        ]
                )
                config.options
        )
    ]


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
