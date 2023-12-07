module Control exposing
    ( Control, Form, sandbox, simpleForm, form
    , bool, int, float, string, char, enum
    , tuple, triple, maybe, result, list, dict, set, array, map
    , ControlConfig, create
    , failIf, noteIf
    , alertIf, respond
    , alertAtIndexes
    , default, debounce, id, name, label, class, classList, wrapView
    , RecordBuilder, record, field, endRecord, LayoutConfig, Subcontrol, layout
    , CustomTypeBuilder, customType, tag0, tag1, tag2, tag3, tag4, tag5, endCustomType
    , State, Delta, ListDelta, End
    , AdvancedControl, ControlFns, Alert, RecordFns, Status, InternalViewConfig, Path, Feedback
    )

{-|


# Creating a form

@docs Control, Form, sandbox, simpleForm, form


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
show a helpful error message on the "Confirm password" field if they don't. You
can achieve this as follows:

    passwordControl =
        record
            (\password confirmation ->
                { password = password
                , confirmation = confirmation
                }
            )
            |> field string
            |> field
                (string
                    |> respond
                        { alert = "passwords-do-not-match"
                        , fail = True
                        , message = "Must match 'Enter password' field"
                        }
                )
            |> endRecord
            |> alertIf
                (\{ password, confirmation } ->
                    password /= confirmation
                )
                "passwords-do-not-match"

@docs alertIf, respond


## List validation

@docs alertAtIndexes


# Configuring controls

@docs default, debounce, id, name, label, class, classList, wrapView


# Building record combinators

@docs RecordBuilder, record, field, endRecord, LayoutConfig, Subcontrol, layout


# Building custom type combinators

@docs CustomTypeBuilder, customType, tag0, tag1, tag2, tag3, tag4, tag5, endCustomType


# Form internals

These are types that you will see in your form's `State` and `Delta` type signatures.

@docs State, Delta, ListDelta, End


# Internal stuff that you can ignore

A user of this package shouldn't need to know about any of these types -
they are only exposed to make it possible to write type signatures.

@docs AdvancedControl, ControlFns, Alert, RecordFns, Status, InternalViewConfig, Path, Feedback

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
Elm `Program` to initialise, view, update, subscribe, and submit your form.
-}
type alias Form state delta output msg =
    { blank : ( State state, Cmd msg )
    , prefill : output -> ( State state, Cmd msg )
    , update : Delta delta -> State state -> ( State state, Cmd msg )
    , view : State state -> Html msg
    , subscriptions : State state -> Sub msg
    , submit : State state -> ( State state, Result (List Feedback) output )
    }


{-| Configuration for a custom control. You need to supply the following fields:

  - `blank`: an initial "empty" value for the `state` of the control, plus an initial (optional) `Cmd` to run. For a control whose `state` is of type `String`, the state value might be `""`, for a number it might be `0`.
  - `prefill`: a function that uses a value of the control's `output` type to initialise the control's `state` and (optionally) run an initial `Cmd`. For a control that outputs an `Int`, but whose `state` is a `String`, you might use `String.fromInt` to convert the `output` value into a `state` value.
  - `update`: a function that updates the `state` of the control based on its existing `state` and a `delta` type that it receives. This is exactly analogous to an Elm program's update function, where `state` = `Model` and `delta` = `Msg`.
  - `view`: a function that outputs `Html delta` based on the `state` of the control. This is similar to an Elm program's view function, except instead of just taking the `state` as an argument, it takes a record containing the `state` plus the id, name, class, and label for the input.
  - `subscriptions`: a function that outputs `Sub delta` based on the `state` of the control. This is exactly analogous to an Elm program's subscriptions function.
  - `parse`: a function that attempts to parse the control's state into its output type, producing a result. If parsing fails, it can provide a list of errors.

-}
type alias ControlConfig state delta output =
    { blank : ( state, Cmd delta )
    , prefill : output -> ( state, Cmd delta )
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
        { fns : ControlFns input state delta output
        , fromInput : recordOutput -> output
        }


{-| Some internal stuff needed to build up record and custom type controls
-}
type ControlFns input state delta output
    = ControlFns
        { index : Int
        , initBlank : ( State state, Cmd (Delta delta) )
        , initPrefilled : input -> ( State state, Cmd (Delta delta) )
        , baseUpdate : Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
        , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
        , view : InternalViewConfig state -> List (Html (Delta delta))
        , subControlViews : InternalViewConfig state -> List (Subcontrol delta)
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


{-| Data used by the `layout` function to render a subcontrol of a combinator.
-}
type alias Subcontrol delta =
    { html : List (Html (Delta delta))
    , label : String
    , index : Int
    }


{-| Some internal stuff needed to handle validations
-}
type Alert
    = AlertLabel String
    | AlertPath Path Int
    | AlertList Path String (List Int)


{-| Some internal stuff to handle validation
-}
type alias Feedback =
    { path : Path
    , label : String
    , message : String
    , fail : Bool
    }


{-| Some internal stuff needed to view controls
-}
type alias InternalViewConfig state =
    { state : state
    , status : Status
    , alerts : List Alert
    , selected : Int
    , name : String
    , id : String
    , label : String
    , class : List String
    }


{-| Configuration for the `layout` function
-}
type alias LayoutConfig delta =
    { selected : Int
    , selectMsg : Int -> Delta delta
    , id : String
    , label : String
    , class : String
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


{-| Convert a `Control` into a simple `Form`, rendered as a list of controls
wrapped in an HTML `<form>` element, with a submit button at the bottom.

You will need to supply a couple of variants from your app's `Msg` type: one to
handle updates of the form's state, and one to handle the submission of the
form, as follows:

    type alias Msg
        = FormUpdated (Delta String)
        | FormSubmitted

    mySimpleForm : Form (State String) (Delta String) Int Msg
    mySimpleForm =
        form
            { control = int
            , onUpdate = FormUpdated
            , onSubmit = FormSubmitted
            }

Now you can integrate your `Form` into a larger Elm app:

1.  Call `mySimpleForm.blank` or `mySimpleForm.prefill` in your `init` function.
2.  Call `mySimpleForm.update` in the `FormUpdated` branch of your `update` function.
3.  Call `mySimpleForm.submit` in the `FormSubmitted` branch of your `update` function.
4.  Call `mySimpleForm.view` in your `view` function.
5.  Call `mySimpleForm.subscriptions` in your `subscriptions` function.

-}
simpleForm : { control : Control state delta output, onUpdate : Delta delta -> msg, onSubmit : msg } -> Form state delta output msg
simpleForm { onUpdate, onSubmit, control } =
    form
        { control = control
        , onUpdate = onUpdate
        , view =
            \controlView ->
                H.form [ HE.onSubmit onSubmit ]
                    (controlView ++ [ H.button [ HA.type_ "submit" ] [ H.text "Submit" ] ])
        }


{-| Convert a `Control` into a `Form`, and render the form however you like.

You will need to supply a variant from your app's `Msg` type to handle updates of the form's state.
You will also need to supply a `view` function that takes the `List (Html msg)` produced by the
supplied `Control` and returns an `Html msg`, as follows:

    type alias Msg
        = FormUpdated (Delta String)
        | FormSubmitted

    myForm : Form (State String) (Delta String) Int Msg
    myForm =
        form
            { control = int
            , onUpdate = FormUpdated
            , view =
                \controlView ->
                    H.form
                        [ HE.onSubmit FormSubmitted ]
                        (controlView ++ [ H.button [ HA.type_ "submit" ] [ H.text "Submit" ] ])
            }

Now you can integrate your `Form` into a larger Elm app:

1.  Call `myForm.blank` or `myForm.prefill` in your `init` function.
2.  Call `myForm.update` in the `FormUpdated` branch of your `update` function.
3.  Call `myForm.submit` in the `FormSubmitted` branch of your `update` function.
4.  Call `myForm.view` in your `view` function.
5.  Call `myForm.subscriptions` in your `subscriptions` function.

-}
form : { control : Control state delta output, onUpdate : Delta delta -> msg, view : List (Html msg) -> Html msg } -> Form state delta output msg
form { control, onUpdate, view } =
    let
        path =
            Path.root

        (Control c) =
            control

        (ControlFns fns) =
            c path
    in
    { blank = fns.initBlank |> Tuple.mapSecond (Cmd.map onUpdate)
    , prefill =
        \output ->
            let
                (Control initialisedControl) =
                    default output control

                (ControlFns fns2) =
                    initialisedControl Path.root
            in
            fns2.initBlank
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
                    List.filter (\emittedAlert -> not <| List.member emittedAlert debouncingReceivers) emittedAlerts

                status =
                    getStatus fns.parse fns.collectErrors alerts s
            in
            view
                (fns.view
                    { id = Maybe.withDefault ("control-" ++ Path.toString path) fns.id
                    , name = Maybe.withDefault ("control-" ++ Path.toString path) fns.name
                    , label = fns.label
                    , class = fns.class
                    , state = state
                    , status = status
                    , alerts = alerts
                    , selected = internalState.selected
                    }
                    |> List.map (H.map onUpdate)
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
        path =
            Path.root

        (Control c) =
            control

        (ControlFns fns) =
            c path
    in
    Browser.element
        { init =
            \() ->
                fns.initBlank
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
                            , H.ul [] (List.map (\feedback -> H.li [] [ H.text (Path.toString feedback.path ++ ": " ++ feedback.message) ]) errs)
                            ]
                in
                H.div []
                    [ H.h1 [] [ H.text "Form" ]
                    , H.form []
                        (fns.view
                            { id = Maybe.withDefault (Path.toString path) fns.id
                            , name = Maybe.withDefault (Path.toString path) fns.name
                            , label = fns.label
                            , class = fns.class
                            , state = state
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
            , initPrefilled = identity
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
create controlConfig =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate controlConfig.update

                parse =
                    \(State _ state) ->
                        controlConfig.parse state
                            |> Result.mapError
                                (List.map
                                    (\message ->
                                        { message = message
                                        , label = controlConfig.label
                                        , path = path
                                        , fail = True
                                        }
                                    )
                                )
            in
            ControlFns
                { path = path
                , index = 0
                , initBlank =
                    controlConfig.blank
                        |> Tuple.mapFirst (State { status = Intact_, selected = 1 })
                        |> Tuple.mapSecond (Cmd.map ChangeStateInternally)
                , initPrefilled =
                    \input ->
                        controlConfig.prefill input
                            |> Tuple.mapFirst (State { status = Intact_, selected = 1 })
                            |> Tuple.mapSecond (Cmd.map ChangeStateInternally)
                , baseUpdate = preUpdate
                , update = preUpdate 0
                , subControlViews = \_ -> []
                , view =
                    \viewConfig ->
                        [ controlConfig.view
                            { state = viewConfig.state
                            , label = viewConfig.label
                            , id = viewConfig.id
                            , name = viewConfig.name
                            , class = String.join " " viewConfig.class
                            }
                            |> wrappedView viewConfig.status
                            |> H.map ChangeStateOnInput
                        ]
                , parse = parse
                , setAllIdle = \(State i s) -> State { i | status = Idle_ } s
                , emitAlerts = \_ -> []
                , collectDebouncingReceivers = \_ -> []
                , collectErrors = \_ _ -> []
                , receiverCount = 0
                , label = controlConfig.label
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions =
                    \(State _ s) ->
                        controlConfig.subscriptions s
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

    myList =
        list myString
            |> alertAtIndexes
                (\list_ ->
                    case list of
                        "hello" :: "world" :: _ ->
                            [ 0, 1 ]

                        _ ->
                            []
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
                "This must be greater than zero!"

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
            |> noteIf
                (\x -> x < 1)
                "Should this be greater than zero?"

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
label : String -> AdvancedControl input state delta output -> AdvancedControl input state delta output
label label_ (Control control) =
    let
        labeller (ControlFns i) =
            ControlFns
                { i
                    | label = label_
                    , parse =
                        \state ->
                            i.parse state
                                |> Result.mapError
                                    (\fs ->
                                        List.map
                                            (\f ->
                                                { f
                                                    | label =
                                                        if f.path == i.path then
                                                            label_

                                                        else
                                                            f.label
                                                }
                                            )
                                            fs
                                    )
                }
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


{-| Define the HTML output for a combinator.

This is useful if you want to alter the way that the combinator's subcontrols
are laid out on the screen.

For example, you could use it to present a record control as a wizard, rather
than a list. The wizard example below is very naive, but hopefully gives you the
idea.

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
            |> field string
            |> field int
            |> endRecord
            |> layout wizard

    wizard config subcontrols =
        let
            currentPage =
                config.selected

            totalPages =
                List.length subcontrols

            currentPageView =
                subcontrols
                    |> List.filter (\{ index } -> index == currentPage)
                    |> List.map .html
                    |> List.concat

            nextClicked =
                config.selectMsg (currentPage + 1)

            backClicked =
                config.selectMsg (currentPage - 1)

            navigationButton txt msg =
                Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick msg
                    ]
                    [ Html.text txt ]
        in
        Html.div
            [ Html.Attributes.id config.id
            , Html.Attributes.class config.class
            ]
            ([ Html.h1 [] [ Html.text "Wizard!" ]
             , navigationButton "Back" backClicked
             , Html.text
                (String.join " "
                    [ "page"
                    , String.fromInt currentPage
                    , "of"
                    , String.fromInt totalPages
                    ]
                )
             , navigationButton "Next" nextClicked
             ]
                ++ currentPageView
            )

-}
layout :
    (LayoutConfig delta -> List (Subcontrol delta) -> List (Html (Delta delta)))
    -> Control state delta output
    -> Control state delta output
layout view (Control control) =
    let
        viewer (ControlFns fns) =
            ControlFns
                { fns
                    | view =
                        \internalViewConfig ->
                            let
                                layoutConfig =
                                    { class = String.join " " internalViewConfig.class
                                    , id = internalViewConfig.id
                                    , label = internalViewConfig.label
                                    , selected = internalViewConfig.selected
                                    , selectMsg = TagSelected
                                    }

                                subcontrols =
                                    fns.subControlViews internalViewConfig
                            in
                            view layoutConfig subcontrols
                }
    in
    Control (control >> viewer)



{-
   db   d8b   db d8888b.  .d8b.  d8888b. db    db d888888b d88888b db   d8b   db
   88   I8I   88 88  `8D d8' `8b 88  `8D 88    88   `88'   88'     88   I8I   88
   88   I8I   88 88oobY' 88ooo88 88oodD' Y8    8P    88    88ooooo 88   I8I   88
   Y8   I8I   88 88`8b   88~~~88 88~~~   `8b  d8'    88    88~~~~~ Y8   I8I   88
   `8b d8'8b d8' 88 `88. 88   88 88       `8bd8'    .88.   88.     `8b d8'8b d8'
    `8b8' `8d8'  88   YD YP   YP 88         YP    Y888888P Y88888P  `8b8' `8d8'
-}


{-| Transform the HTML output of a `Control`'s view function.

    wrappedInt =
        int
            |> wrapView (\view -> [ Html.div [] view ])

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
                        \config ->
                            wrapper (i.view config)
                }
    in
    Control (control >> viewer)



{-
   d8888b. d88888b d88888b  .d8b.  db    db db      d888888b
   88  `8D 88'     88'     d8' `8b 88    88 88      `~~88~~'
   88   88 88ooooo 88ooo   88ooo88 88    88 88         88
   88   88 88~~~~~ 88~~~   88~~~88 88    88 88         88
   88  .8D 88.     88      88   88 88b  d88 88booo.    88
   Y8888D' Y88888P YP      YP   YP ~Y8888P' Y88888P    YP
-}


{-| Set a default `output` value for a `Control`.

    oneAndHello =
        tuple int string
            |> default ( 1, "hello" )

-}
default : input -> AdvancedControl input state delta output -> AdvancedControl input state delta output
default input (Control control) =
    let
        initialiser (ControlFns i) =
            ControlFns { i | initBlank = i.initPrefilled input }
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
        { blank = ( "", Cmd.none )
        , prefill = \s -> ( String.fromInt s, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "numeric"
        , parse =
            \state ->
                case String.toInt state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "Must be a whole number" ]
        , subscriptions = \_ -> Sub.none
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
        { blank = ( "", Cmd.none )
        , prefill = \f -> ( String.fromFloat f, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "decimal"
        , parse =
            \state ->
                case String.toFloat state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "Must be a number" ]
        , subscriptions = \_ -> Sub.none
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
        { blank = ( "", Cmd.none )
        , prefill = \s -> ( s, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = textControlView "text"
        , parse = Ok
        , subscriptions = \_ -> Sub.none
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
        { blank = ( "", Cmd.none )
        , prefill = \c -> ( String.fromChar c, Cmd.none )
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
        , subscriptions = \_ -> Sub.none
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
        { blank = ( Tuple.second first, Cmd.none )
        , prefill = \e -> ( e, Cmd.none )
        , update = \delta _ -> ( delta, Cmd.none )
        , view = enumView (first :: second :: rest)
        , parse = Ok
        , subscriptions = \_ -> Sub.none
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
        { blank = ( False, Cmd.none )
        , prefill = \b -> ( b, Cmd.none )
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
        , subscriptions = \_ -> Sub.none
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
        map
            { convert = \i -> Id i
            , revert = \(Id i) -> i
            }
            int

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
                        |> endRecord
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
        |> endCustomType
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
        |> endCustomType
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
        tuple string int

-}
tuple :
    Control state1 delta1 output1
    -> Control state2 delta2 output2
    -> Control ( State state1, ( State state2, End ) ) ( Delta delta1, ( Delta delta2, End ) ) ( output1, output2 )
tuple first second =
    record Tuple.pair
        |> field Tuple.first first
        |> field Tuple.second second
        |> endRecord
        |> label "Tuple"
        |> layout
            (\config subcontrols ->
                [ H.fieldset [ HA.id config.id ] (H.legend [] [ H.text config.label ] :: List.concatMap .html subcontrols) ]
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
        |> endRecord
        |> label "Triple"
        |> layout
            (\config subcontrols ->
                [ H.fieldset [ HA.id config.id ] (H.legend [] [ H.text config.label ] :: List.concatMap .html subcontrols) ]
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
                                    fns.initBlank

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
                , initPrefilled =
                    \input ->
                        let
                            ( initialState, initialCmds ) =
                                List.Extra.indexedFoldr
                                    (\idx itemInput ( itemInputs, itemCmds ) ->
                                        let
                                            (ControlFns itemControl) =
                                                ctrl (Path.add idx path)

                                            ( itemState, itemCmd ) =
                                                itemControl.initPrefilled itemInput
                                        in
                                        ( itemState :: itemInputs
                                        , Cmd.map (ChangeItem idx) itemCmd :: itemCmds
                                        )
                                    )
                                    ( [], [] )
                                    input
                        in
                        ( State { status = Intact_, selected = 1 } initialState
                        , Cmd.map ChangeStateInternally (Cmd.batch initialCmds)
                        )
                , initBlank =
                    ( State { status = Intact_, selected = 1 } []
                    , Cmd.none
                    )
                , baseUpdate = update
                , update = update 0
                , subControlViews = \_ -> []
                , view =
                    \config ->
                        let
                            debouncingReceivers =
                                -- this is a total hack!
                                collectDebouncingReceivers (State { status = Intact_, selected = config.selected } config.state)
                        in
                        listView path config debouncingReceivers ctrl
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
        dict int string

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
            |> layout (\_ subcontrols -> List.concatMap .html subcontrols)
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


{-| A data structure used to build records
-}
type RecordBuilder after afters before befores debouncingReceiverCollector deltaInitialiser errorCollector alertEmitter fns idleSetter initialDeltas initialStates initialiser makeSetters parser subscriptionCollector toOutput updater viewer
    = RecordBuilder
        { after : after
        , afters : afters
        , before : before
        , befores : befores
        , debouncingReceiverCollector : debouncingReceiverCollector
        , deltaInitialiser : deltaInitialiser
        , errorCollector : errorCollector
        , alertEmitter : alertEmitter
        , fns : fns
        , idleSetter : idleSetter
        , index : Int
        , initialDeltas : initialDeltas
        , initialStates : initialStates
        , initialiser : initialiser
        , makeSetters : makeSetters
        , parser : parser
        , subscriptionCollector : subscriptionCollector
        , toOutput : toOutput
        , updater : updater
        , viewer : viewer
        }


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
            |> field string
            |> field int
            |> endRecord

-}
record :
    toOutput
    ->
        RecordBuilder
            End
            End
            (a12 -> a12)
            (a11 -> a11)
            (a10 -> a10)
            (a9 -> a9)
            (a8 -> a8)
            (a7 -> a7)
            (b -> c -> c)
            (a6 -> a6)
            (d -> e -> e)
            (f -> g -> g)
            (a5 -> a5)
            (a4 -> a4)
            (a3 -> a3)
            (a2 -> a2)
            toOutput
            (a1 -> a1)
            (a -> a)
record toOutput =
    RecordBuilder
        { index = 0
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
            |> field string
            |> endRecord

-}
field :
    (recordOutput8 -> output7)
    -> AdvancedControl input8 state9 delta12 output7
    ->
        RecordBuilder
            after1
            afters1
            (( Delta delta13, a18 ) -> c5)
            (( ( Delta delta13, a18 ) -> c5, a17 ) -> c4)
            (a16 -> List Alert -> restFns7 -> restStates7 -> List Alert)
            (a15
             -> List (Cmd delta1_1)
             -> restDeltaSetters2
             -> restDeltas2
             -> List (Cmd delta1_1)
            )
            (a14
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates6
             -> List Feedback
            )
            (a13 -> List Alert -> restFns5 -> restStates5 -> List Alert)
            (Path.Path
             -> ( RecordFns input8 state9 delta12 output7 recordOutput8, a12 )
             -> c3
            )
            (a11 -> restFns4 -> restStates4 -> restStates4)
            (Path.Path -> ( Cmd (Delta delta12), a10 ) -> c2)
            (Path.Path -> ( State state9, a9 ) -> c1)
            (a8 -> (a7 -> c) -> List (Cmd msg1) -> a5 -> b -> d -> e)
            (a4 -> befores -> afters -> next)
            (a3
             -> Result (List Feedback) output1_1
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output2_1
            )
            (a2
             -> List (Sub msg)
             -> restDeltas1
             -> restFns2
             -> restStates2
             -> List (Sub msg)
            )
            toOutput
            (a1
             ->
                { newCmds : List (Cmd recordDelta1)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters1
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta1), newStates : recordState }
            )
            (a
             -> List (Subcontrol recordDelta)
             -> List Alert
             -> restFns
             -> restDeltaSetters
             -> restStates
             -> List (Subcontrol recordDelta)
            )
    ->
        RecordBuilder
            ( Delta delta11, after1 )
            ( after1, afters1 )
            (a18 -> c5)
            (a17 -> c4)
            (a16
             -> List Alert
             -> ( RecordFns input7 state8 delta10 output6 recordOutput7, restFns7 )
             -> ( State state8, restStates7 )
             -> List Alert
            )
            (a15
             -> List (Cmd delta1_1)
             -> ( delta9 -> delta1_1, restDeltaSetters2 )
             -> ( Cmd delta9, restDeltas2 )
             -> List (Cmd delta1_1)
            )
            (a14
             -> List Alert
             -> List Feedback
             -> ( RecordFns input6 state7 delta8 output5 recordOutput6, restFns6 )
             -> ( State state7, restStates6 )
             -> List Feedback
            )
            (a13
             -> List Alert
             -> ( RecordFns input5 state6 delta7 output4 recordOutput5, restFns5 )
             -> ( State state6, restStates5 )
             -> List Alert
            )
            (Path.Path -> a12 -> c3)
            (a11
             -> ( RecordFns input4 state5 delta6 output3 recordOutput4, restFns4 )
             -> ( State state5, restStates4 )
             -> ( State state5, restStates4 )
            )
            (Path.Path -> a10 -> c2)
            (Path.Path -> a9 -> c1)
            (a8
             -> (( State state4, a7 ) -> c)
             -> List (Cmd msg1)
             -> a5
             -> ( RecordFns a6 state4 delta5 a6 a5, b )
             -> ( Delta delta5 -> msg1, d )
             -> e
            )
            (a4
             -> ( ( value, after ) -> delta4, befores )
             -> ( after, afters )
             -> ( value -> delta4, next )
            )
            (a3
             -> Result (List Feedback) (output0 -> output1_1)
             -> ( RecordFns input3 state3 delta3 output0 recordOutput3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output2_1
            )
            (a2
             -> List (Sub msg)
             -> ( Delta delta2 -> msg, restDeltas1 )
             -> ( RecordFns input2 state2 delta2 output2 recordOutput2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub msg)
            )
            toOutput
            (a1
             ->
                { newCmds : List (Cmd recordDelta1)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( RecordFns input1 state1 delta1 output1 recordOutput1, restFns1 )
             -> ( Delta delta1 -> recordDelta1, restDeltaSetters1 )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta1), newStates : recordState }
            )
            (a
             -> List (Subcontrol recordDelta)
             -> List Alert
             -> ( RecordFns input state delta output recordOutput, restFns )
             -> ( Delta delta -> recordDelta, restDeltaSetters )
             -> ( State state, restStates )
             -> List (Subcontrol recordDelta)
            )
field fromInput (Control control) (RecordBuilder rec) =
    let
        newIndex =
            rec.index + 1
    in
    RecordBuilder
        { index = newIndex
        , toOutput = rec.toOutput
        , fns =
            \path ->
                let
                    newPath =
                        Path.add newIndex path

                    (ControlFns fns) =
                        control newPath
                in
                rec.fns path
                    << Tuple.pair
                        (RecordFns
                            { fns = ControlFns { fns | index = newIndex }
                            , fromInput = fromInput
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
                        (fns.initBlank
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
                        (fns.initBlank
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


{-| Finalise the construction of a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field string
            |> endRecord

-}
endRecord :
    RecordBuilder
        after
        afters
        before
        (End -> befores)
        ((List Alert -> End -> End -> List Alert)
         -> List Alert
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( State state, restStates )
         -> List Alert
        )
        ((List (Cmd ( Delta delta, restDeltas ))
          -> End
          -> End
          -> List (Cmd ( Delta delta, restDeltas ))
         )
         -> List (Cmd ( Delta delta, restDeltas ))
         -> ( Delta delta -> recordDelta, restSetters )
         -> deltas
         -> List (Cmd ( Delta delta, restDeltas ))
        )
        ((List Alert -> List Feedback -> End -> End -> List Feedback)
         -> List Alert
         -> List Feedback
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( State state, restStates )
         -> List Feedback
        )
        ((List Alert -> End -> End -> List Alert)
         -> List Alert
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( State state, restStates )
         -> List Alert
        )
        (Path -> End -> ( RecordFns input1 state delta output1 recordInput, restFns ))
        ((End -> End -> End)
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( State state, restStates )
         -> ( State state, restStates )
        )
        (Path -> End -> deltas)
        (Path -> End -> ( State state, restStates ))
        (((End -> state1)
          -> List (Cmd msg)
          -> b
          -> End
          -> End
          -> ( State state1, Cmd (Delta msg) )
         )
         -> (a -> a)
         -> List c
         -> input
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( Delta delta -> recordDelta, restSetters )
         ->
            ( State ( State state, restStates )
            , Cmd (Delta ( Delta delta, restDeltas ))
            )
        )
        ((End -> End -> End)
         -> befores
         -> afters
         -> ( Delta delta -> recordDelta, restSetters )
        )
        ((Result (List Feedback) output -> End -> End -> Result (List Feedback) output)
         -> Result (List Feedback) output1_1
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( State state, restStates )
         -> Result (List Feedback) output
        )
        ((List (Sub ( Delta delta, restDeltas ))
          -> End
          -> End
          -> End
          -> List (Sub ( Delta delta, restDeltas ))
         )
         -> List (Sub ( Delta delta, restDeltas ))
         -> ( Delta delta -> recordDelta, restSetters )
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( State state, restStates )
         -> List (Sub ( Delta delta, restDeltas ))
        )
        output1_1
        (({ newCmds : List (Cmd ( Delta delta, restDeltas ))
          , newStates : End -> ( State state, restStates )
          }
          -> End
          -> End
          -> End
          -> End
          ->
            { newCmds : List (Cmd ( Delta delta, restDeltas ))
            , newStates : End -> ( State state, restStates )
            }
         )
         ->
            { newCmds : List (Cmd ( Delta delta, restDeltas ))
            , newStates : ( State state, restStates ) -> ( State state, restStates )
            }
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( Delta delta -> recordDelta, restSetters )
         -> ( Delta delta, restDeltas )
         -> ( State state, restStates )
         ->
            { newCmds : List (Cmd ( Delta delta, restDeltas ))
            , newStates : End -> ( State state, restStates )
            }
        )
        ((List (Subcontrol ( Delta delta, restDeltas ))
          -> List Alert
          -> End
          -> End
          -> End
          -> List (Subcontrol ( Delta delta, restDeltas ))
         )
         -> List (Subcontrol ( Delta delta, restDeltas ))
         -> List Alert
         -> ( RecordFns input1 state delta output1 recordInput, restFns )
         -> ( Delta delta -> recordDelta, restSetters )
         -> ( State state, restStates )
         -> List (Subcontrol ( Delta delta, restDeltas ))
        )
    ->
        AdvancedControl
            input
            ( State state, restStates )
            ( Delta delta, restDeltas )
            output
endRecord (RecordBuilder rec) =
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

                        TagSelected index ->
                            ( State { s | selected = index } state
                            , Cmd.none
                            )

                        _ ->
                            ( State s state, Cmd.none )

                subcontrolViews config =
                    viewRecordStates rec.viewer fns deltaSetters config

                view config =
                    viewRecordStates rec.viewer fns deltaSetters config
                        |> List.concatMap .html

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
                , initBlank =
                    ( State { status = Intact_, selected = 1 } initialStates
                    , initialiseRecordDeltas rec.deltaInitialiser deltaSetters initialDeltas
                    )
                , initPrefilled = \output -> initialiseRecordStates rec.initialiser output fns deltaSetters
                , baseUpdate = \_ -> update
                , update = update
                , subControlViews = \config -> subcontrolViews config
                , view = view
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


collectRecordSubscriptions :
    ((List (Sub msg)
      -> End
      -> End
      -> End
      -> List (Sub msg)
     )
     -> List (Sub msg)
     -> ( setter, restSetters )
     -> ( RecordFns input state delta output recordInput, restFns )
     -> ( State state, restStates )
     -> List (Sub msg)
    )
    -> ( setter, restSetters )
    -> ( RecordFns input state delta output recordInput, restFns )
    -> ( State state, restStates )
    -> Sub (Delta msg)
collectRecordSubscriptions collector setters fns states =
    collector (\listSubs End End End -> listSubs) [] setters fns states
        |> Sub.batch
        |> Sub.map ChangeStateInternally


recordSubscriptionCollector :
    (List (Sub msg)
     -> restDeltas
     -> restFns
     -> restStates
     -> List (Sub msg)
    )
    -> List (Sub msg)
    -> ( Delta delta -> msg, restDeltas )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List (Sub msg)
recordSubscriptionCollector next listSubs ( setter, restSetters ) ( RecordFns fns, restFns ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.fns
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
            fns.fns
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
            fns.fns
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
            fns.fns
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
    (((End -> state)
      -> List (Cmd msg)
      -> b
      -> End
      -> End
      -> ( State state, Cmd (Delta msg) )
     )
     -> (a -> a)
     -> List c
     -> d
     -> e
     -> f
     -> g
    )
    -> d
    -> e
    -> f
    -> g
initialiseRecordStates initialiser input fns deltaSetters =
    initialiser
        (\states deltas _ End End ->
            ( State { status = Intact_, selected = 1 } (states End)
            , Cmd.batch deltas |> Cmd.map ChangeStateInternally
            )
        )
        identity
        []
        input
        fns
        deltaSetters


recordStateInitialiser :
    ((a2 -> c) -> List (Cmd msg) -> a -> b -> d -> e)
    -> (( State state, a2 ) -> c)
    -> List (Cmd msg)
    -> a
    -> ( RecordFns a1 state delta a1 a, b )
    -> ( Delta delta -> msg, d )
    -> e
recordStateInitialiser next states deltas recordInput ( RecordFns fns, restFns ) ( deltaSetter, restDeltaSetters ) =
    let
        (ControlFns controlFns) =
            fns.fns

        ( state, delta ) =
            recordInput
                |> fns.fromInput
                |> controlFns.initPrefilled
    in
    next (states << Tuple.pair state) (Cmd.map deltaSetter delta :: deltas) recordInput restFns restDeltaSetters


initialiseRecordDeltas :
    ((List (Cmd delta) -> End -> End -> List (Cmd delta))
     -> List (Cmd delta)
     -> deltaSetters
     -> deltas
     -> List (Cmd delta)
    )
    -> deltaSetters
    -> deltas
    -> Cmd (Delta delta)
initialiseRecordDeltas deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_ (\cmdList End End -> cmdList) [] deltaSetters deltas
        |> Cmd.batch
        |> Cmd.map ChangeStateInternally


recordDeltaInitialiser :
    (List (Cmd delta1) -> restDeltaSetters -> restDeltas -> List (Cmd delta1))
    -> List (Cmd delta1)
    -> ( delta -> delta1, restDeltaSetters )
    -> ( Cmd delta, restDeltas )
    -> List (Cmd delta1)
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
            fns.fns
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
            fns.fns
    in
    ( controlFns.setAllIdle state
    , next restFns restStates
    )


viewRecordStates :
    ((List (Subcontrol msg)
      -> List Alert
      -> End
      -> End
      -> End
      -> List (Subcontrol msg)
     )
     -> List (Subcontrol msg)
     -> List Alert
     -> ( RecordFns input state delta output recordOutput, restFns )
     -> ( Delta delta -> recordDelta, restDeltaSetters )
     -> ( State state, restStates )
     -> List (Subcontrol msg)
    )
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> InternalViewConfig ( State state, restStates )
    -> List (Subcontrol msg)
viewRecordStates viewer fns setters config =
    viewer (\views _ End End End -> views) [] config.alerts fns setters config.state


recordStateViewer :
    (List (Subcontrol recordDelta)
     -> List Alert
     -> restFns
     -> restDeltaSetters
     -> restStates
     -> List (Subcontrol recordDelta)
    )
    -> List (Subcontrol recordDelta)
    -> List Alert
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( State state, restStates )
    -> List (Subcontrol recordDelta)
recordStateViewer next views alerts ( RecordFns fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.fns

        view =
            controlFns.view
                { id = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.id
                , name = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.name
                , label = controlFns.label
                , class = controlFns.class
                , state = state
                , status = getStatus controlFns.parse controlFns.collectErrors alerts (State internalState state)
                , alerts = alerts
                , selected = internalState.selected
                }
                |> List.map (H.map (\delta -> ChangeStateOnInput (setter delta)))
    in
    next
        (views
            ++ [ { html = view
                 , label = controlFns.label
                 , index = controlFns.index
                 }
               ]
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
     -> { newStates : recordState, newCmds : List (Cmd recordDelta) }
    )
    -> { newStates : ( State state, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) }
    -> ( RecordFns input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> { newStates : recordState, newCmds : List (Cmd recordDelta) }
recordStateUpdater next { newStates, newCmds } ( RecordFns fns, restFns ) ( deltaSetter, restDeltaSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        (ControlFns controlFns) =
            fns.fns

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


{-| A data structure used to build custom types
-}
type CustomTypeBuilder applyInputs debouncingReceiverCollector deltaAfter deltaAfters deltaBefore deltaBefores destructor errorCollector alertEmitter fns idleSetter initialDeltas initialStates initialiseDeltas makeDeltaSetters makeStateSetters parser stateAfter stateAfters stateBefore stateBefores stateInserter subscriptionCollector toArgStates updater viewer
    = CustomTypeBuilder
        { applyInputs : applyInputs
        , debouncingReceiverCollector : debouncingReceiverCollector
        , deltaAfter : deltaAfter
        , deltaAfters : deltaAfters
        , deltaBefore : deltaBefore
        , deltaBefores : deltaBefores
        , destructor : destructor
        , errorCollector : errorCollector
        , alertEmitter : alertEmitter
        , fns : fns
        , idleSetter : idleSetter
        , index : Int
        , initialDeltas : initialDeltas
        , initialStates : initialStates
        , initialiseDeltas : initialiseDeltas
        , makeDeltaSetters : makeDeltaSetters
        , inputToStateConverters : makeStateSetters
        , parser : parser
        , stateAfter : stateAfter
        , stateAfters : stateAfters
        , stateBefore : stateBefore
        , stateBefores : stateBefores
        , initialStateOverrider : stateInserter
        , subscriptionCollector : subscriptionCollector
        , toArgStates : toArgStates
        , updater : updater
        , viewer : viewer
        }


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
            |> endCustomType

-}
customType :
    destructor
    ->
        CustomTypeBuilder
            (applyInputs -> applyInputs)
            (debouncingReceiverCollector -> debouncingReceiverCollector)
            End
            End
            (deltaBefore -> deltaBefore)
            (deltaBefores -> deltaBefores)
            destructor
            (errorCollector -> errorCollector)
            (alertEmitter -> alertEmitter)
            (Path -> fns -> fns)
            (idleSetter -> idleSetter)
            (Path -> initialDeltas -> initialDeltas)
            (Path -> initialStates -> initialStates)
            (initialiseDeltas -> initialiseDeltas)
            (makeDeltaSetters -> makeDeltaSetters)
            (makeStateSetters -> makeStateSetters)
            (parser -> parser)
            End
            End
            (stateBefore -> stateBefore)
            (stateBefores -> stateBefores)
            (stateInserter -> stateInserter)
            (subscriptionCollector -> subscriptionCollector)
            (toArgStates -> toArgStates)
            (updater -> updater)
            (viewer -> viewer)
customType destructor =
    CustomTypeBuilder
        { index = 0
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
        , inputToStateConverters = identity
        , initialStateOverrider = identity
        , applyInputs = identity
        , alertEmitter = identity
        , errorCollector = identity
        , debouncingReceiverCollector = identity
        , subscriptionCollector = identity
        , destructor = destructor
        }


tagHelper :
    String
    -> AdvancedControl input9 state9 delta13 output9
    -> a22
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14, a19 ) -> c7)
            (( ( Delta delta14, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path -> ( ControlFns input9 state9 delta13 output9, a15 ) -> c5)
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path -> ( Cmd (Delta delta13), a13 ) -> c4)
            (Path.Path -> ( State state9, a12 ) -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( a22, a2 ) -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
tagHelper label_ internalRecord toArgState (CustomTypeBuilder builder) =
    let
        (Control control) =
            internalRecord
                |> label label_

        newIndex =
            builder.index + 1
    in
    CustomTypeBuilder
        { index = newIndex
        , fns =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                builder.fns path
                    << Tuple.pair
                        (ControlFns { controlFns | index = newIndex })
        , initialStates =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                builder.initialStates path
                    << Tuple.pair
                        (controlFns.initBlank
                            |> Tuple.first
                        )
        , initialDeltas =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                builder.initialDeltas path
                    << Tuple.pair
                        (controlFns.initBlank
                            |> Tuple.second
                        )
        , updater = builder.updater >> customTypeStateUpdater
        , viewer = builder.viewer >> selectedTagViewer
        , parser = builder.parser >> selectedTagParser
        , idleSetter = builder.idleSetter >> selectedTagIdleSetter
        , deltaBefore = builder.deltaBefore << Tuple.pair Skip
        , deltaBefores = builder.deltaBefores << Tuple.pair builder.deltaBefore
        , deltaAfter = ( Skip, builder.deltaAfter )
        , deltaAfters = ( builder.deltaAfter, builder.deltaAfters )
        , makeDeltaSetters = builder.makeDeltaSetters >> deltaSetterMaker
        , initialiseDeltas = builder.initialiseDeltas >> customTypeDeltaInitialiser
        , stateBefore = builder.stateBefore << Tuple.pair Nothing
        , stateBefores = builder.stateBefores << Tuple.pair builder.stateBefore
        , toArgStates = builder.toArgStates << Tuple.pair toArgState
        , stateAfter = ( Nothing, builder.stateAfter )
        , stateAfters = ( builder.stateAfter, builder.stateAfters )
        , inputToStateConverters = builder.inputToStateConverters >> convertInputToState
        , initialStateOverrider = builder.initialStateOverrider >> initialStateOverrider
        , applyInputs = builder.applyInputs >> inputToStateConverterToDestructorApplier
        , alertEmitter = builder.alertEmitter >> customTypeAlertEmitter
        , errorCollector = builder.errorCollector >> customTypeErrorCollector
        , debouncingReceiverCollector = builder.debouncingReceiverCollector >> customTypeDebouncingReceiverCollector
        , subscriptionCollector = builder.subscriptionCollector >> customTypeSubscriptionCollector
        , destructor = builder.destructor
        }


{-| Finalise the construction of a `customType` combinator.

    type alias Foo
        = Bar
        | Baz


    helloControl =
        customType
            (\bar baz tag ->
                case tag ofcustomType
                    Bar -> bar
                    Baz -> baz
            )
            |> tag0 "Bar" Bar
            |> tag0 "Baz" Baz
            |> endCustomType

-}
endCustomType :
    CustomTypeBuilder
        (((input
           ->
            ( State ( State state, restStates )
            , Cmd (Delta ( Delta delta, restDeltas ))
            )
          )
          -> End
          -> input
          ->
            ( State ( State state, restStates )
            , Cmd (Delta ( Delta delta, restDeltas ))
            )
         )
         -> (inputToStateConverter -> destructor)
         -> ( inputToStateConverter, restInputToStateConverters )
         -> input
         ->
            ( State ( State state, restStates )
            , Cmd (Delta ( Delta delta, restDeltas ))
            )
        )
        ((List Alert -> End -> End -> List Alert)
         -> List Alert
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( State state, restStates )
         -> List Alert
        )
        deltaAfter
        afters
        deltaBefore
        (End -> befores)
        (inputToStateConverter -> destructor)
        ((List Alert -> List Feedback -> End -> End -> List Feedback)
         -> List Alert
         -> List Feedback
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( State state, restStates )
         -> List Feedback
        )
        ((List Alert -> Int -> End -> End -> List Alert)
         -> List Alert
         -> Int
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( State state, restStates )
         -> List Alert
        )
        (Path -> End -> ( ControlFns input1 state delta output1, restFns ))
        ((Int -> End -> End -> End)
         -> Int
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( State state, restStates )
         -> ( State state, restStates )
        )
        (Path -> End -> deltas)
        (Path -> End -> ( State state, restStates ))
        ((List (Cmd ( Delta delta, restDeltas ))
          -> End
          -> End
          -> List (Cmd ( Delta delta, restDeltas ))
         )
         -> List (Cmd ( Delta delta, restDeltas ))
         -> ( setter, restSetters )
         -> deltas
         -> List (Cmd ( Delta delta, restDeltas ))
        )
        ((End -> End -> End) -> befores -> afters -> ( setter, restSetters ))
        (({ c | finalTagStates : End -> b2 } -> b2)
         ->
            { controlFns : ( ControlFns input1 state delta output1, restFns )
            , deltaSetters : ( setter, restSetters )
            , finalTagStates : a1 -> a1
            , initialTagStates : ( State state, restStates )
            , inputTuplizers : g
            , maybeOverridesAfter : h
            , maybeOverridesBefore : i
            , tagStateOverrider : j
            }
         -> ( inputToStateConverter, restInputToStateConverters )
        )
        ((a -> b1 -> End -> End -> a)
         -> Result (List Feedback) value
         -> Int
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( State state, restStates )
         -> Result (List Feedback) output
        )
        stateAfter
        h
        stateBefore
        (End -> i)
        j
        ((List (Sub ( Delta delta, restDeltas ))
          -> End
          -> End
          -> End
          -> List (Sub ( Delta delta, restDeltas ))
         )
         -> List (Sub ( Delta delta, restDeltas ))
         -> ( setter, restSetters )
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( State state, restStates )
         -> List (Sub ( Delta delta, restDeltas ))
        )
        (End -> g)
        (({ newCmds : List (Cmd ( Delta delta, restDeltas ))
          , newStates : End -> ( State state, restStates )
          }
          -> End
          -> End
          -> End
          -> End
          ->
            { newCmds : List (Cmd ( Delta delta, restDeltas ))
            , newStates : End -> ( State state, restStates )
            }
         )
         ->
            { newCmds : List (Cmd ( Delta delta, restDeltas ))
            , newStates : ( State state, restStates ) -> ( State state, restStates )
            }
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( setter, restSetters )
         -> ( Delta delta, restDeltas )
         -> ( State state, restStates )
         ->
            { newCmds : List (Cmd ( Delta delta, restDeltas ))
            , newStates : End -> ( State state, restStates )
            }
        )
        ((List (Subcontrol ( Delta delta, restDeltas ))
          -> b
          -> End
          -> End
          -> End
          -> List (Subcontrol ( Delta delta, restDeltas ))
         )
         -> List (Subcontrol ( Delta delta, restDeltas ))
         -> List Alert
         -> ( ControlFns input1 state delta output1, restFns )
         -> ( setter, restSetters )
         -> ( State state, restStates )
         -> List (Subcontrol ( Delta delta, restDeltas ))
        )
    ->
        AdvancedControl
            input
            ( State state, restStates )
            ( Delta delta, restDeltas )
            output
endCustomType (CustomTypeBuilder builder) =
    Control
        (\path ->
            let
                fns =
                    builder.fns path End

                initialStates =
                    builder.initialStates path End

                initialDeltas =
                    builder.initialDeltas path End

                deltaSetters =
                    makeDeltaSetters
                        builder.makeDeltaSetters
                        builder.deltaBefores
                        builder.deltaAfters

                stateSetters =
                    makeInputToStateConverters
                        builder.inputToStateConverters
                        builder.initialStateOverrider
                        initialStates
                        fns
                        builder.toArgStates
                        builder.stateBefores
                        builder.stateAfters
                        deltaSetters

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
                                        updateCustomTypeStates builder.updater fns deltaSetters tagDelta state
                                in
                                ( State internalState newTagStates
                                , Cmd.map ChangeStateOnInput cmd
                                )

                            ChangeStateInternally tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates builder.updater fns deltaSetters tagDelta state
                                in
                                ( State internalState newTagStates
                                , Cmd.map ChangeStateInternally cmd
                                )

                            _ ->
                                ( State internalState state, Cmd.none )

                subcontrolView config =
                    viewSelectedTagState builder.viewer fns deltaSetters config

                view config =
                    customTypeView config subcontrolView

                parse =
                    \(State internalState state) -> validateSelectedTagState builder.parser internalState.selected fns state

                setAllIdle =
                    \(State internalState state) ->
                        State
                            { internalState | status = Idle_ }
                            (setSelectedTagStateIdle builder.idleSetter internalState.selected fns state)

                emitAlerts (State internalState state) =
                    emitAlertsForCustomType builder.alertEmitter internalState.selected fns state
            in
            ControlFns
                { path = path
                , index = 0
                , initBlank =
                    ( State { status = Intact_, selected = 1 } initialStates
                    , Cmd.batch (initialiseCustomTypeDeltas builder.initialiseDeltas deltaSetters initialDeltas)
                    )
                , initPrefilled =
                    \tag ->
                        let
                            destructor =
                                applyInputToStateConvertersToDestructor builder.applyInputs builder.destructor stateSetters

                            ( (State { selected } _) as initPrefilledState, initPrefilledDelta ) =
                                destructor tag

                            deltas =
                                initialiseCustomTypeDeltas builder.initialiseDeltas deltaSetters initialDeltas
                                    |> List.indexedMap
                                        (\idx initDelta ->
                                            if (idx + 1) == selected then
                                                initPrefilledDelta

                                            else
                                                initDelta
                                        )
                        in
                        ( initPrefilledState
                        , Cmd.batch deltas
                        )
                , baseUpdate = \_ -> update
                , update = update
                , subControlViews = subcontrolView
                , view = view
                , parse = parse
                , setAllIdle = setAllIdle
                , emitAlerts = emitAlerts
                , collectErrors = \(State _ states) alerts -> collectErrorsForCustomType builder.errorCollector alerts fns states
                , receiverCount = 0
                , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForCustomType builder.debouncingReceiverCollector fns states
                , label = "Custom Type"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \(State _ states) -> collectCustomTypeSubscriptions builder.subscriptionCollector deltaSetters fns states
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
            |> tag0 "Unit" Unit
            |> endCustomType

-}
tag0 :
    String
    -> output9
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14, a19 ) -> c7)
            (( ( Delta delta14, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path -> ( ControlFns output9 () () output9, a15 ) -> c5)
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path -> ( Cmd (Delta ()), a13 ) -> c4)
            (Path.Path -> ( State (), a12 ) -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( (output9 -> b) -> b, a2 ) -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
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
        { blank = ( (), Cmd.none )
        , prefill = \_ -> ( (), Cmd.none )
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
            |> endCustomType

-}
tag1 :
    String
    -> (output10 -> output9)
    -> AdvancedControl output10 state9 delta13 output10
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14, a19 ) -> c7)
            (( ( Delta delta14, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path
             ->
                ( ControlFns
                    ( output10, b )
                    ( State state9, End )
                    ( Delta delta13, End )
                    output9
                , a15
                )
             -> c5
            )
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path -> ( Cmd (Delta ( Delta delta13, End )), a13 ) -> c4)
            (Path.Path -> ( State ( State state9, End ), a12 ) -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( (( d, End ) -> f) -> d -> f, a2 ) -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
tag1 label_ tag control =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control
            |> endRecord
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
            |> tag2 "Point" Point float float

-}
tag2 :
    String
    -> (output11 -> output10 -> output9)
    -> AdvancedControl output11 state10 delta14 output11
    -> AdvancedControl output10 state9 delta13 output10
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14_1, a19 ) -> c7)
            (( ( Delta delta14_1, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path
             ->
                ( ControlFns
                    ( output11, ( output10, b ) )
                    ( State state10, ( State state9, End ) )
                    ( Delta delta14, ( Delta delta13, End ) )
                    output9
                , a15
                )
             -> c5
            )
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path
             -> ( Cmd (Delta ( Delta delta14, ( Delta delta13, End ) )), a13 )
             -> c4
            )
            (Path.Path
             -> ( State ( State state10, ( State state9, End ) ), a12 )
             -> c3
            )
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( (( d, ( f, End ) ) -> g) -> d -> f -> g, a2 ) -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
tag2 label_ tag control1 control2 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> endRecord
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
            |> tag3 "Point3D" Point3D float float float

-}
tag3 :
    String
    -> (output12 -> output11 -> output10 -> output9)
    -> AdvancedControl output12 state11 delta15 output12
    -> AdvancedControl output11 state10 delta14 output11
    -> AdvancedControl output10 state9 delta13 output10
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14_1, a19 ) -> c7)
            (( ( Delta delta14_1, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path
             ->
                ( ControlFns
                    ( output12, ( output11, ( output10, b ) ) )
                    ( State state11, ( State state10, ( State state9, End ) ) )
                    ( Delta delta15, ( Delta delta14, ( Delta delta13, End ) ) )
                    output9
                , a15
                )
             -> c5
            )
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path
             ->
                ( Cmd
                    (Delta
                        ( Delta delta15, ( Delta delta14, ( Delta delta13, End ) ) )
                    )
                , a13
                )
             -> c4
            )
            (Path.Path
             ->
                ( State ( State state11, ( State state10, ( State state9, End ) ) )
                , a12
                )
             -> c3
            )
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( (( d, ( f, ( g, End ) ) ) -> h) -> d -> f -> g -> h, a2 ) -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
tag3 label_ tag control1 control2 control3 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> field (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> endRecord
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, End ) ) )
        )


{-| Add a tag with four arguments to a custom type.
-}
tag4 :
    String
    -> (output13 -> output12 -> output11 -> output10 -> output9)
    -> AdvancedControl output13 state12 delta16 output13
    -> AdvancedControl output12 state11 delta15 output12
    -> AdvancedControl output11 state10 delta14 output11
    -> AdvancedControl output10 state9 delta13 output10
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14_1, a19 ) -> c7)
            (( ( Delta delta14_1, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path
             ->
                ( ControlFns
                    ( output13, ( output12, ( output11, ( output10, b ) ) ) )
                    ( State state12
                    , ( State state11, ( State state10, ( State state9, End ) ) )
                    )
                    ( Delta delta16
                    , ( Delta delta15, ( Delta delta14, ( Delta delta13, End ) ) )
                    )
                    output9
                , a15
                )
             -> c5
            )
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path
             ->
                ( Cmd
                    (Delta
                        ( Delta delta16
                        , ( Delta delta15
                          , ( Delta delta14, ( Delta delta13, End ) )
                          )
                        )
                    )
                , a13
                )
             -> c4
            )
            (Path.Path
             ->
                ( State
                    ( State state12
                    , ( State state11, ( State state10, ( State state9, End ) ) )
                    )
                , a12
                )
             -> c3
            )
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( (( d, ( f, ( g, ( h, End ) ) ) ) -> i) -> d -> f -> g -> h -> i, a2 )
             -> c
            )
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
tag4 label_ tag control1 control2 control3 control4 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> field (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> field (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> endRecord
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 arg4 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, ( arg4, End ) ) ) )
        )


{-| Add a tag with five arguments to a custom type.
-}
tag5 :
    String
    -> (output14 -> output13 -> output12 -> output11 -> output10 -> output9)
    -> AdvancedControl output14 state13 delta17 output14
    -> AdvancedControl output13 state12 delta16 output13
    -> AdvancedControl output12 state11 delta15 output12
    -> AdvancedControl output11 state10 delta14 output11
    -> AdvancedControl output10 state9 delta13 output10
    ->
        CustomTypeBuilder
            (a21
             -> destructor1
             -> restInputToStateConverters
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20 -> List Alert -> restFns7 -> restStates8 -> List Alert)
            deltaAfter
            deltaAfters
            (( Delta delta14_1, a19 ) -> c7)
            (( ( Delta delta14_1, a19 ) -> c7, a18 ) -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> restFns6
             -> restStates7
             -> List Feedback
            )
            (a16 -> List Alert -> Int -> restFns5 -> restStates6 -> List Alert)
            (Path.Path
             ->
                ( ControlFns
                    ( output14
                    , ( output13, ( output12, ( output11, ( output10, b ) ) ) )
                    )
                    ( State state13
                    , ( State state12
                      , ( State state11, ( State state10, ( State state9, End ) ) )
                      )
                    )
                    ( Delta delta17
                    , ( Delta delta16
                      , ( Delta delta15, ( Delta delta14, ( Delta delta13, End ) ) )
                      )
                    )
                    output9
                , a15
                )
             -> c5
            )
            (a14 -> Int -> restFns4 -> restStates5 -> restStates5)
            (Path.Path
             ->
                ( Cmd
                    (Delta
                        ( Delta delta17
                        , ( Delta delta16
                          , ( Delta delta15
                            , ( Delta delta14, ( Delta delta13, End ) )
                            )
                          )
                        )
                    )
                , a13
                )
             -> c4
            )
            (Path.Path
             ->
                ( State
                    ( State state13
                    , ( State state12
                      , ( State state11, ( State state10, ( State state9, End ) ) )
                      )
                    )
                , a12
                )
             -> c3
            )
            (a11
             -> List (Cmd delta1_2)
             -> restDeltaSetters2
             -> restDeltas1
             -> List (Cmd delta1_2)
            )
            (a10 -> befores -> afters -> next)
            (a9
             ->
                { controlFns : restControlFns
                , deltaSetters : restDeltaSetters1
                , finalTagStates : nextFinalTagState -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers : restInputTuplizers
                , maybeOverridesAfter : restMaybeOverridesAfter
                , maybeOverridesBefore : restMaybeOverridesBefore
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> restFns3
             -> restStates3
             -> Result (List Feedback) output3
            )
            stateAfter
            stateAfters
            (( Maybe a23, a6 ) -> c2)
            (( ( Maybe a23, a6 ) -> c2, a5 ) -> c1)
            (a4
             -> Int
             -> Int
             -> (restArgStates -> tagStates)
             -> restMaybeArgStates
             -> restArgStates
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> restSetters1
             -> restFns2
             -> restStates2
             -> List (Sub delta1_1)
            )
            (( (( d, ( f, ( g, ( h, ( i, End ) ) ) ) ) -> j)
               -> d
               -> f
               -> g
               -> h
               -> i
               -> j
             , a2
             )
             -> c
            )
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : restStates1 -> recordState0
                }
             -> restFns1
             -> restDeltaSetters
             -> restDeltas
             -> restStates1
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> restFns
             -> restSetters
             -> restStates
             -> List (Subcontrol delta)
            )
    ->
        CustomTypeBuilder
            (a21
             -> (inputToStateConverter -> destructor1)
             -> ( inputToStateConverter, restInputToStateConverters )
             -> tag
             -> ( State tagStates2, Cmd delta12 )
            )
            (a20
             -> List Alert
             -> ( ControlFns input8 state8 delta11 output8, restFns7 )
             -> ( State state8, restStates8 )
             -> List Alert
            )
            ( Delta delta10, deltaAfter )
            ( deltaAfter, deltaAfters )
            (a19 -> c7)
            (a18 -> c6)
            destructor
            (a17
             -> List Alert
             -> List Feedback
             -> ( ControlFns input7 state7 delta9 output7, restFns6 )
             -> ( State state7, restStates7 )
             -> List Feedback
            )
            (a16
             -> List Alert
             -> Int
             -> ( ControlFns input6 state6 delta8 output6, restFns5 )
             -> ( State state6, restStates6 )
             -> List Alert
            )
            (Path.Path -> a15 -> c5)
            (a14
             -> Int
             -> ( ControlFns input5 state5 delta7 output5, restFns4 )
             -> ( State state5, restStates5 )
             -> ( State state5, restStates5 )
            )
            (Path.Path -> a13 -> c4)
            (Path.Path -> a12 -> c3)
            (a11
             -> List (Cmd delta1_2)
             -> ( delta6 -> delta1_2, restDeltaSetters2 )
             -> ( Cmd delta6, restDeltas1 )
             -> List (Cmd delta1_2)
            )
            (a10
             -> ( ( value, after ) -> delta5, befores )
             -> ( after, afters )
             -> ( value -> delta5, next )
            )
            (a9
             ->
                { controlFns :
                    ( ControlFns input4 state1_1 delta4 output4, restControlFns )
                , deltaSetters : ( Delta delta4 -> tagDelta, restDeltaSetters1 )
                , finalTagStates :
                    ( finalTagState, nextFinalTagState ) -> restFinalTagStates
                , initialTagStates : ( State state4, restStates4 )
                , inputTuplizers :
                    ( (input4 -> ( State tagStates1, Cmd (Delta tagDelta) ))
                      -> finalTagState
                    , restInputTuplizers
                    )
                , maybeOverridesAfter :
                    ( maybeOverrideAfter, restMaybeOverridesAfter )
                , maybeOverridesBefore :
                    ( ( Maybe (State state1_1), maybeOverrideAfter )
                      -> ( Maybe (State state4), restMaybeStates )
                    , restMaybeOverridesBefore
                    )
                , tagStateOverrider :
                    (Int
                     -> Int
                     -> (End -> tagStates1)
                     -> End
                     -> End
                     -> State tagStates1
                    )
                    -> Int
                    -> Int
                    -> (identity -> identity)
                    -> ( Maybe (State state4), restMaybeStates )
                    -> ( State state4, restStates4 )
                    -> State tagStates1
                }
             -> toFinalTagStates
            )
            (a8
             -> Result (List Feedback) output3
             -> Int
             -> ( ControlFns input3 state3 delta3 output3, restFns3 )
             -> ( State state3, restStates3 )
             -> Result (List Feedback) output3
            )
            ( Maybe a7, stateAfter )
            ( stateAfter, stateAfters )
            (a6 -> c2)
            (a5 -> c1)
            (a4
             -> Int
             -> Int
             -> (( State argState, restArgStates ) -> tagStates)
             -> ( Maybe (State argState), restMaybeArgStates )
             -> ( State argState, restArgStates )
             -> State tagStates
            )
            (a3
             -> List (Sub delta1_1)
             -> ( Delta delta2 -> delta1_1, restSetters1 )
             -> ( ControlFns input2 state2 delta2 output2, restFns2 )
             -> ( State state2, restStates2 )
             -> List (Sub delta1_1)
            )
            (a2 -> c)
            (a1
             ->
                { newCmds : List (Cmd recordDelta)
                , newStates : ( State state1, restStates1 ) -> recordState0
                }
             -> ( ControlFns input1 state1 delta1 output1, restFns1 )
             -> ( Delta delta1 -> recordDelta, restDeltaSetters )
             -> ( Delta delta1, restDeltas )
             -> ( State state1, restStates1 )
             -> { newCmds : List (Cmd recordDelta), newStates : recordState }
            )
            (a
             -> List (Subcontrol delta)
             -> List Alert
             -> ( ControlFns input state e output, restFns )
             -> ( Delta e -> delta, restSetters )
             -> ( State state, restStates )
             -> List (Subcontrol delta)
            )
tag5 label_ tag control1 control2 control3 control4 control5 =
    tagHelper
        label_
        (record tag
            |> field Tuple.first control1
            |> field (Tuple.second >> Tuple.first) control2
            |> field (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> field (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> field (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control5
            |> endRecord
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


collectCustomTypeSubscriptions :
    ((List (Sub delta) -> End -> End -> End -> List (Sub delta)) -> List (Sub delta) -> setters -> fns -> states -> List (Sub delta))
    -> setters
    -> fns
    -> states
    -> Sub (Delta delta)
collectCustomTypeSubscriptions collector setters fns states =
    collector (\listSubs End End End -> listSubs) [] setters fns states
        |> Sub.batch
        |> Sub.map ChangeStateInternally


customTypeSubscriptionCollector :
    (List (Sub delta1) -> restSetters -> restFns -> restStates -> List (Sub delta1))
    -> List (Sub delta1)
    -> ( Delta delta -> delta1, restSetters )
    -> ( ControlFns input state delta output, restFns )
    -> ( State state, restStates )
    -> List (Sub delta1)
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
updateCustomTypeStates updater fns setters deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output End End End End -> output)
                { newStates = identity, newCmds = [] }
                fns
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
     -> { newStates : recordState, newCmds : List (Cmd recordDelta) }
    )
    -> { newStates : ( State state, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) }
    -> ( ControlFns input state delta output, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> { newStates : recordState, newCmds : List (Cmd recordDelta) }
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


initialiseCustomTypeDeltas :
    ((List (Cmd delta) -> End -> End -> List (Cmd delta))
     -> List (Cmd delta)
     -> deltaSetters
     -> deltas
     -> List (Cmd delta)
    )
    -> deltaSetters
    -> deltas
    -> List (Cmd (Delta delta))
initialiseCustomTypeDeltas deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_ (\cmdList End End -> cmdList) [] deltaSetters deltas
        |> List.reverse
        |> List.map (Cmd.map ChangeStateInternally)


customTypeDeltaInitialiser :
    (List (Cmd delta1) -> restDeltaSetters -> restDeltas -> List (Cmd delta1))
    -> List (Cmd delta1)
    -> ( delta -> delta1, restDeltaSetters )
    -> ( Cmd delta, restDeltas )
    -> List (Cmd delta1)
customTypeDeltaInitialiser next cmdList ( setter, restSetters ) ( delta, restDeltas ) =
    next (Cmd.map setter delta :: cmdList) restSetters restDeltas


applyInputToStateConvertersToDestructor :
    (((tag -> ( State tagStates, Cmd delta ))
      -> End
      -> (tag -> ( State tagStates, Cmd delta ))
     )
     -> (inputToStateConverter -> destructor)
     -> ( inputToStateConverter, restInputToStateConverters )
     -> (tag -> ( State tagStates, Cmd delta ))
    )
    -> (inputToStateConverter -> destructor)
    -> ( inputToStateConverter, restInputToStateConverters )
    -> (tag -> ( State tagStates, Cmd delta ))
applyInputToStateConvertersToDestructor inputToStateConverterToDestructorApplier_ destructor inputToStateConverters =
    inputToStateConverterToDestructorApplier_
        (\finalDestructor End -> finalDestructor)
        destructor
        inputToStateConverters


inputToStateConverterToDestructorApplier :
    (destructor
     -> restInputToStateConverters
     -> (tag -> ( State tagStates, Cmd delta ))
    )
    -> (inputToStateConverter -> destructor)
    -> ( inputToStateConverter, restInputToStateConverters )
    -> (tag -> ( State tagStates, Cmd delta ))
inputToStateConverterToDestructorApplier next destructor ( inputToStateConverter, restInputToStateConverters ) =
    next (destructor inputToStateConverter) restInputToStateConverters


makeInputToStateConverters :
    (({ c | finalTagStates : End -> b } -> b)
     ->
        { controlFns : d
        , deltaSetters : e
        , finalTagStates : a -> a
        , initialTagStates : f
        , inputTuplizers : g
        , maybeOverridesAfter : h
        , maybeOverridesBefore : i
        , tagStateOverrider : j
        }
     -> k
    )
    -> j
    -> f
    -> d
    -> (End -> g)
    -> (End -> i)
    -> h
    -> e
    -> k
makeInputToStateConverters inputToStateConverters_ initialStateOverrider_ initialTagStates fns inputTuplizers maybeOverridesBefore maybeOverridesAfter deltaSetters =
    inputToStateConverters_
        (\{ finalTagStates } -> finalTagStates End)
        { finalTagStates = identity
        , tagStateOverrider = initialStateOverrider_
        , initialTagStates = initialTagStates
        , controlFns = fns
        , inputTuplizers = inputTuplizers End
        , maybeOverridesBefore = maybeOverridesBefore End
        , maybeOverridesAfter = maybeOverridesAfter
        , deltaSetters = deltaSetters
        }


convertInputToState :
    ({ controlFns : restControlFns
     , deltaSetters : restDeltaSetters
     , finalTagStates : nextFinalTagState -> restFinalTagStates
     , initialTagStates : ( State state, restStates )
     , inputTuplizers : restInputTuplizers
     , maybeOverridesAfter : restMaybeOverridesAfter
     , maybeOverridesBefore : restMaybeOverridesBefore
     , tagStateOverrider :
        (Int -> Int -> (End -> tagStates) -> End -> End -> State tagStates)
        -> Int
        -> Int
        -> (identity -> identity)
        -> ( Maybe (State state), restMaybeStates )
        -> ( State state, restStates )
        -> State tagStates
     }
     -> toFinalTagStates
    )
    ->
        { controlFns : ( ControlFns input state1 delta output, restControlFns )
        , deltaSetters : ( Delta delta -> tagDelta, restDeltaSetters )
        , finalTagStates : ( finalTagState, nextFinalTagState ) -> restFinalTagStates
        , initialTagStates : ( State state, restStates )
        , inputTuplizers :
            ( (input -> ( State tagStates, Cmd (Delta tagDelta) )) -> finalTagState, restInputTuplizers )
        , maybeOverridesAfter : ( maybeOverrideAfter, restMaybeOverridesAfter )
        , maybeOverridesBefore :
            ( ( Maybe (State state1), maybeOverrideAfter )
              -> ( Maybe (State state), restMaybeStates )
            , restMaybeOverridesBefore
            )
        , tagStateOverrider :
            (Int -> Int -> (End -> tagStates) -> End -> End -> State tagStates)
            -> Int
            -> Int
            -> (identity -> identity)
            -> ( Maybe (State state), restMaybeStates )
            -> ( State state, restStates )
            -> State tagStates
        }
    -> toFinalTagStates
convertInputToState next { finalTagStates, tagStateOverrider, initialTagStates, controlFns, inputTuplizers, maybeOverridesBefore, maybeOverridesAfter, deltaSetters } =
    let
        ( ControlFns fns, restFns ) =
            controlFns

        ( inputTuplizer, restInputTuplizers ) =
            inputTuplizers

        ( maybeOverrideBefore, restMaybeOverrideBefores ) =
            maybeOverridesBefore

        ( maybeOverrideAfter, restMaybeOverrideAfters ) =
            maybeOverridesAfter

        ( deltaSetter, restDeltaSetters ) =
            deltaSetters

        finalTagState =
            inputTuplizer
                (\tupledInput ->
                    let
                        ( state, delta ) =
                            fns.initPrefilled tupledInput

                        maybeOverrides =
                            maybeOverrideBefore ( Just state, maybeOverrideAfter )
                    in
                    ( overrideInitialStates tagStateOverrider maybeOverrides initialTagStates
                    , Cmd.map (deltaSetter >> ChangeStateInternally) delta
                    )
                )
    in
    next
        { finalTagStates = finalTagStates << Tuple.pair finalTagState
        , tagStateOverrider = tagStateOverrider
        , initialTagStates = initialTagStates
        , controlFns = restFns
        , inputTuplizers = restInputTuplizers
        , maybeOverridesBefore = restMaybeOverrideBefores
        , maybeOverridesAfter = restMaybeOverrideAfters
        , deltaSetters = restDeltaSetters
        }


overrideInitialStates :
    ((Int
      -> Int
      -> (End -> tagStates)
      -> End
      -> End
      -> State tagStates
     )
     -> Int
     -> Int
     -> (c -> c)
     -> ( Maybe (State state), restMaybeStates )
     -> ( State state, restStates )
     -> State tagStates
    )
    -> ( Maybe (State state), restMaybeStates )
    -> ( State state, restStates )
    -> State tagStates
overrideInitialStates initialStateOverrider_ maybeOverrides initialTagStates =
    initialStateOverrider_
        (\_ selectedTag finalTagStates End End -> State { status = Intact_, selected = selectedTag } (finalTagStates End))
        1
        1
        identity
        maybeOverrides
        initialTagStates


initialStateOverrider :
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
initialStateOverrider next thisTagIndex currentlySelectedTagIndex tagStates ( thisMaybeOverride, restMaybeOverrides ) ( initialTagState, restInitialTagStates ) =
    let
        ( selectedTag, tagArgState ) =
            case thisMaybeOverride of
                Just override ->
                    ( thisTagIndex, override )

                Nothing ->
                    ( currentlySelectedTagIndex, initialTagState )
    in
    next (thisTagIndex + 1) selectedTag (tagStates << Tuple.pair tagArgState) restMaybeOverrides restInitialTagStates


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
    ((List (Subcontrol delta) -> b -> End -> End -> End -> List (Subcontrol delta))
     -> List (Subcontrol delta)
     -> List Alert
     -> fns
     -> setters
     -> state
     -> List (Subcontrol delta)
    )
    -> fns
    -> setters
    -> InternalViewConfig state
    -> List (Subcontrol delta)
viewSelectedTagState viewer fns setters config =
    viewer (\listSubcontrol _ End End End -> listSubcontrol) [] config.alerts fns setters config.state


selectedTagViewer :
    (List (Subcontrol delta) -> List Alert -> restFns -> restSetters -> restStates -> List (Subcontrol delta))
    -> List (Subcontrol delta)
    -> List Alert
    -> ( ControlFns input state e output, restFns )
    -> ( Delta e -> delta, restSetters )
    -> ( State state, restStates )
    -> List (Subcontrol delta)
selectedTagViewer next listSubcontrol alerts ( ControlFns fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    next
        (listSubcontrol
            ++ [ { html =
                    fns.view
                        { id = Maybe.withDefault ("control-" ++ Path.toString fns.path) fns.id
                        , name = Maybe.withDefault ("control-" ++ Path.toString fns.path) fns.name
                        , label = fns.label
                        , class = fns.class
                        , state = state
                        , status = Intact
                        , alerts = alerts
                        , selected = internalState.selected
                        }
                        |> List.map (H.map (setter >> ChangeStateOnInput))
                 , label = fns.label
                 , index = fns.index
                 }
               ]
        )
        alerts
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
    InternalViewConfig state
    -> (InternalViewConfig state -> List (Subcontrol delta))
    -> List (Html (Delta delta))
customTypeView config toSubcontrols =
    let
        subcontrols =
            toSubcontrols config

        subcontrolView =
            List.filterMap
                (\sc ->
                    if sc.index == config.selected then
                        Just sc.html

                    else
                        Nothing
                )
                subcontrols
                |> List.concat
    in
    if List.length subcontrols > 1 then
        radioView
            { options = List.map (\sc -> ( sc.index, sc.label )) subcontrols
            , selectedOption = config.selected
            , toMsg = TagSelected
            , id = config.id
            , name = config.name
            , label = config.label
            }
            ++ subcontrolView

    else
        subcontrolView


button : msg -> String -> Html msg
button msg text =
    H.button
        [ HA.type_ "button"
        , HE.onClick msg
        ]
        [ H.text text ]


listView :
    Path
    -> InternalViewConfig (List (State state))
    -> List Alert
    -> (Path -> ControlFns input state delta output)
    -> List (Html (Delta (ListDelta delta)))
listView path config debouncingReceivers subcontrol =
    let
        view_ =
            H.div
                [ HA.id config.id
                , HA.class "control-container"
                ]
                [ H.label
                    [ HA.for config.id ]
                    [ H.text config.label ]
                , if List.isEmpty config.state then
                    button (ChangeStateOnInput (InsertItem 0)) "Add item"

                  else
                    H.ol []
                        (List.indexedMap
                            (\idx (State internalState state) ->
                                let
                                    itemPath =
                                        Path.add idx path

                                    (ControlFns itemFns) =
                                        subcontrol itemPath

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
                                            config.alerts

                                    filteredAlerts2 =
                                        List.filter (\f -> not <| List.member f debouncingReceivers) filteredAlerts1
                                in
                                H.li []
                                    [ H.div []
                                        (itemFns.view
                                            { id = Maybe.withDefault ("control-" ++ Path.toString itemPath) itemFns.id
                                            , name = Maybe.withDefault ("control-" ++ Path.toString itemPath) itemFns.name
                                            , label = itemFns.label
                                            , class = itemFns.class
                                            , state = state
                                            , status = getStatus itemFns.parse itemFns.collectErrors filteredAlerts2 (State internalState state)
                                            , alerts = filteredAlerts2
                                            , selected = internalState.selected
                                            }
                                        )
                                        |> H.map (ChangeItem idx)
                                    , button (DeleteItem idx) "Delete item"
                                    , H.div [] [ button (InsertItem (idx + 1)) "Insert item" ]
                                    ]
                            )
                            config.state
                        )
                        |> H.map ChangeStateOnInput
                ]
    in
    [ view_ ]


textControlView : String -> { state : String, id : String, label : String, name : String, class : String } -> List (Html String)
textControlView inputmode config =
    [ H.label [ HA.for config.id ] [ H.text config.label ]
    , H.input
        [ HE.onInput identity
        , HA.name config.name
        , HA.id config.id
        , HA.class config.class
        , HA.attribute "inputmode" inputmode
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
