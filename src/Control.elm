module Control exposing
    ( Control, Form, sandbox, simpleForm, form
    , bool, int, float, string, char, enum
    , tuple, triple, maybe, result, list, dict, set, array, map
    , ControlConfig, define
    , failIf, noteIf
    , alertIf, respond
    , alertAtIndexes
    , default, debounce, id, name, label, class, classList, wrapView
    , RecordBuilder, record, field, endRecord, LayoutConfig, Subcontrol, layout
    , CustomTypeBuilder, customType, tag0, tag1, tag2, tag3, tag4, tag5, endCustomType
    , State, Delta, ListDelta, End
    , AdvancedControl, ControlFns, Alert, RecordFns, Status, InternalViewConfig, Path, Feedback
    , alertAtIndexesWithContext, alertIfWithContext, failIfWithContext, formWithContext, noteIfWithContext, simpleFormWithContext
    )

{-|


# Creating a form

@docs Control, Form, sandbox, simpleForm, form


# Basic controls

@docs bool, int, float, string, char, enum


# Basic combinators

@docs tuple, triple, maybe, result, list, dict, set, array, map


# Creating a new control

@docs ControlConfig, define


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
type alias FormWithContext context state delta output msg =
    { blank : ( State state, Cmd msg )
    , prefill : output -> ( State state, Cmd msg )
    , update : context -> Delta delta -> State state -> ( State state, Cmd msg )
    , view : context -> State state -> Html msg
    , subscriptions : context -> State state -> Sub msg
    , submit : context -> State state -> ( State state, Result (List Feedback) output )
    }


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
type alias ControlConfig context state delta output =
    { blank : ( state, Cmd delta )
    , prefill : output -> ( state, Cmd delta )
    , update : context -> delta -> state -> ( state, Cmd delta )
    , view : context -> { label : String, id : String, name : String, class : String, state : state } -> List (Html delta)
    , subscriptions : context -> state -> Sub delta
    , parse : context -> state -> Result (List String) output
    , label : String
    }


{-| A `Control` is the basic unit from which forms are composed. This library
provides various example controls and combinators that you can use to build a
form for any arbitrarily complex Elm type.
-}
type alias Control context state delta output =
    AdvancedControl context output state delta output


{-| A slightly more flexible version of `Control`
-}
type AdvancedControl context input state delta output
    = Control (Path -> ControlFns context input state delta output)


{-| Some internal stuff needed to build up a record control
-}
type RecordFns context input state delta output recordOutput
    = RecordFns
        { controlFns : ControlFns context input state delta output
        , fromInput : recordOutput -> output
        }


{-| Some internal stuff needed to build up record and custom type controls
-}
type ControlFns context input state delta output
    = ControlFns
        { index : Int
        , initBlank : ( State state, Cmd (Delta delta) )
        , initPrefilled : input -> ( State state, Cmd (Delta delta) )
        , baseUpdate : Float -> context -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
        , update : context -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
        , view : context -> InternalViewConfig state -> List (Html (Delta delta))
        , subControlViews : context -> InternalViewConfig state -> List (Subcontrol delta)
        , parse : context -> State state -> Result (List Feedback) output
        , path : Path
        , emitAlerts : context -> State state -> List Alert
        , collectDebouncingReceivers : State state -> List Alert
        , collectFeedback : State state -> List Alert -> List Feedback
        , receiverCount : Int
        , setAllIdle : State state -> State state
        , label : String
        , id : Maybe String
        , name : Maybe String
        , class : List String
        , subscriptions : context -> State state -> Sub (Delta delta)
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
    = NoDelta
    | StateChangedByInput delta
    | StateChangedInternally delta
    | DebounceTimerSet Time.Posix
    | DebounceTimerExpired Time.Posix
    | TagSelected Int


{-| Special `Delta` type used only by `list` controls
-}
type ListDelta delta
    = ItemInserted Int
    | ItemDeleted Int
    | ItemUpdated Int (Delta delta)



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
simpleForm :
    { control : Control () state delta output, onUpdate : Delta delta -> msg, onSubmit : msg }
    -> Form state delta output msg
simpleForm { onUpdate, onSubmit, control } =
    form
        { control = control
        , onUpdate = onUpdate
        , view =
            \controlView ->
                H.form [ HE.onSubmit onSubmit ]
                    (controlView ++ [ H.button [ HA.type_ "submit" ] [ H.text "Submit" ] ])
        }


simpleFormWithContext :
    { control : Control context state delta output, onUpdate : Delta delta -> msg, onSubmit : msg }
    -> FormWithContext context state delta output msg
simpleFormWithContext { onUpdate, onSubmit, control } =
    formWithContext
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
form :
    { control : Control () state delta output
    , onUpdate : Delta delta -> msg
    , view : List (Html msg) -> Html msg
    }
    -> Form state delta output msg
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
            fns.update () msg state
                |> Tuple.mapSecond (Cmd.map onUpdate)
    , view =
        \((State internalState state) as s) ->
            let
                emittedAlerts =
                    fns.emitAlerts () s

                debouncingReceivers =
                    fns.collectDebouncingReceivers s

                alerts =
                    List.filter (\emittedAlert -> not <| List.member emittedAlert debouncingReceivers) emittedAlerts

                status =
                    getStatus fns.parse fns.collectFeedback alerts () s
            in
            view
                (fns.view ()
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
                    fns.parse () state

                validationErrors =
                    fns.emitAlerts () state
                        |> fns.collectFeedback state
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
            fns.subscriptions () state
                |> Sub.map onUpdate
    }


formWithContext :
    { control : Control context state delta output, onUpdate : Delta delta -> msg, view : List (Html msg) -> Html msg }
    -> FormWithContext context state delta output msg
formWithContext { control, onUpdate, view } =
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
        \ctx msg state ->
            fns.update ctx msg state
                |> Tuple.mapSecond (Cmd.map onUpdate)
    , view =
        \ctx ((State internalState state) as s) ->
            let
                emittedAlerts =
                    fns.emitAlerts ctx s

                debouncingReceivers =
                    fns.collectDebouncingReceivers s

                alerts =
                    List.filter (\emittedAlert -> not <| List.member emittedAlert debouncingReceivers) emittedAlerts

                status =
                    getStatus fns.parse fns.collectFeedback alerts ctx s
            in
            view
                (fns.view ctx
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
        \ctx state ->
            let
                parsingResult =
                    fns.parse ctx state

                validationErrors =
                    fns.emitAlerts ctx state
                        |> fns.collectFeedback state
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
        \ctx state ->
            fns.subscriptions ctx state
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
sandbox :
    { outputToString : output -> String
    , control : Control context state delta output
    , context : context
    }
    -> Program () (State state) (Delta delta)
sandbox { outputToString, control, context } =
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
                fns.update context msg state
        , view =
            \((State internalState state) as s) ->
                let
                    emittedAlerts =
                        fns.emitAlerts context s

                    debouncingReceivers =
                        fns.collectDebouncingReceivers s

                    alerts =
                        List.filter (\f -> not <| List.member f debouncingReceivers) emittedAlerts

                    parsingResult =
                        fns.parse context s

                    validationErrors =
                        emittedAlerts
                            |> fns.collectFeedback s
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
                        (fns.view context
                            { id = Maybe.withDefault (Path.toString path) fns.id
                            , name = Maybe.withDefault (Path.toString path) fns.name
                            , label = fns.label
                            , class = fns.class
                            , state = state
                            , status = getStatus fns.parse fns.collectFeedback alerts context (State internalState state)
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
        , subscriptions = fns.subscriptions context
        }



{-
    .o88b.  .d88b.  d8b   db d888888b d8888b.  .d88b.  db      .d8888.
   d8P  Y8 .8P  Y8. 888o  88 `~~88~~' 88  `8D .8P  Y8. 88      88'  YP
   8P      88    88 88V8o 88    88    88oobY' 88    88 88      `8bo.
   8b      88    88 88 V8o88    88    88`8b   88    88 88        `Y8b.
   Y8b  d8 `8b  d8' 88  V888    88    88 `88. `8b  d8' 88booo. db   8D
    `Y88P'  `Y88P'  VP   V8P    YP    88   YD  `Y88P'  Y88888P `8888Y'
-}


{-| Define a new type of control, with any arbitrary `state` and `delta` types,
and producing any arbitrary `output` type.

Here's how we could create a control like the famous
[counter example](https://elm-lang.org/examples/buttons) from the Elm Guide

    type CounterDelta
        = Increment
        | Decrement

    counterControl : Control Int CounterDelta Int
    counterControl =
        create
            { blank = 0
            , prefill = identity
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
define : ControlConfig context state delta output -> Control context state delta output
define controlConfig =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate controlConfig.update

                parse =
                    \ctx (State _ state) ->
                        controlConfig.parse ctx state
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
                        |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , initPrefilled =
                    \input ->
                        controlConfig.prefill input
                            |> Tuple.mapFirst (State { status = Intact_, selected = 1 })
                            |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , baseUpdate = preUpdate
                , update = preUpdate 0
                , subControlViews = \_ _ -> []
                , view =
                    \ctx viewConfig ->
                        [ controlConfig.view ctx
                            { state = viewConfig.state
                            , label = viewConfig.label
                            , id = viewConfig.id
                            , name = viewConfig.name
                            , class = String.join " " viewConfig.class
                            }
                            |> wrappedView viewConfig.status
                            |> H.map StateChangedByInput
                        ]
                , parse = parse
                , setAllIdle = \(State i s) -> State { i | status = Idle_ } s
                , emitAlerts = \_ _ -> []
                , collectDebouncingReceivers = \_ -> []
                , collectFeedback = \_ _ -> []
                , receiverCount = 0
                , label = controlConfig.label
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions =
                    \ctx (State _ s) ->
                        controlConfig.subscriptions ctx s
                            |> Sub.map StateChangedInternally
                }
        )


wrapUpdate :
    (context -> delta -> state -> ( state, Cmd delta ))
    -> Float
    -> context
    -> Delta delta
    -> State state
    -> ( State state, Cmd (Delta delta) )
wrapUpdate innerUpdate debounce_ ctx wrappedDelta (State internalState state) =
    case wrappedDelta of
        NoDelta ->
            ( State internalState state
            , Cmd.none
            )

        StateChangedInternally delta ->
            let
                ( newState, cmd ) =
                    innerUpdate ctx delta state
            in
            ( State internalState newState
            , Cmd.map StateChangedInternally cmd
            )

        StateChangedByInput delta ->
            let
                ( newState, cmd ) =
                    innerUpdate ctx delta state
            in
            if debounce_ > 0 then
                ( State internalState newState
                , Cmd.batch
                    [ Task.perform DebounceTimerSet Time.now
                    , Cmd.map StateChangedByInput cmd
                    ]
                )

            else
                ( State { internalState | status = Idle_ } newState
                , Cmd.map StateChangedByInput cmd
                )

        DebounceTimerSet now ->
            ( State { internalState | status = DebouncingSince now } state
            , Task.perform (\() -> DebounceTimerExpired now) (Process.sleep debounce_)
            )

        DebounceTimerExpired now ->
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
alertIfWithContext : (context -> output -> Bool) -> String -> Control context state delta output -> Control context state delta output
alertIfWithContext when alert (Control control) =
    Control (control >> alertEmitter when (AlertLabel alert))


alertIf : (output -> Bool) -> String -> Control a state delta output -> Control a state delta output
alertIf when alert control =
    alertIfWithContext (\_ -> when) alert control


alertEmitter : (context -> output -> Bool) -> Alert -> ControlFns context input state delta output -> ControlFns context input state delta output
alertEmitter check alert (ControlFns ctrl) =
    ControlFns
        { ctrl
            | emitAlerts =
                \ctx state ->
                    let
                        oldAlerts =
                            ctrl.emitAlerts ctx state

                        newAlerts =
                            case ctrl.parse ctx state of
                                Ok output ->
                                    if check ctx output then
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
respond : { alert : String, fail : Bool, message : String } -> Control context state delta output -> Control context state delta output
respond { alert, fail, message } (Control control) =
    Control (control >> alertReceiver (AlertLabel alert) fail message)


alertReceiver : Alert -> Bool -> String -> ControlFns context input state delta output -> ControlFns context input state delta output
alertReceiver alert fail message (ControlFns ctrl) =
    ControlFns
        { ctrl
            | collectFeedback =
                \state alerts ->
                    let
                        oldReceiver =
                            ctrl.collectFeedback state alerts

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
alertAtIndexesWithContext :
    (context -> output -> List Int)
    -> String
    -> Control context (List state) (ListDelta delta) output
    -> Control context (List state) (ListDelta delta) output
alertAtIndexesWithContext toIndexes alert (Control control) =
    Control (control >> listAlertEmitter toIndexes alert)


alertAtIndexes :
    (output -> List Int)
    -> String
    -> Control a (List state) (ListDelta delta) output
    -> Control a (List state) (ListDelta delta) output
alertAtIndexes toIndexes alert control =
    alertAtIndexesWithContext (\_ -> toIndexes) alert control


listAlertEmitter :
    (context -> output -> List Int)
    -> String
    -> ControlFns context input (List state) (ListDelta delta) output
    -> ControlFns context input (List state) (ListDelta delta) output
listAlertEmitter check alertLabel (ControlFns ctrl) =
    ControlFns
        { ctrl
            | emitAlerts =
                \ctx state ->
                    let
                        oldAlerts =
                            ctrl.emitAlerts ctx state

                        newAlerts =
                            case ctrl.parse ctx state of
                                Ok output ->
                                    [ AlertList ctrl.path alertLabel (check ctx output) ]

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
failIfWithContext : (context -> output -> Bool) -> String -> Control context state delta output -> Control context state delta output
failIfWithContext check message (Control c) =
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


failIf : (output -> Bool) -> String -> Control a state delta output -> Control a state delta output
failIf check message control =
    failIfWithContext (\_ -> check) message control


{-| Display an note on a `Control` if its output fails to meet the predicate.

This just shows the user a message - it doesn't cause the `Control` to fail
validation.

    positiveInt =
        int
            |> noteIf
                (\x -> x < 1)
                "Should this be greater than zero?"

-}
noteIfWithContext : (context -> output -> Bool) -> String -> Control context state delta output -> Control context state delta output
noteIfWithContext check message (Control c) =
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


noteIf : (output -> Bool) -> String -> Control a state delta output -> Control a state delta output
noteIf check message control =
    noteIfWithContext (\_ -> check) message control



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
debounce : Float -> Control context state delta output -> Control context state delta output
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
id : String -> Control context state delta output -> Control context state delta output
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
name : String -> Control context state delta output -> Control context state delta output
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
label : String -> AdvancedControl context input state delta output -> AdvancedControl context input state delta output
label label_ (Control control) =
    let
        labeller (ControlFns i) =
            ControlFns
                { i
                    | label = label_
                    , parse =
                        \ctx state ->
                            i.parse ctx state
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
class : String -> Control context state delta output -> Control context state delta output
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
classList : List ( String, Bool ) -> Control context state delta output -> Control context state delta output
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
    -> Control context state delta output
    -> Control context state delta output
layout view (Control control) =
    let
        viewer (ControlFns fns) =
            ControlFns
                { fns
                    | view =
                        \ctx internalViewConfig ->
                            let
                                layoutConfig =
                                    { class = String.join " " internalViewConfig.class
                                    , id = internalViewConfig.id
                                    , label = internalViewConfig.label
                                    , selected = internalViewConfig.selected
                                    , selectMsg = TagSelected
                                    }

                                subcontrols =
                                    fns.subControlViews ctx internalViewConfig
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
    -> Control context state delta output
    -> Control context state delta output
wrapView wrapper (Control control) =
    let
        viewer (ControlFns i) =
            ControlFns
                { i
                    | view =
                        \ctx config ->
                            wrapper (i.view ctx config)
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
default : input -> AdvancedControl context input state delta output -> AdvancedControl context input state delta output
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
int : Control context String String Int
int =
    define
        { blank = ( "", Cmd.none )
        , prefill = \s -> ( String.fromInt s, Cmd.none )
        , update = \_ delta _ -> ( delta, Cmd.none )
        , view = \_ -> textControlView "numeric"
        , parse =
            \_ state ->
                case String.toInt state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "Must be a whole number" ]
        , subscriptions = \_ _ -> Sub.none
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
float : Control context String String Float
float =
    define
        { blank = ( "", Cmd.none )
        , prefill = \f -> ( String.fromFloat f, Cmd.none )
        , update = \_ delta _ -> ( delta, Cmd.none )
        , view = \_ -> textControlView "decimal"
        , parse =
            \_ state ->
                case String.toFloat state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "Must be a number" ]
        , subscriptions = \_ _ -> Sub.none
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
string : Control context String String String
string =
    define
        { blank = ( "", Cmd.none )
        , prefill = \s -> ( s, Cmd.none )
        , update = \_ delta _ -> ( delta, Cmd.none )
        , view = \_ -> textControlView "text"
        , parse = \_ -> Ok
        , subscriptions = \_ _ -> Sub.none
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
char : Control context String String Char
char =
    define
        { blank = ( "", Cmd.none )
        , prefill = \c -> ( String.fromChar c, Cmd.none )
        , update = \_ delta _ -> ( delta, Cmd.none )
        , view = \_ -> textControlView "text"
        , parse =
            \_ str ->
                case String.uncons str of
                    Just ( char_, rest ) ->
                        if String.isEmpty rest then
                            Ok char_

                        else
                            Err [ "Must be exactly one character" ]

                    Nothing ->
                        Err [ "Must not be blank" ]
        , subscriptions = \_ _ -> Sub.none
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
    -> Control context enum enum enum
enum first second rest =
    define
        { blank = ( Tuple.second first, Cmd.none )
        , prefill = \e -> ( e, Cmd.none )
        , update = \_ delta _ -> ( delta, Cmd.none )
        , view = \_ -> enumView (first :: second :: rest)
        , parse = \_ -> Ok
        , subscriptions = \_ _ -> Sub.none
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
bool : Control context Bool Bool Bool
bool =
    define
        { blank = ( False, Cmd.none )
        , prefill = \b -> ( b, Cmd.none )
        , view =
            \_ config ->
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
        , update = \_ delta _ -> ( delta, Cmd.none )
        , parse = \_ -> Ok
        , subscriptions = \_ _ -> Sub.none
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
    -> Control context state delta a
    -> Control context ( State state, End ) ( Delta delta, End ) b
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
    Control context state delta output
    ->
        Control
            context
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
    Control context failureState failureDelta failureOutput
    -> Control context successState successDelta successOutput
    ->
        Control
            context
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
    Control context state1 delta1 output1
    -> Control context state2 delta2 output2
    -> Control context ( State state1, ( State state2, End ) ) ( Delta delta1, ( Delta delta2, End ) ) ( output1, output2 )
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
    Control context state1 delta1 output1
    -> Control context state2 delta2 output2
    -> Control context state3 delta3 output3
    ->
        Control
            context
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
    Control context state delta output
    -> Control context (List (State state)) (ListDelta delta) (List output)
list (Control ctrl) =
    Control
        (\path ->
            let
                update =
                    wrapUpdate listUpdate

                listUpdate ctx delta state =
                    case delta of
                        ItemInserted idx ->
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
                            , Cmd.map (ItemUpdated idx) initialCmd
                            )

                        ItemUpdated idx itemDelta ->
                            let
                                ( newState, cmd ) =
                                    List.Extra.indexedFoldr
                                        (\thisIdx item ( items, prevCmd ) ->
                                            if thisIdx == idx then
                                                let
                                                    (ControlFns itemControl) =
                                                        ctrl (Path.add idx path)

                                                    ( newItem, newCmd ) =
                                                        itemControl.update ctx itemDelta item
                                                in
                                                ( newItem :: items, newCmd )

                                            else
                                                ( item :: items, prevCmd )
                                        )
                                        ( [], Cmd.none )
                                        state
                            in
                            ( newState
                            , Cmd.map (ItemUpdated idx) cmd
                            )

                        ItemDeleted idx ->
                            ( List.Extra.removeAt idx state, Cmd.none )

                parse ctx (State _ state) =
                    List.foldr
                        (\( idx, item ) res ->
                            let
                                (ControlFns itemControl) =
                                    ctrl (Path.add idx path)
                            in
                            case res of
                                Ok outputs ->
                                    case itemControl.parse ctx item of
                                        Ok output ->
                                            Ok (output :: outputs)

                                        Err errs ->
                                            Err errs

                                Err errs ->
                                    case itemControl.parse ctx item of
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
                                        , Cmd.map (ItemUpdated idx) itemCmd :: itemCmds
                                        )
                                    )
                                    ( [], [] )
                                    input
                        in
                        ( State { status = Intact_, selected = 1 } initialState
                        , Cmd.map StateChangedInternally (Cmd.batch initialCmds)
                        )
                , initBlank =
                    ( State { status = Intact_, selected = 1 } []
                    , Cmd.none
                    )
                , baseUpdate = update
                , update = update 0
                , subControlViews = \_ _ -> []
                , view =
                    \ctx config ->
                        let
                            debouncingReceivers =
                                -- this is a total hack!
                                collectDebouncingReceivers (State { status = Intact_, selected = config.selected } config.state)
                        in
                        listView path ctx config debouncingReceivers ctrl
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
                    \ctx (State _ s) ->
                        List.indexedMap
                            (\idx item ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                itemControl.emitAlerts ctx item
                            )
                            s
                            |> List.concat
                , collectFeedback =
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
                                itemControl.collectFeedback item filteredAlerts
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
                    \ctx (State _ listState) ->
                        List.indexedMap
                            (\idx itemState ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                itemControl.subscriptions ctx itemState
                                    |> Sub.map (ItemUpdated idx)
                            )
                            listState
                            |> Sub.batch
                            |> Sub.map StateChangedInternally
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
    Control context keyState keyDelta comparable
    -> Control context valueState valueDelta value
    ->
        Control
            context
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
        |> alertAtIndexesWithContext
            (\ctx output ->
                output
                    |> List.map Tuple.first
                    |> nonUniqueIndexes ctx
            )
            "@@dict-unique-keys"
        |> label "Dict"
        |> map { convert = Dict.fromList, revert = Dict.toList }


nonUniqueIndexes : context -> List comparable -> List Int
nonUniqueIndexes ctx listState =
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
    Control context state delta comparable
    -> Control context ( State (List (State state)), End ) ( Delta (ListDelta delta), End ) (Set.Set comparable)
set memberControl =
    list
        (memberControl |> respond { alert = "@@set-unique-keys", fail = True, message = "Set members must be unique" })
        |> alertAtIndexesWithContext nonUniqueIndexes "@@set-unique-keys"
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
    Control context state delta output
    ->
        Control
            context
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


nestForwards : (c -> a -> b) -> (b -> d) -> c -> a -> d
nestForwards mkBifunctor previous this =
    \next -> previous (mkBifunctor this next)


nestBackwards : (a -> b -> c) -> b -> a -> c
nestBackwards mkBifunctor this =
    \next -> mkBifunctor next this


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
field fromInput (Control control) (RecordBuilder builder) =
    let
        newIndex =
            builder.index + 1
    in
    RecordBuilder
        { index = newIndex
        , toOutput = builder.toOutput
        , fns =
            \path ->
                let
                    previousFns =
                        builder.fns path

                    newPath =
                        Path.add newIndex path

                    (ControlFns fns) =
                        control newPath

                    newFns =
                        RecordFns
                            { controlFns = ControlFns { fns | index = newIndex }
                            , fromInput = fromInput
                            }
                in
                nestForwards Tuple.pair previousFns newFns
        , initialStates =
            \path ->
                let
                    newPath =
                        Path.add newIndex path

                    (ControlFns fns) =
                        control newPath

                    previousInitialStates =
                        builder.initialStates path

                    newInitialState =
                        Tuple.first fns.initBlank
                in
                nestForwards Tuple.pair previousInitialStates newInitialState
        , initialDeltas =
            \path ->
                let
                    newPath =
                        Path.add newIndex path

                    (ControlFns fns) =
                        control newPath

                    previousInitialDeltas =
                        builder.initialDeltas path

                    newInitialDelta =
                        Tuple.second fns.initBlank
                in
                nestForwards Tuple.pair previousInitialDeltas newInitialDelta
        , updater = builder.updater >> recordStateUpdater
        , viewer = builder.viewer >> recordStateViewer
        , parser = builder.parser >> recordStateValidator
        , idleSetter = builder.idleSetter >> recordStateIdleSetter
        , initialiser = builder.initialiser >> recordStateInitialiser
        , deltaInitialiser = builder.deltaInitialiser >> recordDeltaInitialiser
        , before = nestForwards Tuple.pair builder.before NoDelta
        , befores = nestForwards Tuple.pair builder.befores builder.before
        , after = nestBackwards Tuple.pair builder.after NoDelta
        , afters = nestBackwards Tuple.pair builder.afters builder.after
        , makeSetters = builder.makeSetters >> deltaSetterMaker
        , alertEmitter = builder.alertEmitter >> recordAlertEmitter
        , errorCollector = builder.errorCollector >> recordFeedbackCollector
        , debouncingReceiverCollector = builder.debouncingReceiverCollector >> recordDebouncingReceiverCollector
        , subscriptionCollector = builder.subscriptionCollector >> recordSubscriptionCollector
        }


{-| Finalise the construction of a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field string
            |> endRecord

-}
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

                update ctx delta (State s state) =
                    case delta of
                        NoDelta ->
                            ( State s state, Cmd.none )

                        StateChangedByInput deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater ctx fns deltaSetters deltas state
                            in
                            ( State s newState
                            , Cmd.map StateChangedByInput cmd
                            )

                        StateChangedInternally deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater ctx fns deltaSetters deltas state
                            in
                            ( State s newState
                            , Cmd.map StateChangedInternally cmd
                            )

                        TagSelected index ->
                            ( State { s | selected = index } state
                            , Cmd.none
                            )

                        _ ->
                            ( State s state, Cmd.none )

                subcontrolViews ctx config =
                    viewRecordStates rec.viewer ctx fns deltaSetters config

                view ctx config =
                    viewRecordStates rec.viewer ctx fns deltaSetters config
                        |> List.concatMap .html

                parse ctx (State _ state) =
                    validateRecordStates rec.parser rec.toOutput fns ctx state

                setAllIdle (State i state) =
                    State { i | status = Idle_ } (setAllRecordStatesToIdle rec.idleSetter fns state)

                emitAlerts ctx (State _ state) =
                    emitAlertsForRecord rec.alertEmitter fns ctx state
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
                , subControlViews = subcontrolViews
                , view = view
                , parse = parse
                , setAllIdle = setAllIdle
                , emitAlerts = emitAlerts
                , collectFeedback = \(State _ states) alerts -> collectFeedbackForRecord rec.errorCollector alerts fns states
                , receiverCount = 0
                , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForRecord rec.debouncingReceiverCollector fns states
                , label = "Record"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \ctx (State _ states) -> collectRecordSubscriptions rec.subscriptionCollector deltaSetters fns ctx states
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
    (({ listSubs : List (Sub msg)
      , setters : End
      , fns : End
      , context : context
      , states : End
      }
      -> List (Sub msg)
     )
     ->
        { listSubs : List (Sub msg)
        , setters : ( setter, restDeltaSetters )
        , fns : ( RecordFns context input state delta output recordInput, restFns )
        , context : context
        , states : ( State state, restStates )
        }
     -> List (Sub msg)
    )
    -> ( setter, restDeltaSetters )
    -> ( RecordFns context input state delta output recordInput, restFns )
    -> context
    -> ( State state, restStates )
    -> Sub (Delta msg)
collectRecordSubscriptions collector setters fns ctx states =
    collector (\{ listSubs } -> listSubs) { listSubs = [], setters = setters, fns = fns, context = ctx, states = states }
        |> Sub.batch
        |> Sub.map StateChangedInternally


recordSubscriptionCollector :
    ({ listSubs : List (Sub msg)
     , setters : restDeltas
     , fns : restFns
     , context : context
     , states : restStates
     }
     -> List (Sub msg)
    )
    ->
        { listSubs : List (Sub msg)
        , setters : ( Delta delta -> msg, restDeltas )
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , context : context
        , states : ( State state, restStates )
        }
    -> List (Sub msg)
recordSubscriptionCollector next { listSubs, setters, fns, context, states } =
    let
        ( setter, restDeltaSetters ) =
            setters

        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns

        ( state, restStates ) =
            states

        newSub =
            controlFns.subscriptions context state
                |> Sub.map setter
    in
    next
        { listSubs = newSub :: listSubs
        , setters = restDeltaSetters
        , fns = restFns
        , context = context
        , states = restStates
        }


collectDebouncingReceiversForRecord :
    (({ alerts : List Alert
      , fns : End
      , states : End
      }
      -> List Alert
     )
     ->
        { alerts : List Alert
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , states : ( State state, restStates )
        }
     -> List Alert
    )
    -> ( RecordFns context input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Alert
collectDebouncingReceiversForRecord debouncingReceiverCollector_ fns states =
    debouncingReceiverCollector_ (\{ alerts } -> alerts) { alerts = [], fns = fns, states = states }


recordDebouncingReceiverCollector :
    ({ alerts : List Alert
     , fns : restFns
     , states : restStates
     }
     -> List Alert
    )
    ->
        { alerts : List Alert
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , states : ( State state, restStates )
        }
    -> List Alert
recordDebouncingReceiverCollector next { alerts, fns, states } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns

        ( state, restStates ) =
            states
    in
    next
        { alerts = alerts ++ controlFns.collectDebouncingReceivers state
        , fns = restFns
        , states = restStates
        }


collectFeedbackForRecord :
    (({ alerts : List Alert
      , feedback : List Feedback
      , fns : End
      , states : End
      }
      -> List Feedback
     )
     ->
        { alerts : List Alert
        , feedback : List Feedback
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , states : ( State state, restStates )
        }
     -> List Feedback
    )
    -> List Alert
    -> ( RecordFns context input state delta output recordOutput, restFns )
    -> ( State state, restStates )
    -> List Feedback
collectFeedbackForRecord feedbackCollector_ alerts fns states =
    feedbackCollector_ (\{ feedback } -> feedback)
        { alerts = alerts
        , feedback = []
        , fns = fns
        , states = states
        }


recordFeedbackCollector :
    ({ alerts : List Alert
     , feedback : List Feedback
     , fns : restFns
     , states : restStates
     }
     -> List Feedback
    )
    ->
        { alerts : List Alert
        , feedback : List Feedback
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , states : ( State state, restStates )
        }
    -> List Feedback
recordFeedbackCollector next { alerts, feedback, fns, states } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns

        ( state, restStates ) =
            states
    in
    next
        { alerts = alerts
        , feedback = feedback ++ controlFns.collectFeedback state alerts
        , fns = restFns
        , states = restStates
        }


emitAlertsForRecord :
    (({ alerts : List Alert
      , fns : End
      , context : context
      , states : End
      }
      -> List Alert
     )
     ->
        { alerts : List Alert
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , context : context
        , states : ( State state, restStates )
        }
     -> List Alert
    )
    -> ( RecordFns context input state delta output recordOutput, restFns )
    -> context
    -> ( State state, restStates )
    -> List Alert
emitAlertsForRecord alertEmitter_ fns ctx states =
    alertEmitter_ (\{ alerts } -> alerts)
        { alerts = []
        , fns = fns
        , context = ctx
        , states = states
        }


recordAlertEmitter :
    ({ alerts : List Alert
     , fns : restFns
     , context : context
     , states : restStates
     }
     -> List Alert
    )
    ->
        { alerts : List Alert
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , context : context
        , states : ( State state, restStates )
        }
    -> List Alert
recordAlertEmitter next { alerts, context, fns, states } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns

        ( state, restStates ) =
            states

        newAlerts =
            controlFns.emitAlerts context state
    in
    next
        { alerts = alerts ++ newAlerts
        , fns = restFns
        , context = context
        , states = restStates
        }


makeDeltaSetters :
    (({ output : End -> deltaSetters
      , befores : End
      , afters : End
      }
      -> deltaSetters
     )
     ->
        { output : identity -> identity
        , befores : befores
        , afters : afters
        }
     -> deltaSetters
    )
    -> (End -> befores)
    -> afters
    -> deltaSetters
makeDeltaSetters makeSetters_ befores afters =
    makeSetters_ (\{ output } -> output End)
        { output = identity
        , befores = befores End
        , afters = afters
        }


deltaSetterMaker :
    ({ output : restDeltaSetters -> deltaSetters
     , befores : befores
     , afters : afters
     }
     -> deltaSetters
    )
    ->
        { output : ( value -> delta, restDeltaSetters ) -> deltaSetters
        , befores : ( ( value, after ) -> delta, befores )
        , afters : ( after, afters )
        }
    -> deltaSetters
deltaSetterMaker next { output, befores, afters } =
    let
        ( before, restBefores ) =
            befores

        ( after, restAfters ) =
            afters
    in
    next
        { output = nestForwards Tuple.pair output (\value -> before ( value, after ))
        , befores = restBefores
        , afters = restAfters
        }



-- initialiseRecordStates :
--     (((End -> state)
--       -> List (Cmd msg)
--       -> b
--       -> End
--       -> End
--       -> ( State state, Cmd (Delta msg) )
--      )
--      -> (a -> a)
--      -> List c
--      -> d
--      -> e
--      -> f
--      -> g
--     )
--     -> d
--     -> e
--     -> f
--     -> g


initialiseRecordStates :
    (({ states : End -> states
      , deltas : List (Cmd delta)
      , recordInput : recordInput
      , fns : End
      , deltaSetters : End
      }
      -> ( State states, Cmd (Delta delta) )
     )
     ->
        { states : identity -> identity
        , deltas : List (Cmd delta)
        , recordInput : recordInput
        , fns : recordFns
        , deltaSetters : deltaSetters
        }
     -> ( State states, Cmd (Delta delta) )
    )
    -> recordInput
    -> recordFns
    -> deltaSetters
    -> ( State states, Cmd (Delta delta) )
initialiseRecordStates initialiser input fns deltaSetters =
    initialiser
        (\{ states, deltas } ->
            ( State { status = Intact_, selected = 1 } (states End)
            , Cmd.batch deltas |> Cmd.map StateChangedInternally
            )
        )
        { states = identity
        , deltas = []
        , recordInput = input
        , fns = fns
        , deltaSetters = deltaSetters
        }


recordStateInitialiser :
    ({ states : nextState -> states
     , deltas : List (Cmd msg)
     , recordInput : recordOutput
     , fns : fns
     , deltaSetters : deltaSetters
     }
     -> statesAndDeltas
    )
    ->
        { states : ( State state, nextState ) -> states
        , deltas : List (Cmd msg)
        , recordInput : recordOutput
        , fns : ( RecordFns context input state delta input recordOutput, fns )
        , deltaSetters : ( Delta delta -> msg, deltaSetters )
        }
    -> statesAndDeltas
recordStateInitialiser next { states, deltas, recordInput, fns, deltaSetters } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        ( deltaSetter, restDeltaSetters ) =
            deltaSetters

        (ControlFns controlFns) =
            recordFns.controlFns

        ( state, delta ) =
            recordInput
                |> recordFns.fromInput
                |> controlFns.initPrefilled
    in
    next
        { states = nestForwards Tuple.pair states state
        , deltas = Cmd.map deltaSetter delta :: deltas
        , recordInput = recordInput
        , fns = restFns
        , deltaSetters = restDeltaSetters
        }


initialiseRecordDeltas :
    (({ cmds : List (Cmd recordDelta)
      , deltaSetters : End
      , fieldDeltas : End
      }
      -> List (Cmd recordDelta)
     )
     ->
        { cmds : List (Cmd recordDelta)
        , deltaSetters : deltaSetters
        , fieldDeltas : fieldDeltas
        }
     -> List (Cmd recordDelta)
    )
    -> deltaSetters
    -> fieldDeltas
    -> Cmd (Delta recordDelta)
initialiseRecordDeltas deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_ (\{ cmds } -> cmds)
        { cmds = []
        , deltaSetters = deltaSetters
        , fieldDeltas = deltas
        }
        |> Cmd.batch
        |> Cmd.map StateChangedInternally


recordDeltaInitialiser :
    ({ cmds : List (Cmd recordDelta)
     , deltaSetters : restDeltaSetters
     , fieldDeltas : restDeltas
     }
     -> List (Cmd recordDelta)
    )
    ->
        { cmds : List (Cmd recordDelta)
        , deltaSetters : ( fieldDelta -> recordDelta, restDeltaSetters )
        , fieldDeltas : ( Cmd fieldDelta, restDeltas )
        }
    -> List (Cmd recordDelta)
recordDeltaInitialiser next { cmds, deltaSetters, fieldDeltas } =
    let
        ( deltaSetter, restDeltaSetters ) =
            deltaSetters

        ( delta, restDeltas ) =
            fieldDeltas
    in
    next
        { cmds = Cmd.map deltaSetter delta :: cmds
        , deltaSetters = restDeltaSetters
        , fieldDeltas = restDeltas
        }


validateRecordStates :
    (({ toOutputResult : Result (List Feedback) output2
      , context : context
      , fns : End
      , states : End
      }
      -> Result (List Feedback) output2
     )
     ->
        { toOutputResult : Result (List Feedback) output1
        , context : context
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , states : ( State state, restStates )
        }
     -> Result (List Feedback) output2
    )
    -> output1
    -> ( RecordFns context input state delta output recordOutput, restFns )
    -> context
    -> ( State state, restStates )
    -> Result (List Feedback) output2
validateRecordStates parser toOutput fns context states =
    parser (\{ toOutputResult } -> toOutputResult)
        { toOutputResult = Ok toOutput
        , fns = fns
        , context = context
        , states = states
        }


recordStateValidator :
    ({ toOutputResult : Result (List Feedback) output1
     , context : context
     , fns : restFns
     , states : restStates
     }
     -> Result (List Feedback) output2
    )
    ->
        { toOutputResult : Result (List Feedback) (output0 -> output1)
        , context : context
        , fns : ( RecordFns context input state delta output0 recordOutput, restFns )
        , states : ( State state, restStates )
        }
    -> Result (List Feedback) output2
recordStateValidator next { toOutputResult, fns, context, states } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns

        ( state, restStates ) =
            states
    in
    next
        { toOutputResult =
            case ( toOutputResult, controlFns.parse context state ) of
                ( Ok toOutput, Ok parsed ) ->
                    Ok (toOutput parsed)

                ( Ok _, Err es ) ->
                    Err es

                ( Err es, Ok _ ) ->
                    Err es

                ( Err es, Err es2 ) ->
                    Err (es ++ es2)
        , fns = restFns
        , context = context
        , states = restStates
        }


setAllRecordStatesToIdle :
    (({ inputStates : End
      , outputStates : End -> outputStates
      , fns : End
      }
      -> outputStates
     )
     ->
        { inputStates : ( State inputState, restInputStates )
        , outputStates : identity -> identity
        , fns : ( RecordFns context input inputState delta output recordOutput, restFns )
        }
     -> outputStates
    )
    -> ( RecordFns context input inputState delta output recordOutput, restFns )
    -> ( State inputState, restInputStates )
    -> outputStates
setAllRecordStatesToIdle idleSetter_ fns states =
    idleSetter_ (\{ outputStates } -> outputStates End)
        { outputStates = identity
        , fns = fns
        , inputStates = states
        }


recordStateIdleSetter :
    ({ inputStates : restInputStates
     , outputStates : restInputStates -> outputStates
     , fns : restFns
     }
     -> outputStates
    )
    ->
        { inputStates : ( State inputState, restInputStates )
        , outputStates : ( State inputState, restInputStates ) -> outputStates
        , fns : ( RecordFns context input inputState delta output recordOutput, restFns )
        }
    -> outputStates
recordStateIdleSetter next { inputStates, outputStates, fns } =
    let
        ( inputState, restInputStates ) =
            inputStates

        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns
    in
    next
        { outputStates = nestForwards Tuple.pair outputStates (controlFns.setAllIdle inputState)
        , fns = restFns
        , inputStates = restInputStates
        }


viewRecordStates :
    (({ views : List (Subcontrol recordDelta)
      , alerts : List Alert
      , context : context
      , fns : End
      , deltaSetters : End
      , states : End
      }
      -> List (Subcontrol recordDelta)
     )
     ->
        { views : List (Subcontrol recordDelta)
        , alerts : List Alert
        , context : context
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , deltaSetters : ( Delta delta -> recordDelta, restDeltaSetters )
        , states : ( State state, restStates )
        }
     -> List (Subcontrol recordDelta)
    )
    -> context
    -> ( RecordFns context input state delta output recordOutput, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> { j | alerts : List Alert, state : ( State state, restStates ) }
    -> List (Subcontrol recordDelta)
viewRecordStates viewer context fns setters config =
    viewer (\{ views } -> views)
        { views = []
        , alerts = config.alerts
        , fns = fns
        , deltaSetters = setters
        , context = context
        , states = config.state
        }


recordStateViewer :
    ({ views : List (Subcontrol recordDelta)
     , alerts : List Alert
     , fns : restFns
     , deltaSetters : restDeltaSetters
     , context : context
     , states : restStates
     }
     -> List (Subcontrol recordDelta)
    )
    ->
        { views : List (Subcontrol recordDelta)
        , alerts : List Alert
        , deltaSetters : ( Delta delta -> recordDelta, restDeltaSetters )
        , fns : ( RecordFns context input state delta output recordOutput, restFns )
        , context : context
        , states : ( State state, restStates )
        }
    -> List (Subcontrol recordDelta)
recordStateViewer next { views, alerts, deltaSetters, fns, context, states } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        (ControlFns controlFns) =
            recordFns.controlFns

        ( setter, restDeltaSetters ) =
            deltaSetters

        ( State internalState state, restStates ) =
            states

        view =
            controlFns.view context
                { id = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.id
                , name = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.name
                , label = controlFns.label
                , class = controlFns.class
                , state = state
                , status = getStatus controlFns.parse controlFns.collectFeedback alerts context (State internalState state)
                , alerts = alerts
                , selected = internalState.selected
                }
                |> List.map (H.map (\delta -> StateChangedByInput (setter delta)))
    in
    next
        { views =
            views
                ++ [ { html = view
                     , label = controlFns.label
                     , index = controlFns.index
                     }
                   ]
        , alerts = alerts
        , fns = restFns
        , deltaSetters = restDeltaSetters
        , context = context
        , states = restStates
        }


getStatus :
    (context -> State state -> Result (List Feedback) output)
    -> (State state -> List Alert -> List Feedback)
    -> List Alert
    -> context
    -> State state
    -> Status
getStatus parse collectErrors alerts ctx ((State internalState _) as state) =
    case internalState.status of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                parsedErrors =
                    case parse ctx state of
                        Ok _ ->
                            []

                        Err errs ->
                            errs

                flaggedErrors =
                    collectErrors state alerts
            in
            Idle (parsedErrors ++ flaggedErrors)



-- updateRecordStates :
--     (({ newStates : End -> states, newCmds : List (Cmd msg) }
--       -> End
--       -> End
--       -> context
--       -> End
--       -> End
--       -> { newStates : End -> states, newCmds : List (Cmd msg) }
--      )
--      -> { newStates : states -> states, newCmds : List (Cmd msg) }
--      -> ( RecordFns context input state delta output recordOutput, restFns )
--      -> ( setter, restDeltaSetters )
--      -> context
--      -> ( Delta delta, restDeltas )
--      -> ( State state, restStates )
--      -> { newStates : End -> states, newCmds : List (Cmd msg) }
--     )
--     -> ( RecordFns context input state delta output recordOutput, restFns )
--     -> ( setter, restDeltaSetters )
--     -> context
--     -> ( Delta delta, restDeltas )
--     -> ( State state, restStates )
--     -> ( states, Cmd msg )


updateRecordStates :
    (({ newStates : End -> finalRecordStates
      , newCmds : List (Cmd recordDelta)
      , context : context
      , fns : End
      , deltaSetters : End
      , deltas : End
      , states : End
      }
      ->
        { newStates : End -> finalRecordStates
        , newCmds : List (Cmd recordDelta)
        }
     )
     ->
        { newStates : d -> d
        , newCmds : List e
        , context : context
        , fns : ( RecordFns context input state fieldDelta output recordOutput, restFns )
        , deltaSetters : ( Delta fieldDelta -> recordDelta, restDeltaSetters )
        , deltas : ( Delta fieldDelta, restDeltas )
        , states : ( State state, restStates )
        }
     ->
        { newStates : End -> finalRecordStates
        , newCmds : List (Cmd recordDelta)
        }
    )
    -> context
    -> ( RecordFns context input state fieldDelta output recordOutput, restFns )
    -> ( Delta fieldDelta -> recordDelta, restDeltaSetters )
    -> ( Delta fieldDelta, restDeltas )
    -> ( State state, restStates )
    -> ( finalRecordStates, Cmd recordDelta )
updateRecordStates updater context fields setters deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output ->
                    { newStates = output.newStates
                    , newCmds = output.newCmds
                    }
                )
                { newStates = identity
                , newCmds = []
                , context = context
                , fns = fields
                , deltaSetters = setters
                , deltas = deltas
                , states = states
                }
    in
    ( newStates End
    , Cmd.batch newCmds
    )


recordStateUpdater :
    ({ newStates : restStates -> intermediateRecordStates
     , newCmds : List (Cmd recordDelta)
     , context : context
     , fns : restFns
     , deltaSetters : restDeltaSetters
     , deltas : restDeltas
     , states : restStates
     }
     ->
        { newStates : finalRecordStates
        , newCmds : List (Cmd recordDelta)
        }
    )
    ->
        { newStates : ( State state, restStates ) -> intermediateRecordStates
        , newCmds : List (Cmd recordDelta)
        , context : context
        , fns : ( RecordFns context input state fieldDelta output recordOutput, restFns )
        , deltaSetters : ( Delta fieldDelta -> recordDelta, restDeltaSetters )
        , deltas : ( Delta fieldDelta, restDeltas )
        , states : ( State state, restStates )
        }
    ->
        { newStates : finalRecordStates
        , newCmds : List (Cmd recordDelta)
        }
recordStateUpdater next { newStates, newCmds, context, fns, deltaSetters, deltas, states } =
    let
        ( RecordFns recordFns, restFns ) =
            fns

        ( deltaSetter, restDeltaSetters ) =
            deltaSetters

        ( delta, restDeltas ) =
            deltas

        ( state, restStates ) =
            states

        (ControlFns controlFns) =
            recordFns.controlFns

        ( newState, newCmd ) =
            controlFns.update context delta state

        cmd2 =
            newCmd
                |> Cmd.map deltaSetter
    in
    next
        { newStates = nestForwards Tuple.pair newStates newState
        , newCmds = cmd2 :: newCmds
        , context = context
        , fns = restFns
        , deltaSetters = restDeltaSetters
        , deltas = restDeltas
        , states = restStates
        }



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
                nestForwards Tuple.pair
                    (builder.fns path)
                    (ControlFns { controlFns | index = newIndex })
        , initialStates =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                nestForwards Tuple.pair
                    (builder.initialStates path)
                    (controlFns.initBlank |> Tuple.first)
        , initialDeltas =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                nestForwards Tuple.pair
                    (builder.initialDeltas path)
                    (controlFns.initBlank |> Tuple.second)
        , updater = builder.updater >> customTypeStateUpdater
        , viewer = builder.viewer >> selectedTagViewer
        , parser = builder.parser >> selectedTagParser
        , idleSetter = builder.idleSetter >> selectedTagIdleSetter
        , deltaBefore = nestForwards Tuple.pair builder.deltaBefore NoDelta
        , deltaBefores = nestForwards Tuple.pair builder.deltaBefores builder.deltaBefore
        , deltaAfter = nestBackwards Tuple.pair builder.deltaAfter NoDelta
        , deltaAfters = nestBackwards Tuple.pair builder.deltaAfters builder.deltaAfter
        , makeDeltaSetters = builder.makeDeltaSetters >> deltaSetterMaker
        , initialiseDeltas = builder.initialiseDeltas >> customTypeDeltaInitialiser
        , stateBefore = nestForwards Tuple.pair builder.stateBefore Nothing
        , stateBefores = nestForwards Tuple.pair builder.stateBefores builder.stateBefore
        , toArgStates = nestForwards Tuple.pair builder.toArgStates toArgState
        , stateAfter = nestBackwards Tuple.pair builder.stateAfter Nothing
        , stateAfters = nestBackwards Tuple.pair builder.stateAfters builder.stateAfter
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
                    \ctx delta (State internalState state) ->
                        case delta of
                            NoDelta ->
                                ( State internalState state, Cmd.none )

                            TagSelected idx ->
                                ( State { internalState | selected = idx } state, Cmd.none )

                            StateChangedByInput tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates builder.updater fns deltaSetters tagDelta ctx state
                                in
                                ( State internalState newTagStates
                                , Cmd.map StateChangedByInput cmd
                                )

                            StateChangedInternally tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates builder.updater fns deltaSetters tagDelta ctx state
                                in
                                ( State internalState newTagStates
                                , Cmd.map StateChangedInternally cmd
                                )

                            _ ->
                                ( State internalState state, Cmd.none )

                subcontrolView ctx config =
                    viewSelectedTagState builder.viewer fns deltaSetters ctx config

                view ctx config =
                    customTypeView ctx config subcontrolView

                parse ctx (State internalState state) =
                    validateSelectedTagState builder.parser internalState.selected fns ctx state

                setAllIdle =
                    \(State internalState state) ->
                        State
                            { internalState | status = Idle_ }
                            (setSelectedTagStateIdle builder.idleSetter internalState.selected fns state)

                emitAlerts ctx (State internalState state) =
                    emitAlertsForCustomType builder.alertEmitter internalState.selected fns ctx state
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
                , collectFeedback = \(State _ states) alerts -> collectErrorsForCustomType builder.errorCollector alerts fns states
                , receiverCount = 0
                , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForCustomType builder.debouncingReceiverCollector fns states
                , label = "Custom Type"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \ctx (State _ states) -> collectCustomTypeSubscriptions builder.subscriptionCollector deltaSetters fns ctx states
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
tag0 label_ tag =
    tagHelper
        label_
        (null tag)
        (\insertArgStateIntoTagStates ->
            insertArgStateIntoTagStates tag
        )


null : tag -> Control context () () tag
null tag =
    define
        { blank = ( (), Cmd.none )
        , prefill = \_ -> ( (), Cmd.none )
        , update = \_ () () -> ( (), Cmd.none )
        , view = \_ _ -> []
        , parse = \_ () -> Ok tag
        , subscriptions = \_ () -> Sub.none
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
    ((List (Sub delta)
      -> End
      -> End
      -> context
      -> End
      -> List (Sub delta)
     )
     -> List (Sub delta)
     -> setters
     -> fns
     -> context
     -> states
     -> List (Sub delta)
    )
    -> setters
    -> fns
    -> context
    -> states
    -> Sub (Delta delta)
collectCustomTypeSubscriptions collector setters fns ctx states =
    collector (\listSubs End End _ End -> listSubs) [] setters fns ctx states
        |> Sub.batch
        |> Sub.map StateChangedInternally


customTypeSubscriptionCollector :
    (List (Sub delta1) -> restDeltaSetters -> restFns -> context -> restStates -> List (Sub delta1))
    -> List (Sub delta1)
    -> ( Delta delta -> delta1, restDeltaSetters )
    -> ( ControlFns context input state delta output, restFns )
    -> context
    -> ( State state, restStates )
    -> List (Sub delta1)
customTypeSubscriptionCollector next listSubs ( setter, restDeltaSetters ) ( ControlFns fns, restFns ) ctx ( state, restStates ) =
    next ((fns.subscriptions ctx state |> Sub.map setter) :: listSubs) restDeltaSetters restFns ctx restStates


collectDebouncingReceiversForCustomType :
    ((List Alert
      -> End
      -> End
      -> List Alert
     )
     -> List Alert
     -> ( ControlFns context input state delta output, restFns )
     -> ( State state, restStates )
     -> List Alert
    )
    -> ( ControlFns context input state delta output, restFns )
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
    -> ( ControlFns context input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
customTypeDebouncingReceiverCollector next receivers ( ControlFns fns, restFns ) ( state, restStates ) =
    next (receivers ++ fns.collectDebouncingReceivers state) restFns restStates


updateCustomTypeStates :
    (({ newStates : End -> states, newCmds : List (Cmd msg) }
      -> End
      -> End
      -> End
      -> context
      -> End
      -> { newStates : End -> states, newCmds : List (Cmd msg) }
     )
     -> { newStates : states -> states, newCmds : List (Cmd msg) }
     -> ( ControlFns context input state delta output, restFns )
     -> ( setter, restDeltaSetters )
     -> ( Delta delta, restDeltas )
     -> context
     -> ( State state, restStates )
     -> { newStates : End -> states, newCmds : List (Cmd msg) }
    )
    -> ( ControlFns context input state delta output, restFns )
    -> ( setter, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> context
    -> ( State state, restStates )
    -> ( states, Cmd msg )
updateCustomTypeStates updater fns setters deltas ctx states =
    let
        { newStates, newCmds } =
            updater
                (\output End End End _ End -> output)
                { newStates = identity, newCmds = [] }
                fns
                setters
                deltas
                ctx
                states
    in
    ( newStates End, Cmd.batch newCmds )


customTypeStateUpdater :
    ({ newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) }
     -> restFns
     -> restDeltaSetters
     -> restDeltas
     -> context
     -> restStates
     -> { newStates : recordState, newCmds : List (Cmd recordDelta) }
    )
    -> { newStates : ( State state, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) }
    -> ( ControlFns context input state delta output, restFns )
    -> ( Delta delta -> recordDelta, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> context
    -> ( State state, restStates )
    -> { newStates : recordState, newCmds : List (Cmd recordDelta) }
customTypeStateUpdater next { newStates, newCmds } ( ControlFns fns, restFns ) ( deltaSetter, restDeltaSetters ) ( delta, restDeltas ) ctx ( state, restStates ) =
    let
        ( newState, newCmd ) =
            fns.update ctx delta state
    in
    next
        { newStates = nestForwards Tuple.pair newStates newState
        , newCmds = Cmd.map deltaSetter newCmd :: newCmds
        }
        restFns
        restDeltaSetters
        restDeltas
        ctx
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
        |> List.map (Cmd.map StateChangedInternally)


customTypeDeltaInitialiser :
    (List (Cmd delta1) -> restDeltaSetters -> restDeltas -> List (Cmd delta1))
    -> List (Cmd delta1)
    -> ( delta -> delta1, restDeltaSetters )
    -> ( Cmd delta, restDeltas )
    -> List (Cmd delta1)
customTypeDeltaInitialiser next cmdList ( setter, restDeltaSetters ) ( delta, restDeltas ) =
    next (Cmd.map setter delta :: cmdList) restDeltaSetters restDeltas


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
        { controlFns : ( ControlFns context input state1 delta output, restControlFns )
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
                    , Cmd.map (deltaSetter >> StateChangedInternally) delta
                    )
                )
    in
    next
        { finalTagStates = nestForwards Tuple.pair finalTagStates finalTagState
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
    next (thisTagIndex + 1) selectedTag (nestForwards Tuple.pair tagStates tagArgState) restMaybeOverrides restInitialTagStates


collectErrorsForCustomType :
    ((List Alert
      -> List Feedback
      -> End
      -> End
      -> List Feedback
     )
     -> List Alert
     -> List Feedback
     -> ( ControlFns context input state delta output, restFns )
     -> ( State state, restStates )
     -> List Feedback
    )
    -> List Alert
    -> ( ControlFns context input state delta output, restFns )
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
    -> ( ControlFns context input state delta output, restFns )
    -> ( State state, restStates )
    -> List Feedback
customTypeErrorCollector next alerts errors ( ControlFns fns, restFns ) ( state, restStates ) =
    next alerts (errors ++ fns.collectFeedback state alerts) restFns restStates


emitAlertsForCustomType :
    ((List Alert
      -> Int
      -> End
      -> context
      -> End
      -> List Alert
     )
     -> List Alert
     -> Int
     -> ( ControlFns context input state delta output, restFns )
     -> context
     -> ( State state, restStates )
     -> List Alert
    )
    -> Int
    -> ( ControlFns context input state delta output, restFns )
    -> context
    -> ( State state, restStates )
    -> List Alert
emitAlertsForCustomType alertEmitter_ selectedTag fns ctx tagStates =
    alertEmitter_ (\alerts _ End _ End -> alerts) [] selectedTag fns ctx tagStates


customTypeAlertEmitter :
    (List Alert
     -> Int
     -> restFns
     -> context
     -> restStates
     -> List Alert
    )
    -> List Alert
    -> Int
    -> ( ControlFns context input state delta output, restFns )
    -> context
    -> ( State state, restStates )
    -> List Alert
customTypeAlertEmitter next alerts selectedTag ( ControlFns fns, restFns ) ctx ( tagState, restTagStates ) =
    let
        newAlerts =
            if fns.index == selectedTag then
                fns.emitAlerts ctx tagState

            else
                []
    in
    next (alerts ++ newAlerts) selectedTag restFns ctx restTagStates


setSelectedTagStateIdle :
    ((Int
      -> End
      -> End
      -> End
     )
     -> Int
     -> ( ControlFns context input state delta output, restFns )
     -> ( State state, restStates )
     -> ( State state, restStates )
    )
    -> Int
    -> ( ControlFns context input state delta output, restFns )
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
    -> ( ControlFns context input state delta output, restFns )
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
      -> context
      -> End
      -> a
     )
     -> Result (List Feedback) value
     -> Int
     -> ( ControlFns context x y z w, restFns )
     -> context
     -> states
     -> e
    )
    -> Int
    -> ( ControlFns context x y z w, restFns )
    -> context
    -> states
    -> e
validateSelectedTagState parser selectedTag fns ctx states =
    parser
        (\result_ _ End _ End -> result_)
        (Err [ { path = Path.root, label = "FATAL ERROR", message = "tag index " ++ String.fromInt selectedTag ++ " not found", fail = True } ])
        selectedTag
        fns
        ctx
        states


selectedTagParser :
    (Result (List Feedback) output
     -> Int
     -> restFns
     -> context
     -> restStates
     -> Result (List Feedback) output
    )
    -> Result (List Feedback) output
    -> Int
    -> ( ControlFns context input state delta output, restFns )
    -> context
    -> ( State state, restStates )
    -> Result (List Feedback) output
selectedTagParser next result_ selectedTag ( ControlFns fns, restFns ) ctx ( state, restStates ) =
    next
        (if fns.index == selectedTag then
            fns.parse ctx state

         else
            result_
        )
        selectedTag
        restFns
        ctx
        restStates


viewSelectedTagState :
    ((List (Subcontrol delta)
      -> b
      -> End
      -> End
      -> context
      -> End
      -> List (Subcontrol delta)
     )
     -> List (Subcontrol delta)
     -> List Alert
     -> fns
     -> setters
     -> context
     -> state
     -> List (Subcontrol delta)
    )
    -> fns
    -> setters
    -> context
    -> InternalViewConfig state
    -> List (Subcontrol delta)
viewSelectedTagState viewer fns setters ctx config =
    viewer (\listSubcontrol _ End End _ End -> listSubcontrol) [] config.alerts fns setters ctx config.state


selectedTagViewer :
    (List (Subcontrol delta)
     -> List Alert
     -> restFns
     -> restDeltaSetters
     -> context
     -> restStates
     -> List (Subcontrol delta)
    )
    -> List (Subcontrol delta)
    -> List Alert
    -> ( ControlFns context input state e output, restFns )
    -> ( Delta e -> delta, restDeltaSetters )
    -> context
    -> ( State state, restStates )
    -> List (Subcontrol delta)
selectedTagViewer next listSubcontrol alerts ( ControlFns fns, restFns ) ( setter, restDeltaSetters ) ctx ( State internalState state, restStates ) =
    next
        (listSubcontrol
            ++ [ { html =
                    fns.view ctx
                        { id = Maybe.withDefault ("control-" ++ Path.toString fns.path) fns.id
                        , name = Maybe.withDefault ("control-" ++ Path.toString fns.path) fns.name
                        , label = fns.label
                        , class = fns.class
                        , state = state
                        , status = Intact
                        , alerts = alerts
                        , selected = internalState.selected
                        }
                        |> List.map (H.map (setter >> StateChangedByInput))
                 , label = fns.label
                 , index = fns.index
                 }
               ]
        )
        alerts
        restFns
        restDeltaSetters
        ctx
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
    context
    -> InternalViewConfig state
    -> (context -> InternalViewConfig state -> List (Subcontrol delta))
    -> List (Html (Delta delta))
customTypeView ctx config toSubcontrols =
    let
        subcontrols =
            toSubcontrols ctx config

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
    -> context
    -> InternalViewConfig (List (State state))
    -> List Alert
    -> (Path -> ControlFns context input state delta output)
    -> List (Html (Delta (ListDelta delta)))
listView path ctx config debouncingReceivers subcontrol =
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
                    button (StateChangedByInput (ItemInserted 0)) "Add item"

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
                                        (itemFns.view ctx
                                            { id = Maybe.withDefault ("control-" ++ Path.toString itemPath) itemFns.id
                                            , name = Maybe.withDefault ("control-" ++ Path.toString itemPath) itemFns.name
                                            , label = itemFns.label
                                            , class = itemFns.class
                                            , state = state
                                            , status = getStatus itemFns.parse itemFns.collectFeedback filteredAlerts2 ctx (State internalState state)
                                            , alerts = filteredAlerts2
                                            , selected = internalState.selected
                                            }
                                        )
                                        |> H.map (ItemUpdated idx)
                                    , button (ItemDeleted idx) "Delete item"
                                    , H.div [] [ button (ItemInserted (idx + 1)) "Insert item" ]
                                    ]
                            )
                            config.state
                        )
                        |> H.map StateChangedByInput
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
