module Control exposing
    ( Control, Form, sandbox, simpleForm, form
    , bool, int, float, string, char, enum
    , tuple, triple, maybe, result, list, dict, set, array, map
    , RecordBuilder, record, field, endRecord, LayoutConfig, Subcontrol, layout
    , CustomTypeBuilder, customType, tag0, tag1, tag2, tag3, tag4, tag5, endCustomType
    , Definition, define
    , failIf, noteIf
    , alertIf, respond
    , alertAtIndexes
    , default, debounce, id, name, label, class, classList, wrapView
    , FormWithContext, simpleFormWithContext, formWithContext, DefinitionWithContext, defineWithContext, failIfWithContext, noteIfWithContext, alertIfWithContext, alertAtIndexesWithContext
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


# Building record combinators

@docs RecordBuilder, record, field, endRecord, LayoutConfig, Subcontrol, layout


# Building custom type combinators

@docs CustomTypeBuilder, customType, tag0, tag1, tag2, tag3, tag4, tag5, endCustomType


# Creating a new control

@docs Definition, define


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


# Forms with context

For some advanced use cases, you may want to create a form that knows about more
than its own internal state - for example, it may need to know about some state
that lives elsewhere in your app's `Model`.

This is where the `...WithContext` family of types and functions come into play

  - they allow you to inject some external state into your form controls by
    passing a `context` value as an additional argument to the form's `update`,
    `view`, `subscriptions` and `submit` functions, and also to any validation
    functions applied to the form's `Control`s.

@docs FormWithContext, simpleFormWithContext, formWithContext, DefinitionWithContext, defineWithContext, failIfWithContext, noteIfWithContext, alertIfWithContext, alertAtIndexesWithContext


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


{-| A `FormWithContext` is a record which contains functions that you can use in your
Elm `Program` to initialise, view, update, subscribe, and submit your form.

Unlike the standard `Form`, several of the functions provided by `FormWithContext`
take an extra `context` parameter, which allows you to feed external data (e.g. from
your app's `Model`) into your form and allow the form to respond accordingly.

For example, imagine your app's `Model` contains a list of users, and you build
a form to create a new user. The new user record should only be valid if its
nickname hasn't already been chosen by another user:

    import Set

    type alias User =
        String

    type alias Model =
        { formState : State String
        , existingUsers : Set String
        }

    type Msg
        = FormUpdated (Delta String)
        | FormSubmitted

    userControl =
        string
            |> failIfWithContext
                (\existingUsers newUser ->
                    Set.member newUser existingUsers
                )

    form =
        simpleFormWithContext
            { control = userControl
            , onUpdate = FormUpdated
            , onSubmit = FormSubmitted
            }

    update msg model =
        case msg of
            FormUpdated delta ->
                let
                    (formState, cmd) =
                        form.update model.existingUsers delta model.formState
                in
                ( { model | formState = formState }, cmd )

            FormSubmitted ->

-}
type alias FormWithContext context state delta output msg =
    { blank : ( State state, Cmd msg )
    , prefill : output -> ( State state, Cmd msg )
    , update : context -> Delta delta -> State state -> ( State state, Cmd msg )
    , view : context -> State state -> Html msg
    , subscriptions : context -> State state -> Sub msg
    , submit : context -> State state -> ( State state, Result (List Feedback) output )
    }


{-| A `Form` is a record which contains functions that you can use in your
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


{-| Definition for a custom control. You need to supply the following fields:

  - `blank`: an initial "empty" value for the `state` of the control, plus an initial (optional) `Cmd` to run. For a control whose `state` is of type `String`, the state value might be `""`, for a number it might be `0`.
  - `prefill`: a function that uses a value of the control's `output` type to initialise the control's `state` and (optionally) run an initial `Cmd`. For a control that outputs an `Int`, but whose `state` is a `String`, you might use `String.fromInt` to convert the `output` value into a `state` value.
  - `update`: a function that updates the `state` of the control based on its existing `state` and a `delta` type that it receives. This is exactly analogous to an Elm program's update function, where `state` = `Model` and `delta` = `Msg`.
  - `view`: a function that outputs `Html delta` based on the `state` of the control. This is similar to an Elm program's view function, except instead of just taking the `state` as an argument, it takes a record containing the `state` plus the id, name, class, and label for the input.
  - `subscriptions`: a function that outputs `Sub delta` based on the `state` of the control. This is exactly analogous to an Elm program's subscriptions function.
  - `parse`: a function that attempts to parse the control's state into its output type, producing a result. If parsing fails, it can provide a list of errors.
  - `label`: a default label for the control.

-}
type alias Definition state delta output =
    { blank : ( state, Cmd delta )
    , prefill : output -> ( state, Cmd delta )
    , update : delta -> state -> ( state, Cmd delta )
    , view : { label : String, id : String, name : String, class : String, state : state } -> List (Html delta)
    , subscriptions : state -> Sub delta
    , parse : state -> Result (List String) output
    , label : String
    }


{-| -}
type alias DefinitionWithContext context state delta output =
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
   d888888b d8b   db d888888b d88888b d8888b. d8b   db  .d8b.  db      .d8888.
     `88'   888o  88 `~~88~~' 88'     88  `8D 888o  88 d8' `8b 88      88'  YP
      88    88V8o 88    88    88ooooo 88oobY' 88V8o 88 88ooo88 88      `8bo.
      88    88 V8o88    88    88~~~~~ 88`8b   88 V8o88 88~~~88 88        `Y8b.
     .88.   88  V888    88    88.     88 `88. 88  V888 88   88 88booo. db   8D
   Y888888P VP   V8P    YP    Y88888P 88   YD VP   V8P YP   YP Y88888P `8888Y'
-}


type Internals a
    = State_ (State a)
    | Delta_ (Delta a)


maybeExtractState : Internals state -> Maybe (State state)
maybeExtractState internals =
    case internals of
        State_ state ->
            Just state

        Delta_ _ ->
            Nothing


maybeExtractDelta : Internals delta -> Maybe (Delta delta)
maybeExtractDelta internals =
    case internals of
        State_ _ ->
            Nothing

        Delta_ delta ->
            Just delta



{-
   db   d8b   db d8888b.  .d8b.  d8888b. d8888b. d88888b d8888b. .d8888.
   88   I8I   88 88  `8D d8' `8b 88  `8D 88  `8D 88'     88  `8D 88'  YP
   88   I8I   88 88oobY' 88ooo88 88oodD' 88oodD' 88ooooo 88oobY' `8bo.
   Y8   I8I   88 88`8b   88~~~88 88~~~   88~~~   88~~~~~ 88`8b     `Y8b.
   `8b d8'8b d8' 88 `88. 88   88 88      88      88.     88 `88. db   8D
    `8b8' `8d8'  88   YD YP   YP 88      88      Y88888P 88   YD `8888Y'
-}


genericWrap : (a -> b) -> c -> ((End -> c) -> d -> a) -> d -> b
genericWrap tag end wrapper_ tup =
    tag (wrapper_ (\End -> end) tup)


genericWrapper : (a -> b -> c) -> (d -> a) -> (e -> b) -> ( d, e ) -> c
genericWrapper tag internalsType next ( this, rest ) =
    tag (internalsType this) (next rest)


genericUnwrap : (d -> a) -> (b -> c) -> ((b -> Maybe c) -> a -> e) -> d -> e
genericUnwrap untag unend unwrapper_ wrappedThing =
    unwrapper_ (unend >> Just) (untag wrappedThing)


genericUnwrapper : (d -> ( a, b )) -> (a -> Maybe c) -> (b -> Maybe e) -> d -> Maybe ( c, e )
genericUnwrapper untag maybeExtractor next wrappedThing =
    let
        ( this, rest ) =
            untag wrappedThing
    in
    Maybe.map2 Tuple.pair (maybeExtractor this) (next rest)



-- recordWrapper :
--     { wrapState : ((End -> EndRecord) -> ( State state, restStateTuples ) -> Field state restStateFields) -> ( State state, restStateTuples ) -> Record (Field state restStateFields)
--     , stateWrapper : (restStateTuples -> restStateFields) -> ( State state, restStateTuples ) -> Field state restStateFields
--     , unwrapState : ((EndRecord -> Maybe End) -> Field state restStateFields -> Maybe ( State state, restStateTuples )) -> Record (Field state restStateFields) -> Maybe ( State state, restStateTuples )
--     , stateUnwrapper : (restStateFields -> Maybe restStateTuples) -> Field state restStateFields -> Maybe ( State state, restStateTuples )
--     , wrapDelta : ((End -> EndRecord) -> ( Delta delta, restDeltaTuples ) -> Field delta restDeltaFields) -> ( Delta delta, restDeltaTuples ) -> Record (Field delta restDeltaFields)
--     , deltaWrapper : (restDeltaTuples -> restDeltaFields) -> ( Delta delta, restDeltaTuples ) -> Field delta restDeltaFields
--     , unwrapDelta : ((EndRecord -> Maybe End) -> Field delta restDeltaFields -> Maybe ( Delta delta, restDeltaTuples )) -> Record (Field delta restDeltaFields) -> Maybe ( Delta delta, restDeltaTuples )
--     , deltaUnwrapper : (restDeltaFields -> Maybe restDeltaTuples) -> Field delta restDeltaFields -> Maybe ( Delta delta, restDeltaTuples )
--     }


recordWrapper =
    { wrapState = genericWrap Record EndRecord
    , stateWrapper = genericWrapper Field State_
    , unwrapState = genericUnwrap (\(Record fields) -> fields) (\EndRecord -> End)
    , stateUnwrapper = genericUnwrapper (\(Field this rest) -> ( this, rest )) maybeExtractState
    , wrapDelta = genericWrap Record EndRecord
    , deltaWrapper = genericWrapper Field Delta_
    , unwrapDelta = genericUnwrap (\(Record fields) -> fields) (\EndRecord -> End)
    , deltaUnwrapper = genericUnwrapper (\(Field this rest) -> ( this, rest )) maybeExtractDelta
    }



-- customTypeWrapper :
--     { wrapState : ((End -> EndCustomType) -> ( State state, restStateTuples ) -> Tag state restStateTags) -> ( State state, restStateTuples ) -> CustomType (Tag state restStateTags)
--     , stateWrapper : (restStateTuples -> restStateTags) -> ( State state, restStateTuples ) -> Tag state restStateTags
--     , unwrapState : ((EndCustomType -> Maybe End) -> Tag state restStateTags -> Maybe ( State state, restStateTuples )) -> CustomType (Tag state restStateTags) -> Maybe ( State state, restStateTuples )
--     , stateUnwrapper : (restStateTags -> Maybe restStateTuples) -> Tag state restStateTags -> Maybe ( State state, restStateTuples )
--     , wrapDelta : ((End -> EndCustomType) -> ( Delta delta, restDeltaTuples ) -> Tag delta restDeltaTags) -> ( Delta delta, restDeltaTuples ) -> CustomType (Tag delta restDeltaTags)
--     , deltaWrapper : (restDeltaTuples -> restDeltaTags) -> ( Delta delta, restDeltaTuples ) -> Tag delta restDeltaTags
--     , unwrapDelta : ((EndCustomType -> Maybe End) -> Tag delta restDeltaTags -> Maybe ( Delta delta, restDeltaTuples )) -> CustomType (Tag delta restDeltaTags) -> Maybe ( Delta delta, restDeltaTuples )
--     , deltaUnwrapper : (restDeltaTags -> Maybe restDeltaTuples) -> Tag delta restDeltaTags -> Maybe ( Delta delta, restDeltaTuples )
--     }


customTypeWrapper =
    { wrapState = genericWrap CustomType EndCustomType
    , stateWrapper = genericWrapper Tag State_
    , unwrapState = genericUnwrap (\(CustomType tags) -> tags) (\EndCustomType -> End)
    , stateUnwrapper = genericUnwrapper (\(Tag tag restTags) -> ( tag, restTags )) maybeExtractState
    , wrapDelta = genericWrap CustomType EndCustomType
    , deltaWrapper = genericWrapper Tag Delta_
    , unwrapDelta = genericUnwrap (\(CustomType tags) -> tags) (\EndCustomType -> End)
    , deltaUnwrapper = genericUnwrapper (\(Tag tag restTags) -> ( tag, restTags )) maybeExtractDelta
    }



-- argWrapper :
--     { wrapState : ((End -> EndTag) -> ( State state, restStateTuples ) -> Arg state restStateArgs) -> ( State state, restStateTuples ) -> Arg state restStateArgs
--     , stateWrapper : (restStateTuples -> restStateArgs) -> ( State state, restStateTuples ) -> Arg state restStateArgs
--     , unwrapState : ((EndTag -> Maybe End) -> Arg state restStateArgs -> Maybe ( State state, restStateTuples )) -> Arg state restStateArgs -> Maybe ( State state, restStateTuples )
--     , stateUnwrapper : (restStateArgs -> Maybe restStateTuples) -> Arg state restStateArgs -> Maybe ( State state, restStateTuples )
--     , wrapDelta : ((End -> EndTag) -> ( Delta delta, restDeltaTuples ) -> Arg delta restDeltaArgs) -> ( Delta delta, restDeltaTuples ) -> Arg delta restDeltaArgs
--     , deltaWrapper : (restDeltaTuples -> restDeltaArgs) -> ( Delta delta, restDeltaTuples ) -> Arg delta restDeltaArgs
--     , unwrapDelta : ((EndTag -> Maybe End) -> Arg delta restDeltaArgs -> Maybe ( Delta delta, restDeltaTuples )) -> Arg delta restDeltaArgs -> Maybe ( Delta delta, restDeltaTuples )
--     , deltaUnwrapper : (restDeltaArgs -> Maybe restDeltaTuples) -> Arg delta restDeltaArgs -> Maybe ( Delta delta, restDeltaTuples )
--     }


argWrapper =
    { wrapState = genericWrap identity EndTag
    , stateWrapper = genericWrapper Arg State_
    , unwrapState = genericUnwrap identity (\EndTag -> End)
    , stateUnwrapper = genericUnwrapper (\(Arg arg restArgs) -> ( arg, restArgs )) maybeExtractState
    , wrapDelta = genericWrap identity EndTag
    , deltaWrapper = genericWrapper Arg Delta_
    , unwrapDelta = genericUnwrap identity (\EndTag -> End)
    , deltaUnwrapper = genericUnwrapper (\(Arg arg restArgs) -> ( arg, restArgs )) maybeExtractDelta
    }



-- tupleWrapper :
--     { wrapState : a -> ( State b, ( State c, End ) ) -> Tuple b c
--     , stateWrapper : d -> e -> f -> f
--     , unwrapState : g -> Tuple fst snd -> Maybe ( State fst, ( State snd, End ) )
--     , stateUnwrapper : h -> i -> j -> j
--     , wrapDelta : k -> ( Delta l, ( Delta m, End ) ) -> Tuple l m
--     , deltaWrapper : n -> o -> p -> p
--     , unwrapDelta : q -> Tuple r s -> Maybe ( Delta r, ( Delta s, End ) )
--     , deltaUnwrapper : t -> u -> v -> v
--     }


tupleWrapper =
    { wrapState = \_ ( fst, ( snd, End ) ) -> Tuple (State_ fst) (State_ snd)
    , stateWrapper = \_ _ -> identity
    , unwrapState =
        \_ (Tuple internalsFst internalsSnd) ->
            Maybe.map2 (\fst snd -> ( fst, ( snd, End ) ))
                (maybeExtractState internalsFst)
                (maybeExtractState internalsSnd)
    , stateUnwrapper = \_ _ -> identity
    , wrapDelta = \_ ( fst, ( snd, End ) ) -> Tuple (Delta_ fst) (Delta_ snd)
    , deltaWrapper = \_ _ -> identity
    , unwrapDelta =
        \_ (Tuple internalsFst internalsSnd) ->
            Maybe.map2 (\fst snd -> ( fst, ( snd, End ) ))
                (maybeExtractDelta internalsFst)
                (maybeExtractDelta internalsSnd)
    , deltaUnwrapper = \_ _ -> identity
    }



-- tripleWrapper :
--     { wrapState : a -> ( State b, ( State c, ( State d, End ) ) ) -> Triple b c d
--     , stateWrapper : e -> f -> g -> g
--     , unwrapState : h -> Triple i j k -> Maybe ( State i, ( State j, ( State k, End ) ) )
--     , stateUnwrapper : l -> m -> n -> n
--     , wrapDelta : o -> ( Delta p, ( Delta q, ( Delta r, End ) ) ) -> Triple p q r
--     , deltaWrapper : s -> t -> u -> u
--     , unwrapDelta : v -> Triple w x y -> Maybe ( Delta w, ( Delta x, ( Delta y, End ) ) )
--     , deltaUnwrapper : z -> a1 -> b1 -> b1
--     }


tripleWrapper =
    { wrapState = \_ ( a, ( b, ( c, End ) ) ) -> Triple (State_ a) (State_ b) (State_ c)
    , stateWrapper = \_ _ -> identity
    , unwrapState =
        \_ (Triple internalsFst internalsSnd internalsThd) ->
            Maybe.map3 (\fst snd thd -> ( fst, ( snd, ( thd, End ) ) ))
                (maybeExtractState internalsFst)
                (maybeExtractState internalsSnd)
                (maybeExtractState internalsThd)
    , stateUnwrapper = \_ _ -> identity
    , wrapDelta = \_ ( a, ( b, ( c, End ) ) ) -> Triple (Delta_ a) (Delta_ b) (Delta_ c)
    , deltaWrapper = \_ _ -> identity
    , unwrapDelta =
        \_ (Triple internalsFst internalsSnd internalsThd) ->
            Maybe.map3 (\fst snd thd -> ( fst, ( snd, ( thd, End ) ) ))
                (maybeExtractDelta internalsFst)
                (maybeExtractDelta internalsSnd)
                (maybeExtractDelta internalsThd)
    , deltaUnwrapper = \_ _ -> identity
    }



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
    = MkState InternalState state


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


{-| -}
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
        \((MkState meta state) as s) ->
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
                    , selected = meta.selected
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


{-| -}
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
        \context msg state ->
            fns.update context msg state
                |> Tuple.mapSecond (Cmd.map onUpdate)
    , view =
        \context ((MkState meta state) as s) ->
            let
                emittedAlerts =
                    fns.emitAlerts context s

                debouncingReceivers =
                    fns.collectDebouncingReceivers s

                alerts =
                    List.filter (\emittedAlert -> not <| List.member emittedAlert debouncingReceivers) emittedAlerts

                status =
                    getStatus fns.parse fns.collectFeedback alerts context s
            in
            view
                (fns.view context
                    { id = Maybe.withDefault ("control-" ++ Path.toString path) fns.id
                    , name = Maybe.withDefault ("control-" ++ Path.toString path) fns.name
                    , label = fns.label
                    , class = fns.class
                    , state = state
                    , status = status
                    , alerts = alerts
                    , selected = meta.selected
                    }
                    |> List.map (H.map onUpdate)
                )
    , submit =
        \context state ->
            let
                parsingResult =
                    fns.parse context state

                validationErrors =
                    fns.emitAlerts context state
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
        \context state ->
            fns.subscriptions context state
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
    , control : Control () state delta output
    }
    -> Program () (State state) (Delta delta)
sandbox { outputToString, control } =
    sandboxWithContext
        { outputToString = outputToString
        , control = control
        , context = ()
        }


{-| -}
sandboxWithContext :
    { outputToString : output -> String
    , control : Control context state delta output
    , context : context
    }
    -> Program () (State state) (Delta delta)
sandboxWithContext { outputToString, control, context } =
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
            \((MkState meta state) as s) ->
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
                            , status = getStatus fns.parse fns.collectFeedback alerts context (MkState meta state)
                            , alerts = alerts
                            , selected = meta.selected
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
define : Definition state delta output -> Control context state delta output
define definition =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate (\_ -> definition.update)

                parse =
                    \_ (MkState _ state) ->
                        definition.parse state
                            |> Result.mapError
                                (List.map
                                    (\message ->
                                        { message = message
                                        , label = definition.label
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
                    definition.blank
                        |> Tuple.mapFirst (MkState { status = Intact_, selected = 1 })
                        |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , initPrefilled =
                    \input ->
                        definition.prefill input
                            |> Tuple.mapFirst (MkState { status = Intact_, selected = 1 })
                            |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , baseUpdate = preUpdate
                , update = preUpdate 0
                , subControlViews = \_ _ -> []
                , view =
                    \_ viewConfig ->
                        [ definition.view
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
                , setAllIdle = \(MkState i s) -> MkState { i | status = Idle_ } s
                , emitAlerts = \_ _ -> []
                , collectDebouncingReceivers = \_ -> []
                , collectFeedback = \_ _ -> []
                , receiverCount = 0
                , label = definition.label
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions =
                    \_ (MkState _ s) ->
                        definition.subscriptions s
                            |> Sub.map StateChangedInternally
                }
        )


{-| -}
defineWithContext : DefinitionWithContext context state delta output -> Control context state delta output
defineWithContext definition =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate definition.update

                parse =
                    \context (MkState _ state) ->
                        definition.parse context state
                            |> Result.mapError
                                (List.map
                                    (\message ->
                                        { message = message
                                        , label = definition.label
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
                    definition.blank
                        |> Tuple.mapFirst (MkState { status = Intact_, selected = 1 })
                        |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , initPrefilled =
                    \input ->
                        definition.prefill input
                            |> Tuple.mapFirst (MkState { status = Intact_, selected = 1 })
                            |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , baseUpdate = preUpdate
                , update = preUpdate 0
                , subControlViews = \_ _ -> []
                , view =
                    \context viewConfig ->
                        [ definition.view context
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
                , setAllIdle = \(MkState i s) -> MkState { i | status = Idle_ } s
                , emitAlerts = \_ _ -> []
                , collectDebouncingReceivers = \_ -> []
                , collectFeedback = \_ _ -> []
                , receiverCount = 0
                , label = definition.label
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions =
                    \context (MkState _ s) ->
                        definition.subscriptions context s
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
wrapUpdate innerUpdate debounce_ context wrappedDelta (MkState meta state) =
    case wrappedDelta of
        NoDelta ->
            ( MkState meta state
            , Cmd.none
            )

        StateChangedInternally delta ->
            let
                ( newState, cmd ) =
                    innerUpdate context delta state
            in
            ( MkState meta newState
            , Cmd.map StateChangedInternally cmd
            )

        StateChangedByInput delta ->
            let
                ( newState, cmd ) =
                    innerUpdate context delta state
            in
            if debounce_ > 0 then
                ( MkState meta newState
                , Cmd.batch
                    [ Task.perform DebounceTimerSet Time.now
                    , Cmd.map StateChangedByInput cmd
                    ]
                )

            else
                ( MkState { meta | status = Idle_ } newState
                , Cmd.map StateChangedByInput cmd
                )

        DebounceTimerSet now ->
            ( MkState { meta | status = DebouncingSince now } state
            , Task.perform (\() -> DebounceTimerExpired now) (Process.sleep debounce_)
            )

        DebounceTimerExpired now ->
            case meta.status of
                DebouncingSince startTime ->
                    if now == startTime then
                        ( MkState { meta | status = Idle_ } state
                        , Cmd.none
                        )

                    else
                        ( MkState meta state
                        , Cmd.none
                        )

                _ ->
                    ( MkState meta state
                    , Cmd.none
                    )

        TagSelected idx ->
            ( MkState { meta | selected = idx } state
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


{-| -}
alertIfWithContext : (context -> output -> Bool) -> String -> Control context state delta output -> Control context state delta output
alertIfWithContext when alert (Control control) =
    Control (control >> alertEmitter when (AlertLabel alert))


{-| Emit an alert from a `Control` if its output fails to meet the predicate.

This is meant to be used in combination with `respond`, which listens for the
alert and displays an error or notification message on the appropriate
control(s).

-}
alertIf : (output -> Bool) -> String -> Control context state delta output -> Control context state delta output
alertIf when alert control =
    alertIfWithContext (\_ -> when) alert control


alertEmitter : (context -> output -> Bool) -> Alert -> ControlFns context input state delta output -> ControlFns context input state delta output
alertEmitter check alert (ControlFns ctrl) =
    ControlFns
        { ctrl
            | emitAlerts =
                \context state ->
                    let
                        oldAlerts =
                            ctrl.emitAlerts context state

                        newAlerts =
                            case ctrl.parse context state of
                                Ok output ->
                                    if check context output then
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
                \((MkState meta _) as state) ->
                    case meta.status of
                        DebouncingSince _ ->
                            alert :: ctrl.collectDebouncingReceivers state

                        _ ->
                            ctrl.collectDebouncingReceivers state
        }


{-| -}
alertAtIndexesWithContext :
    (context -> output -> List Int)
    -> String
    -> Control context (List state) (ListDelta delta) output
    -> Control context (List state) (ListDelta delta) output
alertAtIndexesWithContext toIndexes alert (Control control) =
    Control (control >> listAlertEmitter toIndexes alert)


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
    -> Control context (List state) (ListDelta delta) output
    -> Control context (List state) (ListDelta delta) output
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
                \context state ->
                    let
                        oldAlerts =
                            ctrl.emitAlerts context state

                        newAlerts =
                            case ctrl.parse context state of
                                Ok output ->
                                    [ AlertList ctrl.path alertLabel (check context output) ]

                                Err _ ->
                                    []
                    in
                    List.Extra.unique (oldAlerts ++ newAlerts)
        }


{-| -}
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


{-| Display an error on a `Control` if its output fails to meet the predicate.

This causes the `Control` to fail validation.

    positiveInt =
        int
            |> failIf
                (\x -> x < 1)
                "This must be greater than zero!"

-}
failIf : (output -> Bool) -> String -> Control context state delta output -> Control context state delta output
failIf check message control =
    failIfWithContext (\_ -> check) message control


{-| -}
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


{-| Display an note on a `Control` if its output fails to meet the predicate.

This just shows the user a message - it doesn't cause the `Control` to fail
validation.

    positiveInt =
        int
            |> noteIf
                (\x -> x < 1)
                "Should this be greater than zero?"

-}
noteIf : (output -> Bool) -> String -> Control context state delta output -> Control context state delta output
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
                        \context state ->
                            i.parse context state
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
                        \context internalViewConfig ->
                            let
                                layoutConfig =
                                    { class = String.join " " internalViewConfig.class
                                    , id = internalViewConfig.id
                                    , label = internalViewConfig.label
                                    , selected = internalViewConfig.selected
                                    , selectMsg = TagSelected
                                    }

                                subcontrols =
                                    fns.subControlViews context internalViewConfig
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
                        \context config ->
                            wrapper (i.view context config)
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
float : Control context String String Float
float =
    define
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
string : Control context String String String
string =
    define
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
char : Control context String String Char
char =
    define
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
    -> Control context enum enum enum
enum first second rest =
    define
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
bool : Control context Bool Bool Bool
bool =
    define
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
    -> Control context state delta a
    ->
        Control
            context
            (Record (Field state EndRecord))
            (Record (Field delta EndRecord))
            b
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
            (CustomType (Tag EndTag (Tag (Arg state EndTag) EndCustomType)))
            (CustomType (Tag EndTag (Tag (Arg delta EndTag) EndCustomType)))
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
            (CustomType (Tag (Arg failureState EndTag) (Tag (Arg successState EndTag) EndCustomType)))
            (CustomType (Tag (Arg failureDelta EndTag) (Tag (Arg successDelta EndTag) EndCustomType)))
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


type Tuple fst snd
    = Tuple (Internals fst) (Internals snd)


{-| A combinator that produces a tuple of two controls of given types.

    myTupleControl =
        tuple string int

-}
tuple :
    Control context state1 delta1 output1
    -> Control context state2 delta2 output2
    ->
        Control
            context
            (Tuple state1 state2)
            (Tuple delta1 delta2)
            ( output1, output2 )
tuple first second =
    productType Tuple.pair
        |> productField tupleWrapper Tuple.first first
        |> productField tupleWrapper Tuple.second second
        |> endProductType tupleWrapper
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


type Triple a b c
    = Triple (Internals a) (Internals b) (Internals c)


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
            (Triple state1 state2 state3)
            (Triple delta1 delta2 delta3)
            ( output1, output2, output3 )
triple first second third =
    productType (\a b c -> ( a, b, c ))
        |> productField tripleWrapper (\( a, _, _ ) -> a) first
        |> productField tripleWrapper (\( _, b, _ ) -> b) second
        |> productField tripleWrapper (\( _, _, c ) -> c) third
        |> endProductType tripleWrapper
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

                listUpdate context delta state =
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
                                                        itemControl.update context itemDelta item
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

                parse context (MkState _ state) =
                    List.foldr
                        (\( idx, item ) res ->
                            let
                                (ControlFns itemControl) =
                                    ctrl (Path.add idx path)
                            in
                            case res of
                                Ok outputs ->
                                    case itemControl.parse context item of
                                        Ok output ->
                                            Ok (output :: outputs)

                                        Err errs ->
                                            Err errs

                                Err errs ->
                                    case itemControl.parse context item of
                                        Ok _ ->
                                            Err errs

                                        Err newErrs ->
                                            Err (newErrs ++ errs)
                        )
                        (Ok [])
                        (List.indexedMap Tuple.pair state)

                collectDebouncingReceivers (MkState _ listState) =
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
                        ( MkState { status = Intact_, selected = 1 } initialState
                        , Cmd.map StateChangedInternally (Cmd.batch initialCmds)
                        )
                , initBlank =
                    ( MkState { status = Intact_, selected = 1 } []
                    , Cmd.none
                    )
                , baseUpdate = update
                , update = update 0
                , subControlViews = \_ _ -> []
                , view =
                    \context config ->
                        let
                            debouncingReceivers =
                                -- this is a total hack!
                                collectDebouncingReceivers (MkState { status = Intact_, selected = config.selected } config.state)
                        in
                        listView path context config debouncingReceivers ctrl
                , parse = parse
                , setAllIdle =
                    \(MkState i s) ->
                        MkState { i | status = Idle_ }
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
                    \context (MkState _ s) ->
                        List.indexedMap
                            (\idx item ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                itemControl.emitAlerts context item
                            )
                            s
                            |> List.concat
                , collectFeedback =
                    \(MkState _ listState) alerts ->
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

                                                    AlertLabel label_ ->
                                                        Just (AlertLabel label_)

                                                    AlertPath path_ number ->
                                                        Just (AlertPath path_ number)
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
                    \context (MkState _ listState) ->
                        List.indexedMap
                            (\idx itemState ->
                                let
                                    (ControlFns itemControl) =
                                        ctrl (Path.add idx path)
                                in
                                itemControl.subscriptions context itemState
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
            (Record (Field (List (State (Tuple keyState valueState))) EndRecord))
            (Record (Field (ListDelta (Tuple keyDelta valueDelta)) EndRecord))
            (Dict.Dict comparable value)
dict keyControl valueControl =
    list
        (tuple
            (keyControl
                |> respond
                    { alert = "@@dict-unique-keys"
                    , fail = True
                    , message = "Keys must be unique"
                    }
            )
            valueControl
            |> layout (\_ subcontrols -> List.concatMap .html subcontrols)
        )
        |> alertAtIndexes
            (\output ->
                output
                    |> List.map Tuple.first
                    |> nonUniqueIndexes
            )
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
    Control context state delta comparable
    ->
        Control
            context
            (Record (Field (List (State state)) EndRecord))
            (Record (Field (ListDelta delta) EndRecord))
            (Set.Set comparable)
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
    Control context state delta output
    ->
        Control
            context
            (Record (Field (List (State state)) EndRecord))
            (Record (Field (ListDelta delta) EndRecord))
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


type Record record
    = Record record


type Field field restFields
    = Field (Internals field) restFields


type EndRecord
    = EndRecord


tupleAppend : (( b, a ) -> c) -> b -> a -> c
tupleAppend previous this next =
    previous ( this, next )


tuplePrepend : a -> b -> ( b, a )
tuplePrepend this next =
    ( next, this )


{-| A data structure used to build records
-}
type RecordBuilder after afters before befores debouncingReceiverCollector deltaInitialiser errorCollector alertEmitter fns idleSetter initialCmds initialStates initialiser makeSetters parser subscriptionCollector toOutput updater viewer stateWrapper stateUnwrapper deltaWrapper deltaUnwrapper emptyDeltas
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
        , initialCmds : initialCmds
        , emptyDeltas : emptyDeltas
        , initialStates : initialStates
        , initialiser : initialiser
        , makeSetters : makeSetters
        , parser : parser
        , subscriptionCollector : subscriptionCollector
        , toOutput : toOutput
        , updater : updater
        , viewer : viewer
        , stateWrapper : stateWrapper
        , stateUnwrapper : stateUnwrapper
        , deltaWrapper : deltaWrapper
        , deltaUnwrapper : deltaUnwrapper
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



-- record : a -> RecordBuilder End End (b -> b) (c -> c) (d -> d) (e -> e) (f -> f) (g -> g) (h -> i -> i) (j -> j) (k -> l -> l) (m -> n -> n) (o -> o) (p -> p) (q -> q) (r -> r) a (s -> s) (t -> t)


record =
    productType


productType toOutput =
    RecordBuilder
        { index = 0
        , toOutput = toOutput
        , fns = \_ x -> x
        , initialStates = \_ x -> x
        , initialCmds = \_ x -> x
        , emptyDeltas = End
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
        , stateWrapper = identity
        , stateUnwrapper = identity
        , deltaWrapper = identity
        , deltaUnwrapper = identity
        }


{-| Add a field to a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field string
            |> endRecord

-}



--


field =
    productField recordWrapper


productField wrapper fromInput (Control control) (RecordBuilder builder) =
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
                tupleAppend previousFns newFns
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
                tupleAppend previousInitialStates newInitialState
        , initialCmds =
            \path ->
                let
                    newPath =
                        Path.add newIndex path

                    (ControlFns fns) =
                        control newPath

                    previousInitialDeltas =
                        builder.initialCmds path

                    newInitialDelta =
                        Tuple.second fns.initBlank
                in
                tupleAppend previousInitialDeltas newInitialDelta
        , emptyDeltas = tuplePrepend builder.emptyDeltas NoDelta
        , updater = builder.updater >> recordStateUpdater
        , viewer = builder.viewer >> recordStateViewer
        , parser = builder.parser >> recordStateValidator
        , idleSetter = builder.idleSetter >> recordStateIdleSetter
        , initialiser = builder.initialiser >> recordStateAndCmdInitialiser
        , deltaInitialiser = builder.deltaInitialiser >> recordDeltaInitialiser
        , before = tupleAppend builder.before NoDelta
        , befores = tupleAppend builder.befores builder.before
        , after = tuplePrepend builder.after NoDelta
        , afters = tuplePrepend builder.afters builder.after
        , makeSetters = builder.makeSetters >> deltaSetterMaker
        , alertEmitter = builder.alertEmitter >> recordAlertEmitter
        , errorCollector = builder.errorCollector >> recordFeedbackCollector
        , debouncingReceiverCollector = builder.debouncingReceiverCollector >> recordDebouncingReceiverCollector
        , subscriptionCollector = builder.subscriptionCollector >> recordSubscriptionCollector
        , stateWrapper = builder.stateWrapper >> wrapper.stateWrapper
        , stateUnwrapper = builder.stateUnwrapper >> wrapper.stateUnwrapper
        , deltaWrapper = builder.deltaWrapper >> wrapper.deltaWrapper
        , deltaUnwrapper = builder.deltaUnwrapper >> wrapper.deltaUnwrapper
        }


{-| Finalise the construction of a `record` combinator.

    type alias Hello =
        { hello : String }

    helloControl =
        record Hello
            |> field string
            |> endRecord

-}
endRecord =
    endProductType recordWrapper


endProductType wrapper (RecordBuilder builder) =
    Control
        (\path ->
            let
                fns =
                    builder.fns path End

                wrapState unwrappedState =
                    wrapper.wrapState builder.stateWrapper unwrappedState

                unwrapState wrappedState =
                    wrapper.unwrapState builder.stateUnwrapper wrappedState
                        |> Maybe.withDefault initialStates

                initialStates =
                    builder.initialStates path End

                -- ( String -> Record (Field String (Field Bool, EndRecord))
                -- , ( Bool -> Record (Field String (Field Bool, EndRecord))
                --   , End
                --   )
                -- )
                deltaSetters =
                    makeDeltaSetters builder.makeSetters wrapFieldDeltas builder.befores builder.afters

                -- (Cmd (Delta String), ( Cmd (Delta Bool), End ) )
                initialCmdFieldDeltas =
                    builder.initialCmds path End

                wrapFieldDeltas unwrappedFieldDeltas =
                    wrapper.wrapDelta builder.deltaWrapper unwrappedFieldDeltas

                unwrapFieldDeltas wrappedFieldDeltas =
                    wrapper.unwrapDelta builder.deltaUnwrapper wrappedFieldDeltas
                        |> Maybe.withDefault builder.emptyDeltas

                update context delta (MkState meta wrappedStates) =
                    case delta of
                        NoDelta ->
                            ( MkState meta wrappedStates, Cmd.none )

                        StateChangedByInput wrappedDeltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates builder.updater context fns deltaSetters (unwrapFieldDeltas wrappedDeltas) (unwrapState wrappedStates)
                            in
                            ( MkState meta (wrapState newState)
                            , Cmd.map StateChangedByInput cmd
                            )

                        StateChangedInternally wrappedDeltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates builder.updater context fns deltaSetters (unwrapFieldDeltas wrappedDeltas) (unwrapState wrappedStates)
                            in
                            ( MkState meta (wrapState newState)
                            , Cmd.map StateChangedInternally cmd
                            )

                        TagSelected index ->
                            ( MkState { meta | selected = index } wrappedStates
                            , Cmd.none
                            )

                        _ ->
                            ( MkState meta wrappedStates, Cmd.none )

                subcontrolViews context config =
                    let
                        unwrappedConfig =
                            { state = unwrapState config.state
                            , alerts = config.alerts
                            , class = config.class
                            , id = config.id
                            , label = config.label
                            , name = config.name
                            , selected = config.selected
                            , status = config.status
                            }
                    in
                    viewRecordStates builder.viewer context fns deltaSetters unwrappedConfig

                view context config =
                    subcontrolViews context config
                        |> List.concatMap .html

                parse context (MkState _ state) =
                    validateRecordStates builder.parser builder.toOutput fns context (unwrapState state)

                setAllIdle (MkState i state) =
                    MkState { i | status = Idle_ } (wrapState (setAllRecordStatesToIdle builder.idleSetter fns (unwrapState state)))

                emitAlerts context (MkState _ state) =
                    emitAlertsForRecord builder.alertEmitter fns context (unwrapState state)
            in
            ControlFns
                { path = path
                , index = 0
                , initBlank =
                    ( MkState { status = Intact_, selected = 1 } (wrapState initialStates)
                    , initialCmdFieldDeltas
                        -- (Cmd (Delta String), ( Cmd (Delta Bool), End ) )
                        |> convertFieldDeltasToRecordDelta builder.deltaInitialiser deltaSetters
                        -- List (Cmd (Record (Field String (Field Bool EndRecord))))
                        |> Cmd.batch
                        -- Cmd (Record (Field String (Field Bool EndRecord)))
                        |> Cmd.map StateChangedInternally
                      -- Cmd (Delta (Record (Field String (Field Bool EndRecord))))
                    )
                , initPrefilled =
                    \output ->
                        initialiseRecordStatesAndCmds builder.initialiser output fns deltaSetters
                            |> Tuple.mapFirst (\(MkState meta subcontrolStates) -> MkState meta (wrapState subcontrolStates))
                            |> Tuple.mapSecond (Cmd.map StateChangedInternally)
                , baseUpdate = \_ -> update
                , update = update
                , subControlViews = subcontrolViews
                , view = view
                , parse = parse
                , setAllIdle = setAllIdle
                , emitAlerts = emitAlerts
                , collectFeedback = \(MkState _ states) alerts -> collectFeedbackForRecord builder.errorCollector alerts fns (unwrapState states)
                , receiverCount = 0
                , collectDebouncingReceivers = \(MkState _ states) -> collectDebouncingReceiversForRecord builder.debouncingReceiverCollector fns (unwrapState states)
                , label = "Record"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \context (MkState _ states) -> collectRecordSubscriptions builder.subscriptionCollector deltaSetters fns context (unwrapState states)
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
collectRecordSubscriptions collector setters fns context states =
    collector (\{ listSubs } -> listSubs) { listSubs = [], setters = setters, fns = fns, context = context, states = states }
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
emitAlertsForRecord alertEmitter_ fns context states =
    alertEmitter_ (\{ alerts } -> alerts)
        { alerts = []
        , fns = fns
        , context = context
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



-- makeDeltaSetters :
--     (({ output : End -> deltaSetters
--       , befores : End
--       , afters : End
--       }
--       -> deltaSetters
--      )
--      ->
--         { output : identity -> identity
--         , befores : befores
--         , afters : afters
--         }
--      -> deltaSetters
--     )
--     -> (End -> befores)
--     -> afters
--     -> deltaSetters


makeDeltaSetters makeSetters_ wrapDeltas befores afters =
    makeSetters_ (\{ output } -> output End)
        { output = identity
        , befores = befores End
        , afters = afters
        , wrapDeltas = wrapDeltas
        }



-- deltaSetterMaker :
--     ({ output : restDeltaSetters -> deltaSetters
--      , befores : befores
--      , afters : afters
--      }
--      -> deltaSetters
--     )
--     ->
--         { output : ( value -> delta, restDeltaSetters ) -> deltaSetters
--         , befores : ( ( value, after ) -> delta, befores )
--         , afters : ( after, afters )
--         }
--     -> deltaSetters


deltaSetterMaker next { output, befores, afters, wrapDeltas } =
    let
        ( before, restBefores ) =
            befores

        ( after, restAfters ) =
            afters
    in
    next
        { output = tupleAppend output (\value -> wrapDeltas (before ( value, after )))
        , befores = restBefores
        , afters = restAfters
        , wrapDeltas = wrapDeltas
        }


initialiseRecordStatesAndCmds :
    (({ states : End -> states
      , deltas : List (Cmd delta)
      , recordInput : recordInput
      , fns : End
      , deltaSetters : End
      }
      -> ( State states, Cmd delta )
     )
     ->
        { states : identity -> identity
        , deltas : List (Cmd delta)
        , recordInput : recordInput
        , fns : recordFns
        , deltaSetters : deltaSetters
        }
     -> ( State states, Cmd delta )
    )
    -> recordInput
    -> recordFns
    -> deltaSetters
    -> ( State states, Cmd delta )
initialiseRecordStatesAndCmds initialiser input fns deltaSetters =
    initialiser
        (\{ states, deltas } ->
            ( MkState { status = Intact_, selected = 1 } (states End)
            , Cmd.batch deltas
            )
        )
        { states = identity
        , deltas = []
        , recordInput = input
        , fns = fns
        , deltaSetters = deltaSetters
        }


recordStateAndCmdInitialiser :
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
recordStateAndCmdInitialiser next { states, deltas, recordInput, fns, deltaSetters } =
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
        { states = tupleAppend states state
        , deltas = Cmd.map deltaSetter delta :: deltas
        , recordInput = recordInput
        , fns = restFns
        , deltaSetters = restDeltaSetters
        }


convertFieldDeltasToRecordDelta :
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
    -> List (Cmd recordDelta)
convertFieldDeltasToRecordDelta deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_ (\{ cmds } -> cmds)
        { cmds = []
        , deltaSetters = deltaSetters
        , fieldDeltas = deltas
        }


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
        { outputStates = tupleAppend outputStates (controlFns.setAllIdle inputState)
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
    -> InternalViewConfig ( State state, restStates )
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

        ( MkState meta state, restStates ) =
            states

        view =
            controlFns.view context
                { id = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.id
                , name = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.name
                , label = controlFns.label
                , class = controlFns.class
                , state = state
                , status = getStatus controlFns.parse controlFns.collectFeedback alerts context (MkState meta state)
                , alerts = alerts
                , selected = meta.selected
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
getStatus parse collectErrors alerts context ((MkState meta _) as state) =
    case meta.status of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                parsedErrors =
                    case parse context state of
                        Ok _ ->
                            []

                        Err errs ->
                            errs

                flaggedErrors =
                    collectErrors state alerts
            in
            Idle (parsedErrors ++ flaggedErrors)


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
            Cmd.map deltaSetter newCmd
    in
    next
        { newStates = tupleAppend newStates newState
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


type CustomType customType
    = CustomType customType


type Tag args restTags
    = Tag (Internals args) restTags


type EndCustomType
    = EndCustomType


{-| A data structure used to build custom types
-}
type CustomTypeBuilder applyInputs debouncingReceiverCollector deltaAfter deltaAfters deltaBefore deltaBefores destructor errorCollector alertEmitter fns idleSetter initialDeltas initialStates initialiseDeltas makeDeltaSetters makeStateSetters parser stateAfter stateAfters stateBefore stateBefores stateInserter subscriptionCollector toArgStates updater viewer stateWrapper stateUnwrapper deltaWrapper deltaUnwrapper
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
        , stateWrapper : stateWrapper
        , stateUnwrapper : stateUnwrapper
        , deltaWrapper : deltaWrapper
        , deltaUnwrapper : deltaUnwrapper
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
            (stateWrapper -> stateWrapper)
            (stateUnwrapper -> stateUnwrapper)
            (deltaWrapper -> deltaWrapper)
            (deltaUnwrapper -> deltaUnwrapper)
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
        , stateWrapper = identity
        , stateUnwrapper = identity
        , deltaWrapper = identity
        , deltaUnwrapper = identity
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
                tupleAppend
                    (builder.fns path)
                    (ControlFns { controlFns | index = newIndex })
        , initialStates =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                tupleAppend
                    (builder.initialStates path)
                    (controlFns.initBlank |> Tuple.first)
        , initialDeltas =
            \path ->
                let
                    (ControlFns controlFns) =
                        control (Path.add newIndex path)
                in
                tupleAppend
                    (builder.initialDeltas path)
                    (controlFns.initBlank |> Tuple.second)
        , updater = builder.updater >> customTypeStateUpdater
        , viewer = builder.viewer >> selectedTagViewer
        , parser = builder.parser >> selectedTagParser
        , idleSetter = builder.idleSetter >> selectedTagIdleSetter
        , deltaBefore = tupleAppend builder.deltaBefore NoDelta
        , deltaBefores = tupleAppend builder.deltaBefores builder.deltaBefore
        , deltaAfter = tuplePrepend builder.deltaAfter NoDelta
        , deltaAfters = tuplePrepend builder.deltaAfters builder.deltaAfter
        , makeDeltaSetters = builder.makeDeltaSetters >> deltaSetterMaker
        , initialiseDeltas = builder.initialiseDeltas >> customTypeDeltaInitialiser
        , stateBefore = tupleAppend builder.stateBefore Nothing
        , stateBefores = tupleAppend builder.stateBefores builder.stateBefore
        , toArgStates = tupleAppend builder.toArgStates toArgState
        , stateAfter = tuplePrepend builder.stateAfter Nothing
        , stateAfters = tuplePrepend builder.stateAfters builder.stateAfter
        , inputToStateConverters = builder.inputToStateConverters >> convertInputToState
        , initialStateOverrider = builder.initialStateOverrider >> initialStateOverrider
        , applyInputs = builder.applyInputs >> inputToStateConverterToDestructorApplier
        , alertEmitter = builder.alertEmitter >> customTypeAlertEmitter
        , errorCollector = builder.errorCollector >> customTypeFeedbackCollector
        , debouncingReceiverCollector = builder.debouncingReceiverCollector >> customTypeDebouncingReceiverCollector
        , subscriptionCollector = builder.subscriptionCollector >> customTypeSubscriptionCollector
        , destructor = builder.destructor
        , stateWrapper = builder.stateWrapper >> customTypeWrapper.stateWrapper
        , stateUnwrapper = builder.stateUnwrapper >> customTypeWrapper.stateUnwrapper
        , deltaWrapper = builder.deltaWrapper >> customTypeWrapper.deltaWrapper
        , deltaUnwrapper = builder.deltaUnwrapper >> customTypeWrapper.deltaUnwrapper
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
                wrapState =
                    customTypeWrapper.wrapState builder.stateWrapper

                unwrapState wrappedStates =
                    customTypeWrapper.unwrapState builder.stateUnwrapper wrappedStates
                        |> Maybe.withDefault initialStates

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
                    \context delta (MkState meta wrappedState) ->
                        let
                            state =
                                unwrapState wrappedState
                        in
                        case delta of
                            NoDelta ->
                                ( MkState meta wrappedState, Cmd.none )

                            TagSelected idx ->
                                ( MkState { meta | selected = idx } wrappedState, Cmd.none )

                            StateChangedByInput tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates builder.updater context fns deltaSetters tagDelta state
                                in
                                ( MkState meta (wrapState newTagStates)
                                , Cmd.map StateChangedByInput cmd
                                )

                            StateChangedInternally tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateCustomTypeStates builder.updater context fns deltaSetters tagDelta state
                                in
                                ( MkState meta (wrapState newTagStates)
                                , Cmd.map StateChangedInternally cmd
                                )

                            _ ->
                                ( MkState meta wrappedState, Cmd.none )

                subcontrolView context config =
                    let
                        unwrappedConfig =
                            { state = unwrapState config.state
                            , alerts = config.alerts
                            , class = config.class
                            , id = config.id
                            , label = config.label
                            , name = config.name
                            , selected = config.selected
                            , status = config.status
                            }
                    in
                    viewSelectedTagState builder.viewer context fns deltaSetters unwrappedConfig

                view context config =
                    customTypeView context config subcontrolView

                parse context (MkState meta state) =
                    validateSelectedTagState builder.parser meta.selected context fns (unwrapState state)

                setAllIdle =
                    \(MkState meta state) ->
                        MkState
                            { meta | status = Idle_ }
                            (wrapState (setSelectedTagStateIdle builder.idleSetter meta.selected fns (unwrapState state)))

                emitAlerts context (MkState meta state) =
                    emitAlertsForCustomType builder.alertEmitter context meta.selected fns (unwrapState state)
            in
            ControlFns
                { path = path
                , index = 0
                , initBlank =
                    ( MkState { status = Intact_, selected = 1 } (wrapState initialStates)
                    , Cmd.batch (initialiseCustomTypeDeltas builder.initialiseDeltas deltaSetters initialDeltas)
                    )
                , initPrefilled =
                    \tag ->
                        let
                            destructor =
                                applyInputToStateConvertersToDestructor builder.applyInputs builder.destructor stateSetters

                            ( MkState meta states, initPrefilledDelta ) =
                                destructor tag

                            initPrefilledState =
                                MkState meta (wrapState states)

                            deltas =
                                initialiseCustomTypeDeltas builder.initialiseDeltas deltaSetters initialDeltas
                                    |> List.indexedMap
                                        (\idx initDelta ->
                                            if (idx + 1) == meta.selected then
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
                , collectFeedback = \(MkState _ states) alerts -> collectFeedbackForCustomType builder.errorCollector alerts fns (unwrapState states)
                , receiverCount = 0
                , collectDebouncingReceivers = \(MkState _ states) -> collectDebouncingReceiversForCustomType builder.debouncingReceiverCollector fns (unwrapState states)
                , label = "Custom Type"
                , id = Nothing
                , name = Nothing
                , class = []
                , subscriptions = \context (MkState _ states) -> collectCustomTypeSubscriptions builder.subscriptionCollector context deltaSetters fns (unwrapState states)
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


type Arg arg restArgs
    = Arg (Internals arg) restArgs


type EndTag
    = EndTag


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


null : tag -> Control context EndTag () tag
null tag =
    define
        { blank = ( EndTag, Cmd.none )
        , prefill = \_ -> ( EndTag, Cmd.none )
        , view = \_ -> []
        , update = \_ EndTag -> ( EndTag, Cmd.none )
        , parse = \EndTag -> Ok tag
        , subscriptions = \EndTag -> Sub.none
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
        (productType tag
            |> productField argWrapper Tuple.first control
            |> endProductType argWrapper
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
        (productType tag
            |> productField argWrapper Tuple.first control1
            |> productField argWrapper (Tuple.second >> Tuple.first) control2
            |> endProductType argWrapper
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
        (productType tag
            |> productField argWrapper Tuple.first control1
            |> productField argWrapper (Tuple.second >> Tuple.first) control2
            |> productField argWrapper (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> endProductType argWrapper
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, End ) ) )
        )


{-| Add a tag with four arguments to a custom type.
-}
tag4 label_ tag control1 control2 control3 control4 =
    tagHelper
        label_
        (productType tag
            |> productField argWrapper Tuple.first control1
            |> productField argWrapper (Tuple.second >> Tuple.first) control2
            |> productField argWrapper (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> productField argWrapper (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> endProductType argWrapper
        )
        (\insertArgStateIntoTagStates arg1 arg2 arg3 arg4 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, ( arg3, ( arg4, End ) ) ) )
        )


{-| Add a tag with five arguments to a custom type.
-}
tag5 label_ tag control1 control2 control3 control4 control5 =
    tagHelper
        label_
        (productType tag
            |> productField argWrapper Tuple.first control1
            |> productField argWrapper (Tuple.second >> Tuple.first) control2
            |> productField argWrapper (Tuple.second >> Tuple.second >> Tuple.first) control3
            |> productField argWrapper (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control4
            |> productField argWrapper (Tuple.second >> Tuple.second >> Tuple.second >> Tuple.second >> Tuple.first) control5
            |> endProductType argWrapper
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
    (({ subs : List (Sub customTypeDelta)
      , context : context
      , deltaSetters : End
      , fns : End
      , states : End
      }
      -> List (Sub customTypeDelta)
     )
     ->
        { subs : List (Sub customTypeDelta)
        , context : context
        , deltaSetters : ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
        , fns : ( ControlFns context input state tagDelta output, restFns )
        , states : ( State state, restStates )
        }
     -> List (Sub customTypeDelta)
    )
    -> context
    -> ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
    -> ( ControlFns context input state tagDelta output, restFns )
    -> ( State state, restStates )
    -> Sub (Delta customTypeDelta)
collectCustomTypeSubscriptions collector context setters fns states =
    collector (\{ subs } -> subs)
        { subs = []
        , deltaSetters = setters
        , fns = fns
        , context = context
        , states = states
        }
        |> Sub.batch
        |> Sub.map StateChangedInternally


customTypeSubscriptionCollector :
    ({ subs : List (Sub customTypeDelta)
     , context : context
     , deltaSetters : restDeltaSetters
     , fns : restFns
     , states : restStates
     }
     -> List (Sub customTypeDelta)
    )
    ->
        { subs : List (Sub customTypeDelta)
        , context : context
        , deltaSetters : ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
        , fns : ( ControlFns context input state tagDelta output, restFns )
        , states : ( State state, restStates )
        }
    -> List (Sub customTypeDelta)
customTypeSubscriptionCollector next { subs, deltaSetters, fns, context, states } =
    let
        ( setter, restDeltaSetters ) =
            deltaSetters

        ( ControlFns controlFns, restFns ) =
            fns

        ( state, restStates ) =
            states
    in
    next
        { subs = (controlFns.subscriptions context state |> Sub.map setter) :: subs
        , context = context
        , deltaSetters = restDeltaSetters
        , fns = restFns
        , states = restStates
        }


collectDebouncingReceiversForCustomType :
    (({ receivers : List Alert
      , fns : End
      , states : End
      }
      -> List Alert
     )
     ->
        { receivers : List Alert
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
     -> List Alert
    )
    -> ( ControlFns context input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
collectDebouncingReceiversForCustomType debouncingReceiverCollector_ fns states =
    debouncingReceiverCollector_ (\{ receivers } -> receivers)
        { receivers = []
        , fns = fns
        , states = states
        }


customTypeDebouncingReceiverCollector :
    ({ receivers : List Alert
     , fns : restFns
     , states : restStates
     }
     -> List Alert
    )
    ->
        { receivers : List Alert
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
    -> List Alert
customTypeDebouncingReceiverCollector next { receivers, fns, states } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( state, restStates ) =
            states
    in
    next
        { receivers = receivers ++ controlFns.collectDebouncingReceivers state
        , fns = restFns
        , states = restStates
        }


updateCustomTypeStates :
    (({ newStates : End -> finalCustomTypeStates
      , newCmds : List (Cmd customTypeDelta)
      , context : context
      , fns : End
      , deltaSetters : End
      , deltas : End
      , states : End
      }
      ->
        { newStates : End -> finalCustomTypeStates
        , newCmds : List (Cmd customTypeDelta)
        }
     )
     ->
        { newStates : identity -> identity
        , newCmds : List (Cmd customTypeDelta)
        , context : context
        , fns : ( ControlFns context input state delta output, restFns )
        , deltaSetters : ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
        , deltas : ( Delta delta, restDeltas )
        , states : ( State state, restStates )
        }
     ->
        { newStates : End -> finalCustomTypeStates
        , newCmds : List (Cmd customTypeDelta)
        }
    )
    -> context
    -> ( ControlFns context input state delta output, restFns )
    -> ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
    -> ( Delta delta, restDeltas )
    -> ( State state, restStates )
    -> ( finalCustomTypeStates, Cmd customTypeDelta )
updateCustomTypeStates updater context fns deltaSetters deltas states =
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
                , fns = fns
                , deltaSetters = deltaSetters
                , deltas = deltas
                , context = context
                , states = states
                }
    in
    ( newStates End, Cmd.batch newCmds )


customTypeStateUpdater :
    ({ newStates : restStates -> intermediateCustomTypeStates
     , newCmds : List (Cmd customTypeDelta)
     , context : context
     , fns : restFns
     , deltaSetters : restDeltaSetters
     , deltas : restDeltas
     , states : restStates
     }
     ->
        { newStates : End -> finalCustomTypeStates
        , newCmds : List (Cmd customTypeDelta)
        }
    )
    ->
        { newStates : ( State state, restStates ) -> intermediateCustomTypeStates
        , newCmds : List (Cmd customTypeDelta)
        , context : context
        , fns : ( ControlFns context input state tagDelta output, restFns )
        , deltaSetters : ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
        , deltas : ( Delta tagDelta, restDeltas )
        , states : ( State state, restStates )
        }
    ->
        { newStates : End -> finalCustomTypeStates
        , newCmds : List (Cmd customTypeDelta)
        }
customTypeStateUpdater next { newStates, newCmds, context, fns, deltaSetters, deltas, states } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( deltaSetter, restDeltaSetters ) =
            deltaSetters

        ( delta, restDeltas ) =
            deltas

        ( state, restStates ) =
            states

        ( newState, newCmd ) =
            controlFns.update context delta state
    in
    next
        { newStates = tupleAppend newStates newState
        , newCmds = Cmd.map deltaSetter newCmd :: newCmds
        , context = context
        , fns = restFns
        , deltaSetters = restDeltaSetters
        , deltas = restDeltas
        , states = restStates
        }


initialiseCustomTypeDeltas :
    (({ cmds : List (Cmd customTypeDelta)
      , deltaSetters : End
      , deltas : End
      }
      -> List (Cmd customTypeDelta)
     )
     ->
        { cmds : List (Cmd customTypeDelta)
        , deltaSetters : ( tagDelta -> customTypeDelta, restDeltaSetters )
        , deltas : ( Cmd tagDelta, restDeltas )
        }
     -> List (Cmd customTypeDelta)
    )
    -> ( tagDelta -> customTypeDelta, restDeltaSetters )
    -> ( Cmd tagDelta, restDeltas )
    -> List (Cmd (Delta customTypeDelta))
initialiseCustomTypeDeltas deltaInitialiser_ deltaSetters deltas =
    deltaInitialiser_
        (\{ cmds } -> cmds)
        { cmds = []
        , deltaSetters = deltaSetters
        , deltas = deltas
        }
        |> List.reverse
        |> List.map (Cmd.map StateChangedInternally)


customTypeDeltaInitialiser :
    ({ cmds : List (Cmd customTypeDelta)
     , deltaSetters : restDeltaSetters
     , deltas : restDeltas
     }
     -> List (Cmd customTypeDelta)
    )
    ->
        { cmds : List (Cmd customTypeDelta)
        , deltaSetters : ( tagDelta -> customTypeDelta, restDeltaSetters )
        , deltas : ( Cmd tagDelta, restDeltas )
        }
    -> List (Cmd customTypeDelta)
customTypeDeltaInitialiser next { cmds, deltaSetters, deltas } =
    let
        ( setter, restDeltaSetters ) =
            deltaSetters

        ( delta, restDeltas ) =
            deltas
    in
    next
        { cmds = Cmd.map setter delta :: cmds
        , deltaSetters = restDeltaSetters
        , deltas = restDeltas
        }


applyInputToStateConvertersToDestructor :
    (({ destructor : tag -> ( State tagStates, Cmd delta )
      , inputToStateConverters : End
      }
      -> (tag -> ( State tagStates, Cmd delta ))
     )
     ->
        { destructor : inputToStateConverter -> destructor
        , inputToStateConverters : ( inputToStateConverter, restInputToStateConverters )
        }
     -> (tag -> ( State tagStates, Cmd delta ))
    )
    -> (inputToStateConverter -> destructor)
    -> ( inputToStateConverter, restInputToStateConverters )
    -> (tag -> ( State tagStates, Cmd delta ))
applyInputToStateConvertersToDestructor inputToStateConverterToDestructorApplier_ destructor inputToStateConverters =
    inputToStateConverterToDestructorApplier_
        .destructor
        { destructor = destructor
        , inputToStateConverters = inputToStateConverters
        }


inputToStateConverterToDestructorApplier :
    ({ destructor : destructor
     , inputToStateConverters : restInputToStateConverters
     }
     -> (tag -> ( State tagStates, Cmd delta ))
    )
    ->
        { destructor : inputToStateConverter -> destructor
        , inputToStateConverters : ( inputToStateConverter, restInputToStateConverters )
        }
    -> (tag -> ( State tagStates, Cmd delta ))
inputToStateConverterToDestructorApplier next { destructor, inputToStateConverters } =
    let
        ( inputToStateConverter, restInputToStateConverters ) =
            inputToStateConverters
    in
    next
        { destructor = destructor inputToStateConverter
        , inputToStateConverters = restInputToStateConverters
        }


makeInputToStateConverters :
    (({ c | finalTagStates : End -> ( toFinalTagState, restToFinalTagStates ) } -> ( toFinalTagState, restToFinalTagStates ))
     ->
        { controlFns : ( ControlFns context input state1 delta output, restControlFns )
        , deltaSetters : ( Delta delta -> tagDelta, restDeltaSetters )
        , finalTagStates : identity -> identity
        , initialTagStates : ( State state, restStates )
        , inputTuplizers : inputTuplizers
        , maybeOverridesAfter : maybeOverridesAfter
        , maybeOverridesBefore : maybeOverridesBefore
        , tagStateOverrider : tagStateOverrider
        }
     -> ( toFinalTagState, restToFinalTagStates )
    )
    -> tagStateOverrider
    -> ( State state, restStates )
    -> ( ControlFns context input state1 delta output, restControlFns )
    -> (End -> inputTuplizers)
    -> (End -> maybeOverridesBefore)
    -> maybeOverridesAfter
    -> ( Delta delta -> tagDelta, restDeltaSetters )
    -> ( toFinalTagState, restToFinalTagStates )
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
    ({ finalTagStates : b -> ( toFinalTagState, restToFinalTagStates )
     , tagStateOverrider :
        ({ thisTagIndex : Int
         , selectedTagIndex : Int
         , toTagStates : End -> tagStates
         , maybeOverrides : End
         , initialTagStates : End
         }
         -> State tagStates
        )
        ->
            { thisTagIndex : Int
            , selectedTagIndex : Int
            , toTagStates : identity -> identity
            , maybeOverrides : ( Maybe (State argState), restMaybeOverrides )
            , initialTagStates : ( State argState, restArgStates )
            }
        -> State tagStates
     , initialTagStates : ( State argState, restArgStates )
     , controlFns : restControlFns
     , inputTuplizers : restInputTuplizers
     , maybeOverridesBefore : restMaybeOverridesBefore
     , maybeOverridesAfter : restMaybeOverridesAfter
     , deltaSetters : restDeltaSetters
     }
     -> ( toFinalTagState, restToFinalTagStates )
    )
    ->
        { finalTagStates : ( k, b ) -> ( toFinalTagState, restToFinalTagStates )
        , tagStateOverrider :
            ({ thisTagIndex : Int
             , selectedTagIndex : Int
             , toTagStates : End -> tagStates
             , maybeOverrides : End
             , initialTagStates : End
             }
             -> State tagStates
            )
            ->
                { thisTagIndex : Int
                , selectedTagIndex : Int
                , toTagStates : identity -> identity
                , maybeOverrides : ( Maybe (State argState), restMaybeOverrides )
                , initialTagStates : ( State argState, restArgStates )
                }
            -> State tagStates
        , initialTagStates : ( State argState, restArgStates )
        , controlFns : ( ControlFns context input state argDelta output, restControlFns )
        , inputTuplizers : ( (input -> ( State tagStates, Cmd (Delta tagDelta) )) -> k, restInputTuplizers )
        , maybeOverridesBefore : ( ( Maybe (State state), maybeOverrideAfter ) -> ( Maybe (State argState), restMaybeOverrides ), restMaybeOverridesBefore )
        , maybeOverridesAfter : ( maybeOverrideAfter, restMaybeOverridesAfter )
        , deltaSetters : ( Delta argDelta -> tagDelta, restDeltaSetters )
        }
    -> ( toFinalTagState, restToFinalTagStates )
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
        { finalTagStates = tupleAppend finalTagStates finalTagState
        , tagStateOverrider = tagStateOverrider
        , initialTagStates = initialTagStates
        , controlFns = restFns
        , inputTuplizers = restInputTuplizers
        , maybeOverridesBefore = restMaybeOverrideBefores
        , maybeOverridesAfter = restMaybeOverrideAfters
        , deltaSetters = restDeltaSetters
        }


overrideInitialStates :
    (({ thisTagIndex : Int
      , selectedTagIndex : Int
      , toTagStates : End -> tagStates
      , maybeOverrides : End
      , initialTagStates : End
      }
      -> State tagStates
     )
     ->
        { thisTagIndex : Int
        , selectedTagIndex : Int
        , toTagStates : c -> c
        , maybeOverrides : ( Maybe (State state), restMaybeOverrides )
        , initialTagStates : ( State state, restStates )
        }
     -> State tagStates
    )
    -> ( Maybe (State state), restMaybeOverrides )
    -> ( State state, restStates )
    -> State tagStates
overrideInitialStates initialStateOverrider_ maybeOverrides initialTagStates =
    initialStateOverrider_
        (\{ selectedTagIndex, toTagStates } -> MkState { status = Intact_, selected = selectedTagIndex } (toTagStates End))
        { thisTagIndex = 1
        , selectedTagIndex = 1
        , toTagStates = identity
        , maybeOverrides = maybeOverrides
        , initialTagStates = initialTagStates
        }


initialStateOverrider :
    ({ thisTagIndex : Int
     , selectedTagIndex : Int
     , toTagStates : restArgStates -> tagStates
     , maybeOverrides : restMaybeOverrides
     , initialTagStates : restArgStates
     }
     -> State tagStates
    )
    ->
        { thisTagIndex : Int
        , selectedTagIndex : Int
        , maybeOverrides : ( Maybe (State argState), restMaybeOverrides )
        , initialTagStates : ( State argState, restArgStates )
        , toTagStates : ( State argState, restArgStates ) -> tagStates
        }
    -> State tagStates
initialStateOverrider next { thisTagIndex, selectedTagIndex, maybeOverrides, initialTagStates, toTagStates } =
    let
        ( maybeOverride, restMaybeOverrides ) =
            maybeOverrides

        ( initialTagState, restInitialTagStates ) =
            initialTagStates

        ( newSelectedTagIndex, tagArgState ) =
            case maybeOverride of
                Just override ->
                    ( thisTagIndex, override )

                Nothing ->
                    ( selectedTagIndex, initialTagState )
    in
    next
        { thisTagIndex = thisTagIndex + 1
        , selectedTagIndex = newSelectedTagIndex
        , toTagStates = tupleAppend toTagStates tagArgState
        , maybeOverrides = restMaybeOverrides
        , initialTagStates = restInitialTagStates
        }


collectFeedbackForCustomType :
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
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
     -> List Feedback
    )
    -> List Alert
    -> ( ControlFns context input state delta output, restFns )
    -> ( State state, restStates )
    -> List Feedback
collectFeedbackForCustomType feedbackCollector_ alerts fns states =
    feedbackCollector_ .feedback
        { alerts = alerts
        , feedback = []
        , fns = fns
        , states = states
        }


customTypeFeedbackCollector :
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
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
    -> List Feedback
customTypeFeedbackCollector next { alerts, feedback, fns, states } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( state, restStates ) =
            states
    in
    next
        { alerts = alerts
        , feedback = feedback ++ controlFns.collectFeedback state alerts
        , fns = restFns
        , states = restStates
        }


emitAlertsForCustomType :
    (({ alerts : List Alert
      , context : context
      , selectedTag : Int
      , fns : End
      , states : End
      }
      -> List Alert
     )
     ->
        { alerts : List Alert
        , context : context
        , selectedTag : Int
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
     -> List Alert
    )
    -> context
    -> Int
    -> ( ControlFns context input state delta output, restFns )
    -> ( State state, restStates )
    -> List Alert
emitAlertsForCustomType alertEmitter_ context selectedTag fns tagStates =
    alertEmitter_ .alerts
        { alerts = []
        , context = context
        , selectedTag = selectedTag
        , fns = fns
        , states = tagStates
        }


customTypeAlertEmitter :
    ({ alerts : List Alert
     , context : context
     , selectedTag : Int
     , fns : restFns
     , states : restStates
     }
     -> List Alert
    )
    ->
        { alerts : List Alert
        , context : context
        , selectedTag : Int
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
    -> List Alert
customTypeAlertEmitter next { alerts, context, selectedTag, fns, states } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( tagState, restTagStates ) =
            states

        newAlerts =
            if controlFns.index == selectedTag then
                controlFns.emitAlerts context tagState

            else
                []
    in
    next
        { alerts = alerts ++ newAlerts
        , context = context
        , selectedTag = selectedTag
        , fns = restFns
        , states = restTagStates
        }


setSelectedTagStateIdle :
    (({ selectedTag : Int
      , fns : End
      , initialStates : End
      , toFinalStates : End -> ( State finalState, restFinalStates )
      }
      -> ( State finalState, restFinalStates )
     )
     ->
        { selectedTag : Int
        , fns : ( ControlFns context input state delta output, restFns )
        , initialStates : ( State initialState, restInitialStates )
        , toFinalStates : identity -> identity
        }
     -> ( State finalState, restFinalStates )
    )
    -> Int
    -> ( ControlFns context input state delta output, restFns )
    -> ( State initialState, restInitialStates )
    -> ( State finalState, restFinalStates )
setSelectedTagStateIdle idleSetter_ selectedTag fns tagStates =
    idleSetter_
        (\{ toFinalStates } -> toFinalStates End)
        { selectedTag = selectedTag
        , fns = fns
        , initialStates = tagStates
        , toFinalStates = identity
        }


selectedTagIdleSetter :
    ({ initialStates : restInitialStates
     , toFinalStates : restInitialStates -> ( State finalState, restFinalStates )
     , selectedTag : Int
     , fns : restFns
     }
     -> ( State finalState, restFinalStates )
    )
    ->
        { selectedTag : Int
        , fns : ( ControlFns context input initialState delta output, restFns )
        , initialStates : ( State initialState, restInitialStates )
        , toFinalStates : ( State initialState, restInitialStates ) -> ( State finalState, restFinalStates )
        }
    -> ( State finalState, restFinalStates )
selectedTagIdleSetter next { selectedTag, fns, initialStates, toFinalStates } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( state, restInitialStates ) =
            initialStates

        newState =
            if controlFns.index == selectedTag then
                controlFns.setAllIdle state

            else
                state
    in
    next
        { initialStates = restInitialStates
        , toFinalStates = tupleAppend toFinalStates newState
        , selectedTag = selectedTag
        , fns = restFns
        }


validateSelectedTagState :
    (({ parsedResult : Result (List Feedback) output
      , selectedTag : Int
      , context : context
      , fns : End
      , states : End
      }
      -> Result (List Feedback) output
     )
     ->
        { parsedResult : Result (List Feedback) output
        , selectedTag : Int
        , context : context
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
     -> Result (List Feedback) output
    )
    -> Int
    -> context
    -> ( ControlFns context input state delta output, restFns )
    -> ( State state, restStates )
    -> Result (List Feedback) output
validateSelectedTagState parser selectedTag context fns states =
    parser
        .parsedResult
        { parsedResult =
            Err
                [ { path = Path.root
                  , label = "FATAL ERROR"
                  , message = "tag index " ++ String.fromInt selectedTag ++ " not found"
                  , fail = True
                  }
                ]
        , selectedTag = selectedTag
        , fns = fns
        , context = context
        , states = states
        }


selectedTagParser :
    ({ parsedResult : Result (List Feedback) output
     , selectedTag : Int
     , fns : restFns
     , context : context
     , states : restStates
     }
     -> Result (List Feedback) output
    )
    ->
        { parsedResult : Result (List Feedback) output
        , selectedTag : Int
        , context : context
        , fns : ( ControlFns context input state delta output, restFns )
        , states : ( State state, restStates )
        }
    -> Result (List Feedback) output
selectedTagParser next { parsedResult, selectedTag, context, fns, states } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( state, restStates ) =
            states
    in
    next
        { parsedResult =
            if controlFns.index == selectedTag then
                controlFns.parse context state

            else
                parsedResult
        , selectedTag = selectedTag
        , fns = restFns
        , context = context
        , states = restStates
        }


viewSelectedTagState :
    (({ subcontrols : List (Subcontrol customTypeDelta)
      , alerts : List Alert
      , context : context
      , fns : End
      , deltaSetters : End
      , states : End
      }
      -> List (Subcontrol customTypeDelta)
     )
     ->
        { subcontrols : List (Subcontrol customTypeDelta)
        , alerts : List Alert
        , context : context
        , fns : ( ControlFns context input state tagDelta output, restFns )
        , deltaSetters : ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
        , states : ( State state, restStates )
        }
     -> List (Subcontrol customTypeDelta)
    )
    -> context
    -> ( ControlFns context input state tagDelta output, restFns )
    -> ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
    -> InternalViewConfig ( State state, restStates )
    -> List (Subcontrol customTypeDelta)
viewSelectedTagState viewer context fns deltaSetters config =
    viewer .subcontrols
        { subcontrols = []
        , alerts = config.alerts
        , fns = fns
        , deltaSetters = deltaSetters
        , context = context
        , states = config.state
        }


selectedTagViewer :
    ({ subcontrols : List (Subcontrol customTypeDelta)
     , alerts : List Alert
     , context : context
     , fns : restFns
     , deltaSetters : restDeltaSetters
     , states : restStates
     }
     -> List (Subcontrol customTypeDelta)
    )
    ->
        { subcontrols : List (Subcontrol customTypeDelta)
        , alerts : List Alert
        , context : context
        , fns : ( ControlFns context input state tagDelta output, restFns )
        , deltaSetters : ( Delta tagDelta -> customTypeDelta, restDeltaSetters )
        , states : ( State state, restStates )
        }
    -> List (Subcontrol customTypeDelta)
selectedTagViewer next { subcontrols, context, alerts, fns, deltaSetters, states } =
    let
        ( ControlFns controlFns, restFns ) =
            fns

        ( setter, restDeltaSetters ) =
            deltaSetters

        ( MkState meta state, restStates ) =
            states
    in
    next
        { subcontrols =
            subcontrols
                ++ [ { html =
                        controlFns.view context
                            { id = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.id
                            , name = Maybe.withDefault ("control-" ++ Path.toString controlFns.path) controlFns.name
                            , label = controlFns.label
                            , class = controlFns.class
                            , state = state
                            , status = Intact
                            , alerts = alerts
                            , selected = meta.selected
                            }
                            |> List.map (H.map (setter >> StateChangedByInput))
                     , label = controlFns.label
                     , index = controlFns.index
                     }
                   ]
        , alerts = alerts
        , context = context
        , fns = restFns
        , deltaSetters = restDeltaSetters
        , states = restStates
        }



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
customTypeView context config toSubcontrols =
    let
        subcontrols =
            toSubcontrols context config

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
listView path context config debouncingReceivers subcontrol =
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
                            (\idx (MkState meta state) ->
                                let
                                    itemPath =
                                        Path.add idx path

                                    (ControlFns itemFns) =
                                        subcontrol itemPath

                                    relevantAlerts =
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

                                                    AlertLabel label_ ->
                                                        Just (AlertLabel label_)

                                                    AlertPath path_ number ->
                                                        Just (AlertPath path_ number)
                                            )
                                            config.alerts

                                    relevantNonDebouncedAlerts =
                                        List.filter (\f -> not <| List.member f debouncingReceivers) relevantAlerts
                                in
                                H.li []
                                    [ H.div []
                                        (itemFns.view context
                                            { id = Maybe.withDefault ("control-" ++ Path.toString itemPath) itemFns.id
                                            , name = Maybe.withDefault ("control-" ++ Path.toString itemPath) itemFns.name
                                            , label = itemFns.label
                                            , class = itemFns.class
                                            , state = state
                                            , status = getStatus itemFns.parse itemFns.collectFeedback relevantNonDebouncedAlerts context (MkState meta state)
                                            , alerts = relevantNonDebouncedAlerts
                                            , selected = meta.selected
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
