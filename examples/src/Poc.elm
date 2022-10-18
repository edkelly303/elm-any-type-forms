module Poc exposing (main)

import Browser
import Dict
import Dict.Extra
import Html as H exposing (..)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import List.Extra
import Process
import Result.Extra
import Task
import Time



-- USERLAND CODE
-- Userland type definitions


type Model
    = Editing UserFormFields
    | Completed User


type Msg
    = UserFormUpdated UserFormFields
    | Submitted
    | Back


type alias UserFormFields =
    ( State String String Int
    , ( State String String Float
      , ( State String String String
        , ( State String String String, End )
        )
      )
    )


type alias User =
    { age : Int
    , height : Float
    , weight : String
    , x : String
    }



-- Userland form definitions


form :
    { init : ( UserFormFields, Cmd Msg )
    , update : UserFormFields -> UserFormFields -> ( UserFormFields, Cmd Msg )
    , view : UserFormFields -> List (Html Msg)
    , submit : UserFormFields -> Result UserFormFields User
    }
form =
    form_new User UserFormUpdated
        |> form_field f0 (field_int "Age")
        |> form_field f1 (field_float "Height")
        |> form_field f2
            (field_string "Name"
                |> field_debounce 500
                |> field_failIf (\name -> String.length name < 2) "name must be at least 2 characters"
                |> field_showIf (\name -> name == "e") "that's a good start"
            )
        |> form_field f3
            (field_string "Pet's name"
                |> field_debounce 500
                |> field_failIf (\name -> String.length name < 2) "pet's name must be at least 2 characters"
            )
        |> form_failIf2
            (\int flt -> int > round flt)
            "height must be greater than age"
            f0
            f1
        |> form_failIf2
            (\int name -> String.length name < int)
            "name must be at least as long as age"
            f0
            f2
        |> form_end



-- Userland widget definitions


field_int : String -> FieldBuilder String String Int (Html msg) msg
field_int id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse =
        \input ->
            case String.toInt input of
                Just i ->
                    Ok i

                Nothing ->
                    Err "not an int"
    , validators = []
    , debounce = 0
    }


field_float : String -> FieldBuilder String String Float (Html msg) msg
field_float id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse =
        \input ->
            case String.toFloat input of
                Just f ->
                    Ok f

                Nothing ->
                    Err "not an int"
    , validators = []
    , debounce = 0
    }


field_string_view : ViewConfig msg -> Html msg
field_string_view fieldState =
    div [ HA.style "margin-bottom" "30px" ]
        [ div
            [ HA.style "margin-bottom" "10px" ]
            [ text fieldState.id ]
        , H.input
            [ onInput fieldState.toMsg
            , HA.value fieldState.input
            , HA.style "background-color"
                (case fieldState.status of
                    Intact ->
                        "white"

                    Debouncing ->
                        "lightYellow"

                    Idle feedback ->
                        if List.any Result.Extra.isErr feedback then
                            "pink"

                        else
                            "paleGreen"
                )
            ]
            [ text fieldState.input ]
        , case fieldState.status of
            Idle feedback ->
                div []
                    (List.map
                        (\f ->
                            let
                                ( icon, txt ) =
                                    case f of
                                        Ok t ->
                                            ( "✅", t )

                                        Err t ->
                                            ( "❌", t )
                            in
                            div
                                [ HA.style "margin-top" "5px" ]
                                [ text (icon ++ " " ++ txt) ]
                        )
                        feedback
                    )

            _ ->
                text ""
        ]


field_string : String -> FieldBuilder String String String (Html msg) msg
field_string id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse = Ok
    , validators = []
    , debounce = 500
    }


field_debounce :
    Float
    -> FieldBuilder input delta output element msg
    -> FieldBuilder input delta output element msg
field_debounce millis fb =
    { fb | debounce = millis }


field_failIf :
    (output -> Bool)
    -> String
    -> FieldBuilder input delta output element msg
    -> FieldBuilder input delta output element msg
field_failIf check feedback fb =
    { fb | validators = { check = check, feedback = Err feedback } :: fb.validators }


field_showIf :
    (output -> Bool)
    -> String
    -> FieldBuilder input delta output element msg
    -> FieldBuilder input delta output element msg
field_showIf check feedback fb =
    { fb | validators = { check = check, feedback = Ok feedback } :: fb.validators }



-- Userland program plumbing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserFormUpdated delta ->
            case model of
                Editing form_ ->
                    form.update delta form_
                        |> Tuple.mapFirst Editing

                Completed _ ->
                    ( model, Cmd.none )

        Submitted ->
            case model of
                Editing form0 ->
                    case form.submit form0 of
                        Ok user ->
                            ( Completed user, Cmd.none )

                        Err form1 ->
                            ( Editing form1, Cmd.none )

                Completed _ ->
                    ( model, Cmd.none )

        Back ->
            form.init
                |> Tuple.mapFirst Editing


view : Model -> Html Msg
view model =
    case model of
        Editing form_ ->
            div []
                [ div [] (form.view form_)
                , button [ onClick Submitted ] [ text "submit" ]
                ]

        Completed user ->
            div []
                [ div [] [ text "Your user's data:" ]
                , div [] [ text ("Age: " ++ String.fromInt user.age) ]
                , div [] [ text ("Height: " ++ String.fromFloat user.height) ]
                , div [] [ text ("Weight: " ++ user.weight) ]
                , button [ onClick Back ] [ text "go back" ]
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> form.init |> Tuple.mapFirst Editing
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- LIBRARY CODE
-- Types


type End
    = End


type alias State input delta output =
    { input : input
    , delta : Delta delta
    , output : Output output
    , lastTouched : Maybe Time.Posix
    , feedback : List (Result String String)
    }


type Delta delta
    = UpdateRequested delta
    | DebouncingStarted Time.Posix
    | DebouncingChecked Time.Posix
    | Noop


type alias FieldBuilder input delta output element msg =
    { id : String
    , init : input
    , update : delta -> input -> input
    , view : ViewConfig msg -> element
    , parse : input -> Result String output
    , validators : List { check : output -> Bool, feedback : Result String String }
    , debounce : Float
    }


type Output output
    = Intact_
    | Debouncing_
    | FailedToParse String
    | Parsed output


type alias ViewConfig msg =
    { id : String
    , toMsg : String -> msg
    , input : String
    , status : Status
    }


type Status
    = Intact
    | Debouncing
    | Idle (List (Result String String))



-- State setters


set1 : (b -> y) -> ( a, b ) -> ( a, y )
set1 =
    Tuple.mapSecond


get1 : ( a, b ) -> b
get1 =
    Tuple.second


f0 =
    composeSelectors { getFieldState = identity, getField = identity, set = identity }


f1 =
    composeSelectors { getFieldState = get1, getField = get1, set = set1 }


f2 =
    f1 >> f1


f3 =
    f2 >> f1


composeSelectors selector1 selector2 =
    { getFieldState = selector1.getFieldState >> selector2.getFieldState
    , getField = selector1.getField >> selector2.getField
    , set = selector1.set >> selector2.set
    }


instantiateSelector selector =
    selector { getFieldState = identity, getField = identity, set = identity }



-- Tuple walkers


updateFieldStates : ((End -> End -> End -> End) -> fields -> deltas -> states -> statesAndCmds) -> fields -> deltas -> states -> statesAndCmds
updateFieldStates fieldStateUpdater fields deltas states =
    fieldStateUpdater (\End End End -> End) fields deltas states


fieldStateUpdater1 :
    (b -> a -> c -> d)
    ->
        ( { e
            | update : delta -> input -> input
            , debounce : Float
            , toDelta : Delta g -> msg
            , parse : input -> Result String output
            , validators : List { check : output -> Bool, feedback : Result String String }
          }
        , b
        )
    -> ( { h | delta : Delta delta }, a )
    -> ( State input delta output, c )
    -> ( ( State input delta output, Cmd msg ), d )
fieldStateUpdater1 next ( field_, fields ) ( delta, deltas ) ( state0, states ) =
    let
        stateAndCmd =
            case delta.delta of
                UpdateRequested d ->
                    let
                        newState =
                            { state0 | input = field_.update d state0.input }
                    in
                    if field_.debounce > 0 then
                        ( newState
                        , Task.perform (field_.toDelta << DebouncingStarted) Time.now
                        )

                    else
                        ( parseField field_ { newState | feedback = [] }
                        , Cmd.none
                        )

                DebouncingStarted now ->
                    ( { state0
                        | lastTouched = Just now
                        , output = Debouncing_
                      }
                    , Task.perform (\() -> field_.toDelta (DebouncingChecked now)) (Process.sleep field_.debounce)
                    )

                DebouncingChecked now ->
                    ( if state0.lastTouched == Just now then
                        parseField field_ { state0 | feedback = [] }

                      else
                        state0
                    , Cmd.none
                    )

                Noop ->
                    ( state0
                    , Cmd.none
                    )
    in
    ( stateAndCmd
    , next fields deltas states
    )


checkFieldParsed fieldParsedChecker fields deltas states =
    fieldParsedChecker (\maybeIndex _ _ _ -> maybeIndex) Nothing fields deltas states


fieldParsedChecker1 next maybeIndex ( field_, fields ) ( delta, deltas ) ( state, states ) =
    let
        newMaybeIndex =
            case delta.delta of
                UpdateRequested _ ->
                    if field_.debounce <= 0 then
                        Just field_.index

                    else
                        maybeIndex

                DebouncingChecked now ->
                    if state.lastTouched == Just now then
                        Just field_.index

                    else
                        maybeIndex

                _ ->
                    maybeIndex
    in
    next newMaybeIndex fields deltas states


validateAll : ((End -> End -> End) -> b -> c -> a) -> b -> c -> a
validateAll validateAller fields states =
    validateAller (\End End -> End) fields states


validateAller1 :
    (b -> a -> c)
    -> ( { d | parse : input -> Result String output, validators : List { check : output -> Bool, feedback : Result String String } }, b )
    -> ( State input delta output, a )
    -> ( State input delta output, c )
validateAller1 next ( field_, fields ) ( state, states ) =
    ( parseField field_ state, next fields states )


parseField :
    { a | parse : input -> Result String output, validators : List { check : output -> Bool, feedback : Result String String } }
    -> State input delta output
    -> State input delta output
parseField field_ state =
    let
        output =
            case field_.parse state.input of
                Err f ->
                    FailedToParse f

                Ok val ->
                    Parsed val
    in
    { state | output = output }


validateField : List { check : val -> Bool, feedback : Result String String } -> val -> List (Result String String)
validateField validators val =
    List.foldl
        (\{ check, feedback } feedbacks ->
            if check val then
                feedback :: feedbacks

            else
                feedbacks
        )
        []
        validators


cmdStrip cmdStripper statesAndCmds =
    let
        ( ( states, cmdsList ), _ ) =
            cmdStripper ( ( End, [] ), statesAndCmds )
    in
    ( states, Cmd.batch cmdsList )


cmdStripper1 ( ( outState, outCmd ), ( ( state, cmd ), statesAndCmds ) ) =
    ( ( ( state, outState ), cmd :: outCmd ), statesAndCmds )


collectValidators validationCollector fields =
    -- POC
    validationCollector (\validations _ -> validations) [] fields


validationCollector1 next output ( field_, fields ) =
    -- POC
    next (( field_.index, field_.validators2 ) :: output) fields


view_ viewer toMsg fields states =
    viewer (\_ End End -> End) toMsg fields states


viewer1 next toMsg ( field_, fields ) ( state, states ) =
    ( field_.view
        { id = field_.id
        , toMsg = \x -> toMsg (field_.toDelta (UpdateRequested x))
        , input = state.input
        , status =
            case state.output of
                Intact_ ->
                    Intact

                Debouncing_ ->
                    Debouncing

                FailedToParse feedback ->
                    Idle [ Err feedback ]

                Parsed _ ->
                    Idle state.feedback
        }
    , next toMsg fields states
    )


combine combiner inits fields =
    combiner (\_ End -> End) inits fields


combiner1 next inits ( field_, fields ) =
    let
        setDelta x =
            Tuple.mapFirst (\s -> { s | delta = x })

        convertValidators =
            -- POC - proving that we can convert validations into functions that
            -- take a field state and return a field state (so that all
            -- validation functions for all fields have the same type signature
            -- and can be stored in a list or dictionary)
            \state ->
                let
                    f =
                        (field_.getter >> Tuple.first) state

                    set fdbk =
                        Tuple.mapFirst (\s -> { s | feedback = fdbk })
                in
                case f.output of
                    Parsed o ->
                        let
                            feedback =
                                validateField field_.validators o
                        in
                        field_.setter (set feedback) state

                    _ ->
                        state
    in
    ( { index = field_.index
      , id = field_.id
      , update = field_.update
      , view = field_.view
      , parse = field_.parse
      , validators = field_.validators

      -- POC
      , validators2 = convertValidators
      , debounce = field_.debounce
      , toDelta = \x -> field_.setter (setDelta x) inits
      }
    , next inits fields
    )


toList : (( List a, b ) -> ( List c, d )) -> b -> List c
toList toLister tuple =
    toLister ( [], tuple )
        |> Tuple.first
        |> List.reverse


toLister1 : ( List a, ( a, b ) ) -> ( List a, b )
toLister1 ( list, ( item, items ) ) =
    ( item :: list, items )


reverse : (( End, b ) -> ( a, c )) -> b -> a
reverse reverser tuple =
    reverser ( End, tuple )
        |> Tuple.first


reverser1 : ( a, ( b, c ) ) -> ( ( b, a ), c )
reverser1 ( s, ( fst, rest ) ) =
    ( ( fst, s ), rest )


unfurl : (( Result error b, c ) -> ( a, d )) -> b -> c -> a
unfurl unfurler toOutput states =
    unfurler ( Ok toOutput, states )
        |> Tuple.first


unfurler1 : ( Result error (output -> a), ( { b | output : Output output }, c ) ) -> ( Result () a, c )
unfurler1 ( toOutput, ( state, states ) ) =
    ( case ( toOutput, state.output ) of
        ( Ok fn, Parsed val ) ->
            Ok (fn val)

        _ ->
            Err ()
    , states
    )



-- Form builder functions


form_new output toMsg =
    { unfurler = identity
    , combiner = identity
    , updater = identity
    , cmdStripper = identity

    -- POC - proving that we can collect validations from fields
    , validationCollector = identity

    -- POC - proving that we can check which field's input has been parsed (if any)
    , fieldParsedChecker = identity
    , validateAller = identity
    , viewer = identity
    , toLister = identity
    , stateReverser = identity
    , fieldReverser = identity
    , index = 0
    , inits = End
    , fields = End
    , emptyMsg = End
    , validators = Dict.empty
    , toMsg = toMsg
    , output = output
    }


form_field idx fieldBuilder f =
    { unfurler = f.unfurler >> unfurler1
    , combiner = f.combiner >> combiner1
    , updater = f.updater >> fieldStateUpdater1
    , cmdStripper = f.cmdStripper >> cmdStripper1

    -- POC - proving that we can collect validations from fields
    , validationCollector = f.validationCollector >> validationCollector1

    -- POC - proving that we can check which field's input has been parsed (if any)
    , fieldParsedChecker = f.fieldParsedChecker >> fieldParsedChecker1
    , validateAller = f.validateAller >> validateAller1
    , viewer = f.viewer >> viewer1
    , toLister = f.toLister >> toLister1
    , stateReverser = f.stateReverser >> reverser1
    , fieldReverser = f.fieldReverser >> reverser1
    , index = f.index + 1
    , inits =
        ( { input = fieldBuilder.init
          , delta = Noop
          , output = Intact_
          , lastTouched = Nothing
          , feedback = []
          }
        , f.inits
        )
    , fields =
        ( { index = f.index
          , id = fieldBuilder.id
          , update = fieldBuilder.update
          , view = fieldBuilder.view
          , parse = fieldBuilder.parse
          , validators = fieldBuilder.validators
          , debounce = fieldBuilder.debounce
          , setter = instantiateSelector idx |> .set
          , getter = instantiateSelector idx |> .getFieldState
          }
        , f.fields
        )
    , validators = f.validators
    , toMsg = f.toMsg
    , output = f.output
    }


form_end f =
    let
        inits =
            reverse f.stateReverser f.inits

        fields =
            combine f.combiner inits f.fields
                |> reverse f.fieldReverser

        -- POC - proving that we can collect validators from fields
        fieldValidators =
            fields
                |> collectValidators f.validationCollector
                |> Dict.fromList
                |> Debug.log "field validators"

        formValidators =
            f.validators
                |> Dict.Extra.mapKeys (\i -> f.index - i - 1)
                |> Debug.log "form validators"
    in
    { init = ( inits, Cmd.none )
    , update =
        \deltas states0 ->
            let
                ( reversedStates1, cmd ) =
                    -- in this step we don't just update the field that the
                    -- delta is targetting, we also clear multivalidation for
                    -- all fields
                    updateFieldStates f.updater fields deltas states0
                        |> cmdStrip f.cmdStripper

                states1 =
                    reversedStates1
                        |> reverse f.stateReverser

                -- POC - proving that we can check which field's input has been parsed (if any)
                maybeParsedIndex =
                    checkFieldParsed f.fieldParsedChecker fields deltas states0
                        |> Debug.log "updated field"

                -- POC - proving that we can validate only fields that have been detected as parsed
                states2 =
                    case maybeParsedIndex of
                        Nothing ->
                            states1

                        Just index ->
                            let
                                relevantFieldValidators =
                                    fieldValidators
                                        |> Dict.get index
                                        |> Maybe.map List.singleton
                                        |> Maybe.withDefault []

                                relevantFormValidators =
                                    formValidators
                                        |> Dict.get index
                                        |> Maybe.withDefault []
                            in
                            List.foldl (\v s -> v s) states1 (relevantFieldValidators ++ relevantFormValidators)
                                |> Debug.log "states"
            in
            ( states2
            , Cmd.map f.toMsg cmd
            )
    , view =
        \states ->
            view_ f.viewer f.toMsg fields states
                |> toList f.toLister
    , submit =
        \states ->
            let
                validatedStates =
                    validateAll f.validateAller fields states
            in
            case unfurl f.unfurler f.output validatedStates of
                Ok output ->
                    Ok output

                Err () ->
                    Err validatedStates
    }



-- FORM-LEVEL VALIDATION (OF MULTIPLE FIELDS)


form_failIf2 check feedback selector1 selector2 formBuilder =
    let
        sel1 =
            instantiateSelector selector1

        sel2 =
            instantiateSelector selector2

        index1 =
            (sel1.getField >> Tuple.first) formBuilder.fields
                |> .index

        index2 =
            (sel2.getField >> Tuple.first) formBuilder.fields
                |> .index

        v =
            \state ->
                let
                    fieldState1 =
                        (sel1.getFieldState >> Tuple.first) state

                    fieldState2 =
                        (sel2.getFieldState >> Tuple.first) state
                in
                case ( fieldState1.output, fieldState2.output ) of
                    ( Parsed output1, Parsed output2 ) ->
                        if check output1 output2 then
                            state
                                |> sel1.set
                                    (Tuple.mapFirst
                                        (\field1_ -> { field1_ | feedback = List.Extra.unique (Err feedback :: field1_.feedback) })
                                    )
                                |> sel2.set
                                    (Tuple.mapFirst
                                        (\field2_ -> { field2_ | feedback = List.Extra.unique (Err feedback :: field2_.feedback) })
                                    )

                        else
                            state
                                |> sel1.set
                                    (Tuple.mapFirst
                                        (\field1_ -> { field1_ | feedback = List.Extra.remove (Err feedback) field1_.feedback })
                                    )
                                |> sel2.set
                                    (Tuple.mapFirst
                                        (\field2_ -> { field2_ | feedback = List.Extra.remove (Err feedback) field2_.feedback })
                                    )

                    _ ->
                        state
                            |> sel1.set
                                (Tuple.mapFirst
                                    (\field1_ -> { field1_ | feedback = List.Extra.remove (Err feedback) field1_.feedback })
                                )
                            |> sel2.set
                                (Tuple.mapFirst
                                    (\field2_ -> { field2_ | feedback = List.Extra.remove (Err feedback) field2_.feedback })
                                )
    in
    { formBuilder
        | validators =
            formBuilder.validators
                |> Dict.update index1
                    (\maybeExistingValidators ->
                        case maybeExistingValidators of
                            Nothing ->
                                Just [ v ]

                            Just existingValidators ->
                                Just (v :: existingValidators)
                    )
                |> Dict.update index2
                    (\maybeExistingValidators ->
                        case maybeExistingValidators of
                            Nothing ->
                                Just [ v ]

                            Just existingValidators ->
                                Just (v :: existingValidators)
                    )
    }



-- A SKETCH OF MULTI-FIELD VALIDATION WITH APPLICATIVES
--
-- showIf : checker -> String -> Checker checker
-- showIf checker feedback =
--     { check = checker, feedback = feedback, fails = False }
-- failIf : checker -> String -> Checker checker
-- failIf checker feedback =
--     { check = checker, feedback = feedback, fails = True }
-- test : Result (List String) (List String)
-- test =
--     multivalidate
--         ( Err "f0 did not parse", ( Ok 1, ( Ok 1, End ) ) )
--         [ failIf (\f1_ f2_ -> f1_ == f2_) "failed because f1 and f2 are equal!"
--         , failIf (\f1_ f2_ -> f1_ <= f2_) "failed because f1 <= f2!"
--         , showIf (\_ _ -> True) "this always gets shown if validation passes"
--         , showIf (\f1_ _ -> f1_ == 2) "this gets shown if f1 == 2"
--         ]
--         (fld f1 >> fld f2)
-- type Validator checker formData
--     = Validator (Result (List String) (List (Checker checker))) formData
-- type alias Checker checker =
--     { check : checker
--     , fails : Bool
--     , feedback : String
--     }
-- multivalidate :
--     formData
--     -> List (Checker checker)
--     -> (Validator checker formData -> Validator Bool formData)
--     -> Result (List String) (List String)
-- multivalidate formData checkers fieldGetters =
--     Validator (Ok checkers) formData
--         |> fieldGetters
--         |> done
-- fld :
--     ({ get : a -> a, set : b -> b } -> { c | get : formData -> ( Result String d, e ) })
--     -> Validator (d -> f) formData
--     -> Validator f formData
-- fld idx (Validator resultCheckers formData) =
--     let
--         fieldGetter =
--             instantiateIndex idx |> .get
--         resultFieldData =
--             fieldGetter formData |> Tuple.first
--         appliedCheckers =
--             case ( resultCheckers, resultFieldData ) of
--                 ( Ok checkers, Ok fieldData ) ->
--                     Ok
--                         (List.map
--                             (\checker ->
--                                 { check = checker.check fieldData
--                                 , feedback = checker.feedback
--                                 , fails = checker.fails
--                                 }
--                             )
--                             checkers
--                         )
--                 ( Err previousErrs, Ok _ ) ->
--                     Err previousErrs
--                 ( Ok _, Err parsingErr ) ->
--                     Err [ parsingErr ]
--                 ( Err previousErrs, Err parsingErr ) ->
--                     Err (parsingErr :: previousErrs)
--     in
--     Validator appliedCheckers formData
-- done : Validator Bool formData -> Result (List String) (List String)
-- done (Validator resultCheckers _) =
--     case resultCheckers of
--         Err e ->
--             Err e
--         Ok [] ->
--             Ok []
--         Ok checkers ->
--             List.foldl
--                 (\{ check, feedback, fails } resultFeedbacks ->
--                     case resultFeedbacks of
--                         Err feedbacks ->
--                             if check && fails then
--                                 Err (feedback :: feedbacks)
--                             else
--                                 Err feedbacks
--                         Ok feedbacks ->
--                             if check then
--                                 if fails then
--                                     Err [ feedback ]
--                                 else
--                                     Ok (feedback :: feedbacks)
--                             else
--                                 Ok feedbacks
--                 )
--                 (Ok [])
--                 checkers
