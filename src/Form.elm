module Form exposing
    ( End
    , end
    , f0
    , f1
    , f10
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , f9
    , failIf2
    , field
    , new
    )

import Dict
import Dict.Extra
import Field
import List.Extra
import Process
import Result.Extra
import Task
import Time



-- LIBRARY CODE
-- Types


type End
    = End



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


f4 =
    f3 >> f1


f5 =
    f4 >> f1


f6 =
    f5 >> f1


f7 =
    f6 >> f1


f8 =
    f7 >> f1


f9 =
    f8 >> f1


f10 =
    f9 >> f1


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



-- fieldStateUpdater1 :
--     (b -> a -> c -> d)
--     ->
--         ( { e
--             | update : delta -> input -> input
--             , debounce : Float
--             , toDelta : Field.Delta g -> msg
--             , parse : input -> Result String output
--             , validators : List { check : output -> Bool, feedback : Result String String }
--           }
--         , b
--         )
--     -> ( { h | delta : Field.Delta delta }, a )
--     -> ( Field.State input delta output, c )
--     -> ( ( Field.State input delta output, Cmd msg ), d )


fieldStateUpdater1 next ( field_, fields ) ( delta, deltas ) ( state, states ) =
    let
        stateAndCmd =
            case delta.delta of
                Field.UpdateRequested d ->
                    let
                        newState =
                            { state | input = field_.update d state.input }
                    in
                    if field_.debounce > 0 then
                        ( newState
                        , Task.perform (field_.toDelta << Field.DebouncingStarted) Time.now
                        )

                    else
                        ( parseField field_ newState
                        , Cmd.none
                        )

                Field.DebouncingStarted now ->
                    ( { state
                        | lastTouched = Just now
                        , output = Field.Debouncing_
                      }
                    , Task.perform (\() -> field_.toDelta (Field.DebouncingChecked now)) (Process.sleep field_.debounce)
                    )

                Field.DebouncingChecked now ->
                    ( if state.lastTouched == Just now then
                        parseField field_ state

                      else
                        state
                    , Cmd.none
                    )

                Field.Noop ->
                    ( state
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
                Field.UpdateRequested _ ->
                    if field_.debounce <= 0 then
                        Just field_.index

                    else
                        maybeIndex

                Field.DebouncingChecked now ->
                    if state.lastTouched == Just now then
                        Just field_.index

                    else
                        maybeIndex

                _ ->
                    maybeIndex
    in
    next newMaybeIndex fields deltas states


parseAllFields : ((End -> End -> End) -> b -> c -> a) -> b -> c -> a
parseAllFields allFieldsParser fields states =
    allFieldsParser (\End End -> End) fields states


allFieldsParser1 next ( field_, fields ) ( state, states ) =
    ( parseField field_ state, next fields states )


parseField field_ state =
    let
        output =
            case field_.parse state.input of
                Err f ->
                    Field.FailedToParse f

                Ok val ->
                    Field.Parsed val
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


extractCmds cmdStripper statesAndCmds =
    let
        ( ( states, cmdsList ), _ ) =
            cmdStripper ( ( End, [] ), statesAndCmds )
    in
    ( states, Cmd.batch cmdsList )


cmdExtractor1 ( ( outState, outCmd ), ( ( state, cmd ), statesAndCmds ) ) =
    ( ( ( state, outState ), cmd :: outCmd ), statesAndCmds )


collectValidators validatorCollector fields =
    validatorCollector (\validations _ -> validations) [] fields


validatorCollector1 next output ( field_, fields ) =
    next (( field_.index, field_.validators ) :: output) fields


view_ viewer toMsg fields states =
    viewer (\_ End End -> End) toMsg fields states


viewer1 next toMsg ( field_, fields ) ( state, states ) =
    ( field_.view
        { id = field_.id
        , toMsg = \x -> toMsg (field_.toDelta (Field.UpdateRequested x))
        , input = state.input
        , status =
            case state.output of
                Field.Intact_ ->
                    Field.Intact

                Field.Debouncing_ ->
                    Field.Debouncing

                Field.FailedToParse feedback ->
                    Field.Idle [ Err feedback ]

                Field.Parsed _ ->
                    Field.Idle state.feedback
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
                    Field.Parsed o ->
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
      , validators = convertValidators
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


unfurler1 :
    ( Result error (output -> a)
    , ( { b | output : Field.Output output, feedback : List (Result String String) }, c )
    )
    -> ( Result () a, c )
unfurler1 ( toOutput, ( state, states ) ) =
    ( case ( toOutput, state.output ) of
        ( Ok fn, Field.Parsed val ) ->
            if List.any Result.Extra.isErr state.feedback then
                Err ()

            else
                Ok (fn val)

        _ ->
            Err ()
    , states
    )



-- Form builder functions


new output toMsg =
    { unfurler = identity
    , combiner = identity
    , updater = identity
    , cmdExtractor = identity
    , validatorCollector = identity
    , fieldParsedChecker = identity
    , allFieldsParser = identity
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


field idx fieldBuilder formBuilder =
    { unfurler = formBuilder.unfurler >> unfurler1
    , combiner = formBuilder.combiner >> combiner1
    , updater = formBuilder.updater >> fieldStateUpdater1
    , cmdExtractor = formBuilder.cmdExtractor >> cmdExtractor1
    , validatorCollector = formBuilder.validatorCollector >> validatorCollector1
    , fieldParsedChecker = formBuilder.fieldParsedChecker >> fieldParsedChecker1
    , allFieldsParser = formBuilder.allFieldsParser >> allFieldsParser1
    , viewer = formBuilder.viewer >> viewer1
    , toLister = formBuilder.toLister >> toLister1
    , stateReverser = formBuilder.stateReverser >> reverser1
    , fieldReverser = formBuilder.fieldReverser >> reverser1
    , index = formBuilder.index + 1
    , inits =
        ( { input = fieldBuilder.init
          , delta = Field.Noop
          , output = Field.Intact_
          , lastTouched = Nothing
          , feedback = []
          }
        , formBuilder.inits
        )
    , fields =
        ( { index = formBuilder.index
          , id = fieldBuilder.id
          , update = fieldBuilder.update
          , view = fieldBuilder.view
          , parse = fieldBuilder.parse
          , validators = fieldBuilder.validators
          , debounce = fieldBuilder.debounce
          , setter = instantiateSelector idx |> .set
          , getter = instantiateSelector idx |> .getFieldState
          }
        , formBuilder.fields
        )
    , validators = formBuilder.validators
    , toMsg = formBuilder.toMsg
    , output = formBuilder.output
    }


end formBuilder =
    let
        inits =
            reverse formBuilder.stateReverser formBuilder.inits

        fields =
            combine formBuilder.combiner inits formBuilder.fields
                |> reverse formBuilder.fieldReverser

        fieldValidators =
            fields
                |> collectValidators formBuilder.validatorCollector
                |> Dict.fromList

        formValidators =
            formBuilder.validators
                |> Dict.Extra.mapKeys (\i -> formBuilder.index - i - 1)
    in
    { init = ( inits, Cmd.none )
    , update =
        \deltas states0 ->
            let
                ( reversedStates1, cmd ) =
                    updateFieldStates formBuilder.updater fields deltas states0
                        |> extractCmds formBuilder.cmdExtractor

                states1 =
                    reversedStates1
                        |> reverse formBuilder.stateReverser

                maybeParsedIndex =
                    checkFieldParsed formBuilder.fieldParsedChecker fields deltas states0

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
            in
            ( states2
            , Cmd.map formBuilder.toMsg cmd
            )
    , view =
        \states ->
            view_ formBuilder.viewer formBuilder.toMsg fields states
                |> toList formBuilder.toLister
    , submit =
        \states0 ->
            let
                states1 =
                    parseAllFields formBuilder.allFieldsParser fields states0

                allFormValidators =
                    Dict.values formValidators
                        |> List.concat

                states2 =
                    List.foldl (\v s -> v s) states1 allFormValidators
            in
            case unfurl formBuilder.unfurler formBuilder.output states2 of
                Ok output ->
                    Ok output

                Err () ->
                    Err states2
    }



-- FORM-LEVEL VALIDATION (OF MULTIPLE FIELDS)


failIf2 check feedback selector1 selector2 formBuilder =
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
                    ( Field.Parsed output1, Field.Parsed output2 ) ->
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
