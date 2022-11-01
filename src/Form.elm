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
    , failIf3
    , field
    , infoIf2
    , infoIf3
    , new
    )

import Dict
import Dict.Extra
import Field
import List.Extra
import Result.Extra



-- Types


type End
    = End



-- State setters


get : (b -> ( a, c )) -> b -> a
get getter data =
    (getter >> Tuple.first) data


set : ((( a, b ) -> ( x, b )) -> c -> d) -> (a -> x) -> c -> d
set setter updater data =
    setter (Tuple.mapFirst updater) data


setter1 : (b -> y) -> ( a, b ) -> ( a, y )
setter1 =
    Tuple.mapSecond


getter1 : ( a, b ) -> b
getter1 =
    Tuple.second


f0 =
    composeSelectors { getFieldState = identity, getField = identity, set = identity }


f1 =
    composeSelectors { getFieldState = getter1, getField = getter1, set = setter1 }


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


fieldStateUpdater1 next ( field_, fields ) ( delta, deltas ) ( state, states ) =
    let
        stateAndCmd =
            Field.update field_ delta state
    in
    ( stateAndCmd
    , next fields deltas states
    )


checkFieldParsed fieldParsedChecker fields deltas states =
    fieldParsedChecker (\maybeIndex _ _ _ -> maybeIndex) Nothing fields deltas states


fieldParsedChecker1 next maybeIndex ( field_, fields ) ( delta, deltas ) ( state, states ) =
    let
        newMaybeIndex =
            case Field.indexIfParsed field_ delta state of
                Just index ->
                    Just index

                Nothing ->
                    maybeIndex
    in
    next newMaybeIndex fields deltas states


parseAllFields : ((End -> End -> End) -> b -> c -> a) -> b -> c -> a
parseAllFields allFieldsParser fields states =
    allFieldsParser (\End End -> End) fields states


allFieldsParser1 next ( field_, fields ) ( state, states ) =
    ( Field.parse field_ state, next fields states )


extractCmds cmdStripper statesAndCmds =
    let
        ( ( states, cmdsList ), _ ) =
            cmdStripper ( ( End, [] ), statesAndCmds )
    in
    ( states, Cmd.batch cmdsList )


cmdExtractor1 ( ( outState, outCmd ), ( ( state, cmd ), statesAndCmds ) ) =
    ( ( ( state, outState ), cmd :: outCmd ), statesAndCmds )


collectValidators validatorCollector fields =
    validatorCollector (\validators _ -> validators) [] fields


validatorCollector1 next output ( field_, fields ) =
    next (( field_.index, field_.validators ) :: output) fields


view viewer toMsg fields states =
    viewer (\_ End End -> End) toMsg fields states


viewer1 next toMsg ( field_, fields ) ( state, states ) =
    ( Field.view toMsg field_ state
    , next toMsg fields states
    )


combine combiner inits fields =
    combiner (\_ End -> End) inits fields


combiner1 next inits ( fieldBuilder, fieldBuilders ) =
    let
        convertValidators =
            \formData ->
                let
                    fieldState =
                        get fieldBuilder.getter formData

                    newFieldState =
                        Field.validate fieldBuilder fieldState
                in
                set fieldBuilder.setter (always newFieldState) formData
    in
    ( { index = fieldBuilder.index
      , id = fieldBuilder.id
      , update = fieldBuilder.update
      , view = fieldBuilder.view
      , parse = fieldBuilder.parse
      , validators = convertValidators
      , debounce = fieldBuilder.debounce
      , toFormData = \delta -> set fieldBuilder.setter (\s -> { s | delta = delta }) inits
      }
    , next inits fieldBuilders
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
    , ( Field.State input delta output
      , c
      )
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


new :
    a
    -> b
    ->
        { unfurler : c -> c
        , combiner : d -> d
        , updater : e -> e
        , cmdExtractor : f -> f
        , validatorCollector : g -> g
        , fieldParsedChecker : h -> h
        , allFieldsParser : i -> i
        , viewer : j -> j
        , toLister : k -> k
        , stateReverser : l -> l
        , fieldReverser : m -> m
        , index : number
        , inits : End
        , fields : End
        , emptyMsg : End
        , validators : Dict.Dict n v
        , toMsg : b
        , output : a
        }
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


end :
    { a
        | stateReverser : ( End, reversedFormData ) -> ( formData, d )
        , inits : reversedFormData
        , combiner : (e -> End -> End) -> formData -> reversedFieldBuilders -> reversedFields
        , fields : reversedFieldBuilders
        , fieldReverser : ( End, reversedFields ) -> ( fields, j )
        , validatorCollector : (k -> l -> k) -> List m -> fields -> List ( Int, formData -> formData )
        , validators : Dict.Dict Int (List (formData -> formData))
        , index : Int
        , updater : (End -> End -> End -> End) -> fields -> formData -> formData -> s
        , cmdExtractor : ( ( End, List (Cmd formData) ), s ) -> ( ( reversedFormData, List (Cmd formData) ), u )
        , fieldParsedChecker : (Maybe Int -> x -> y -> z -> Maybe Int) -> Maybe Int -> fields -> formData -> formData -> Maybe Int
        , toMsg : formData -> msg
        , viewer : (e1 -> End -> End -> End) -> (formData -> msg) -> fields -> formData -> h1
        , toLister : ( List element, h1 ) -> ( List element, k1 )
        , allFieldsParser : (End -> End -> End) -> fields -> formData -> formData
        , unfurler : ( Result String o1, formData ) -> ( Result () value, r1 )
        , output : o1
    }
    ->
        { init : ( formData, Cmd msg )
        , update : formData -> formData -> ( formData, Cmd msg )
        , view : formData -> List element
        , submit : formData -> Result formData value
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
            view formBuilder.viewer formBuilder.toMsg fields states
                |> toList formBuilder.toLister
    , submit =
        \states0 ->
            let
                states1 =
                    parseAllFields formBuilder.allFieldsParser fields states0

                allFieldValidators =
                    Dict.values fieldValidators

                states2 =
                    List.foldl (\v s -> v s) states1 allFieldValidators

                allFormValidators =
                    Dict.values formValidators
                        |> List.concat

                states3 =
                    List.foldl (\v s -> v s) states2 allFormValidators
            in
            case unfurl formBuilder.unfurler formBuilder.output states3 of
                Ok output ->
                    Ok output

                Err () ->
                    Err states2
    }



-- FORM-LEVEL VALIDATION (OF MULTIPLE FIELDS)


failIf2 =
    check2 Err


infoIf2 =
    check2 Ok


check2 toResult check message selector1 selector2 formBuilder =
    let
        feedback =
            toResult message

        sel1 =
            instantiateSelector selector1

        sel2 =
            instantiateSelector selector2

        v =
            \state ->
                let
                    fieldState1 =
                        get sel1.getFieldState state

                    fieldState2 =
                        get sel2.getFieldState state
                in
                case ( fieldState1.output, fieldState2.output ) of
                    ( Field.Parsed output1, Field.Parsed output2 ) ->
                        if check output1 output2 then
                            state
                                |> addFeedback sel1 feedback fieldState1
                                |> addFeedback sel2 feedback fieldState2

                        else
                            state
                                |> removeFeedback sel1 feedback fieldState1
                                |> removeFeedback sel2 feedback fieldState2

                    _ ->
                        state
                            |> removeFeedback sel1 feedback fieldState1
                            |> removeFeedback sel2 feedback fieldState2
    in
    { formBuilder
        | validators =
            formBuilder.validators
                |> updateAtIndex (getIndex sel1 formBuilder) v
                |> updateAtIndex (getIndex sel2 formBuilder) v
    }


getIndex sel formBuilder =
    get sel.getField formBuilder.fields
        |> .index


addFeedback sel feedback fs s =
    set sel.set (\_ -> { fs | feedback = List.Extra.unique (feedback :: fs.feedback) }) s


removeFeedback sel feedback fs s =
    set sel.set (\_ -> { fs | feedback = List.Extra.remove feedback fs.feedback }) s


updateAtIndex idx v =
    Dict.update idx
        (\maybeExistingValidators ->
            case maybeExistingValidators of
                Nothing ->
                    Just [ v ]

                Just existingValidators ->
                    Just (v :: existingValidators)
        )


failIf3 =
    check3 Err


infoIf3 =
    check3 Ok


check3 toResult check message selector1 selector2 selector3 formBuilder =
    let
        feedback =
            toResult message

        sel1 =
            instantiateSelector selector1

        sel2 =
            instantiateSelector selector2

        sel3 =
            instantiateSelector selector3

        v =
            \state ->
                let
                    fieldState1 =
                        get sel1.getFieldState state

                    fieldState2 =
                        get sel2.getFieldState state

                    fieldState3 =
                        get sel3.getFieldState state
                in
                case ( fieldState1.output, fieldState2.output ) of
                    ( Field.Parsed output1, Field.Parsed output2 ) ->
                        if check output1 output2 then
                            state
                                |> addFeedback sel1 feedback fieldState1
                                |> addFeedback sel2 feedback fieldState2
                                |> addFeedback sel3 feedback fieldState3

                        else
                            state
                                |> removeFeedback sel1 feedback fieldState1
                                |> removeFeedback sel2 feedback fieldState2
                                |> removeFeedback sel3 feedback fieldState3

                    _ ->
                        state
                            |> removeFeedback sel1 feedback fieldState1
                            |> removeFeedback sel2 feedback fieldState2
                            |> removeFeedback sel3 feedback fieldState3
    in
    { formBuilder
        | validators =
            formBuilder.validators
                |> updateAtIndex (getIndex sel1 formBuilder) v
                |> updateAtIndex (getIndex sel2 formBuilder) v
                |> updateAtIndex (getIndex sel3 formBuilder) v
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
