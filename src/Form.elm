module Form exposing
    ( Form
    , State
    , done
    , form
    , i0
    , i1
    , i10
    , i2
    , i3
    , i4
    , i5
    , i6
    , i7
    , i8
    , i9
    , withField
    , withRenderer
    , withSubmit
    )

import Dict
import Element exposing (Element)
import Element.Border
import Element.Input
import Field exposing (Field(..))
import Internals
import Json.Decode exposing (index)
import Process
import Task
import Time



-- TYPES


type Form form
    = Form form


type State state msg
    = State (InternalState msg) state


type alias InternalState msg =
    { fields : Dict.Dict Int (InternalFieldState msg)
    , focused : Maybe Int
    }


type alias InternalFieldState msg =
    { interaction : Interaction
    , cmd : Dict.Dict String (Cmd msg)
    }


type Interaction
    = Intact
    | Changing Time.Posix
    | Loading
    | Idle



-- FORM CONFIG


type Builder a b c d e f g h fields element msg
    = Builder
        { reverseSize : a
        , anotherReverseSize : b
        , stateSize : c
        , validateSize : d
        , renderSize : f
        , collectResultsSize : e
        , collectElementsSize : g
        , collectCmdsSize : h
        , fieldCount : Int
        , formState : InternalState msg
        , fields : fields
        , layout : List element -> element
        , submitMsg : Maybe msg
        , submitRenderer : msg -> element
        }



-- CREATING FORMS


form : Builder (b -> b) (c -> c) (d -> d) (e -> e) (f -> f) (g -> g) (h -> h) (i -> i) () (Element msg) msg
form =
    Builder
        { reverseSize = identity
        , anotherReverseSize = identity
        , stateSize = identity
        , validateSize = identity
        , collectResultsSize = identity
        , collectElementsSize = identity
        , collectCmdsSize = identity
        , renderSize = identity
        , fieldCount = 0
        , formState = { fields = Dict.empty, focused = Nothing }
        , fields = ()
        , layout =
            Element.column
                [ Element.spacing 10
                , Element.padding 10
                , Element.centerX
                ]
        , submitMsg = Nothing
        , submitRenderer =
            \msg ->
                Element.Input.button
                    [ Element.centerX
                    , Element.padding 10
                    , Element.Border.width 1
                    , Element.Border.rounded 5
                    ]
                    { label = Element.text "Submit", onPress = Just msg }
        }


withField (Field fld) (Builder bdr) =
    let
        formState =
            bdr.formState
    in
    Builder
        { reverseSize = bdr.reverseSize >> reverseSize1
        , anotherReverseSize = bdr.anotherReverseSize >> anotherReverseSize1
        , stateSize = bdr.stateSize >> stateSize1
        , validateSize = bdr.validateSize >> validateSize1
        , collectCmdsSize = bdr.collectCmdsSize >> collectCmdSize1
        , collectResultsSize = bdr.collectResultsSize >> collectResultsSize1
        , collectElementsSize = bdr.collectElementsSize >> collectElementsSize1
        , renderSize = bdr.renderSize >> renderSize1
        , fieldCount = bdr.fieldCount + 1
        , formState =
            { formState
                | fields =
                    Dict.insert bdr.fieldCount
                        { cmd = Dict.empty
                        , interaction = Intact
                        }
                        formState.fields
            }
        , fields = ( Field { fld | index = bdr.fieldCount }, bdr.fields )
        , layout = bdr.layout
        , submitMsg = bdr.submitMsg
        , submitRenderer = bdr.submitRenderer
        }


withRenderer :
    { layout : List element2 -> element2, submit : msg -> element2 }
    -> Builder b c d e f g h i fields element msg
    -> Builder b c d e f g h i fields element2 msg
withRenderer args (Builder bdr) =
    Builder
        { reverseSize = bdr.reverseSize
        , anotherReverseSize = bdr.anotherReverseSize
        , stateSize = bdr.stateSize
        , validateSize = bdr.validateSize
        , renderSize = bdr.renderSize
        , collectCmdsSize = bdr.collectCmdsSize
        , collectResultsSize = bdr.collectResultsSize
        , collectElementsSize = bdr.collectElementsSize
        , fieldCount = bdr.fieldCount
        , formState = bdr.formState
        , fields = bdr.fields
        , layout = args.layout
        , submitRenderer = args.submit
        , submitMsg = bdr.submitMsg
        }


withSubmit : msg -> Builder b c d e f g h i fields element msg -> Builder b c d e f g h i fields element msg
withSubmit msg (Builder bdr) =
    Builder { bdr | submitMsg = Just msg }


done (Builder bdr) =
    let
        config =
            { layout = bdr.layout
            , submitRenderer = bdr.submitRenderer
            , submitMsg = bdr.submitMsg
            }

        reversedFields =
            reverseTuple bdr.reverseSize bdr.fields

        form_ =
            Form reversedFields

        fieldsState =
            bdr.stateSize (\() -> ()) reversedFields
    in
    { init = State bdr.formState fieldsState
    , submit = submit bdr.validateSize bdr.collectResultsSize bdr.anotherReverseSize form_
    , form = form_
    , updateField = updateField bdr.collectCmdsSize form_
    , viewFields = renderAll bdr.renderSize form_
    , view = view config bdr.renderSize bdr.collectElementsSize form_
    }



-- INDEXES FOR SETTING FIELDS


fieldGetter0 : a -> a
fieldGetter0 =
    identity


fieldGetter1 : (( a, b ) -> c) -> ( d, ( a, b ) ) -> c
fieldGetter1 next ( _, ( currentField, restFields ) ) =
    next ( currentField, restFields )


getField : ((( a, b ) -> ( a, b )) -> c -> ( d, e )) -> c -> d
getField getter form_ =
    getter
        (\( field_, rest ) -> ( field_, rest ))
        form_
        |> Tuple.first


getFieldState : ((( a, b ) -> ( a, b )) -> c -> ( d, e )) -> c -> d
getFieldState getter form_ =
    getter
        (\( field_, rest ) -> ( field_, rest ))
        form_
        |> Tuple.first


fieldMapper0 : a -> a
fieldMapper0 =
    identity


fieldMapper1 : (b -> a -> rest2) -> ( c, b ) -> ( d, a ) -> ( d, rest2 )
fieldMapper1 mapRest ( this0, rest0 ) ( this1, rest1 ) =
    Internals.mapBoth2 (\_ fieldState -> identity fieldState) mapRest ( this0, rest0 ) ( this1, rest1 )


fieldCounter0 : Int -> Int
fieldCounter0 =
    (+) 0


fieldCounter1 : Int -> Int
fieldCounter1 =
    (+) 1


i0 : a -> a
i0 =
    identity


i1 =
    compose ( fieldMapper1, fieldCounter1, fieldGetter1 )


i2 =
    i1 >> i1


i3 =
    i2 >> i1


i4 =
    i3 >> i1


i5 =
    i4 >> i1


i6 =
    i5 >> i1


i7 =
    i6 >> i1


i8 =
    i7 >> i1


i9 =
    i8 >> i1


i10 =
    i9 >> i1


compose : ( a -> b, c -> d, e -> f ) -> ( b -> g, d -> h, f -> i ) -> ( a -> g, c -> h, e -> i )
compose ( a, b, c ) ( a1, b1, c1 ) =
    ( a >> a1, b >> b1, c >> c1 )



-- SETTING FIELDS


updateField :
    ((b -> ( Dict.Dict String (Cmd msg), (), () ) -> ( Dict.Dict String (Cmd msg), (), () ))
     -> Int
     -> ( Dict.Dict String (Cmd msg), form, state )
     -> ( Dict.Dict String (Cmd msg), form2, state2 )
    )
    -> Form form
    ->
        (( c -> c, Int -> Int, d -> d )
         ->
            ( (( Field input delta output element msg, i )
               -> ( Field.State input output, k )
               -> ( Field.State input output, k )
              )
              -> form
              -> state
              -> state
            , Int -> Int
            , (( Field input delta output element msg, p ) -> ( Field input delta output element msg, p ))
              -> form
              -> ( Field input delta output element msg, u )
            )
        )
    -> Field.Delta delta
    -> State state msg
    -> ( State state msg, Cmd msg )
updateField collectCmdsSize (Form form_) index wrappedDelta (State internalState fieldStates) =
    let
        ( selectField, countField, fieldGetter ) =
            index ( fieldMapper0, fieldCounter0, fieldGetter0 )

        indexOfUpdatedField =
            countField 0

        (Field field_) =
            getField fieldGetter form_
    in
    case wrappedDelta of
        Field.Focused ->
            ( State
                { internalState | focused = Just indexOfUpdatedField }
                fieldStates
            , Cmd.none
            )

        Field.Delta ctx delta ->
            let
                newFieldStates =
                    selectField
                        (Internals.mapBoth2
                            (\((Field f) as field) fieldState ->
                                { fieldState | input = f.updater delta fieldState.input }
                                    |> parseAndValidate field
                            )
                            (\_ restFieldStates -> restFieldStates)
                        )
                        form_
                        fieldStates

                cmdsDict =
                    collectCmds indexOfUpdatedField collectCmdsSize form_ newFieldStates

                newInternalState =
                    { internalState
                        | fields =
                            Dict.update indexOfUpdatedField
                                (Maybe.map (\s -> { s | cmd = cmdsDict, interaction = Idle }))
                                internalState.fields
                    }
            in
            ( State newInternalState newFieldStates
            , if ctx.debounce == 0 then
                -- don't debounce, just send the Cmd straight away (if there is a Cmd)
                Dict.get ctx.cmdName cmdsDict
                    |> Maybe.withDefault Cmd.none

              else
                -- debounce and let the "ChangeDetected" handler deal with sending any Cmds
                Task.perform (field_.deltaMsg << Field.ChangeDetected ctx.debounce ctx.cmdName) Time.now
            )

        Field.ChangeDetected debounce cmd_ time ->
            case Dict.get indexOfUpdatedField internalState.fields of
                Just field ->
                    ( State
                        { internalState
                            | fields =
                                Dict.insert indexOfUpdatedField
                                    { field | interaction = Changing time }
                                    internalState.fields
                        }
                        fieldStates
                    , Task.perform
                        (\() -> field_.deltaMsg <| Field.ChangeCompleted cmd_ time)
                        (Process.sleep debounce)
                    )

                Nothing ->
                    ( State internalState fieldStates, Cmd.none )

        Field.ChangeCompleted cmdName time ->
            case Dict.get indexOfUpdatedField internalState.fields of
                Just s ->
                    case s.interaction of
                        Changing lastTouched ->
                            if lastTouched == time then
                                ( State
                                    { internalState
                                        | fields =
                                            Dict.insert indexOfUpdatedField
                                                { s
                                                    | interaction =
                                                        if cmdName == "" then
                                                            Idle

                                                        else
                                                            Loading
                                                }
                                                internalState.fields
                                    }
                                    fieldStates
                                , Dict.get cmdName s.cmd
                                    |> Maybe.withDefault Cmd.none
                                )

                            else
                                ( State internalState fieldStates
                                , Cmd.none
                                )

                        _ ->
                            ( State internalState fieldStates
                            , Cmd.none
                            )

                Nothing ->
                    ( State internalState fieldStates
                    , Cmd.none
                    )



-- EXTRACTING STATE


stateSize1 : (restFields -> restFieldStates) -> ( Field input delta output element msg, restFields ) -> ( Field.State input output, restFieldStates )
stateSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> state) -> Form form -> State state msg -> State state msg
validateAll size (Form form_) (State internalState state_) =
    State internalState (size (\() () -> ()) form_ state_)
        |> Debug.log "only parse & validate fields that are currently Intact - any others will already have been p&v'd"


validateSize1 :
    (rest0 -> rest1 -> rest2)
    -> ( Field input delta output element msg, rest0 )
    -> ( Field.State input output, rest1 )
    -> ( Field.State input output, rest2 )
validateSize1 next form_ state_ =
    Internals.mapBoth2
        (\field fieldState ->
            parseAndValidate field fieldState
        )
        next
        form_
        state_


parseAndValidate : Field input delta output element msg -> Field.State input output -> Field.State input output
parseAndValidate (Field { parser, validators }) fieldState =
    { fieldState
        | validated =
            fieldState.input
                |> parser
                |> Result.mapError List.singleton
                |> Result.andThen
                    (\parsed ->
                        validators
                            |> List.map (\v -> v parsed)
                            |> accumulateErrors parsed
                    )
    }


accumulateErrors : a -> List (Maybe Field.Feedback) -> Result (List Field.Feedback) ( a, List Field.Feedback )
accumulateErrors a list =
    case
        list
            |> List.filterMap identity
            |> List.partition (\{ tag } -> tag == Field.Fail)
    of
        ( [], others ) ->
            Ok ( a, others )

        ( errors, _ ) ->
            Err errors


collectCmds :
    Int
    ->
        ((b -> ( Dict.Dict String (Cmd msg), (), () ) -> ( Dict.Dict String (Cmd msg), (), () ))
         -> Int
         -> ( Dict.Dict String (Cmd msg), form, state )
         -> ( Dict.Dict String (Cmd msg), e, f )
        )
    -> form
    -> state
    -> Dict.Dict String (Cmd msg)
collectCmds fieldIndex size form_ state_ =
    size (\_ ( cmd, (), () ) -> ( cmd, (), () )) fieldIndex ( Dict.empty, form_, state_ )
        |> (\( cmd, _, _ ) -> cmd)


collectCmdSize1 : (Int -> ( Dict.Dict String (Cmd msg), b, a ) -> c) -> Int -> ( Dict.Dict String (Cmd msg), ( Field d delta output element msg, b ), ( { e | input : d }, a ) ) -> c
collectCmdSize1 next fieldIndex ( currentCmd, ( Field field, restFields ), ( { input }, restFieldStates ) ) =
    if fieldIndex == field.index then
        next fieldIndex ( Dict.map (\_ toCmd -> toCmd input) field.loadCmd, restFields, restFieldStates )

    else
        next fieldIndex ( currentCmd, restFields, restFieldStates )



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


collectResults size (State _ state_) =
    size identity ( Ok (), state_ )
        |> Tuple.first


collectResultsSize1 next ( s, ( fst, rst ) ) =
    case s of
        Ok tuple ->
            case fst.validated of
                Ok ( okF, _ ) ->
                    next ( Ok ( okF, tuple ), rst )

                Err e ->
                    next ( Err e, rst )

        Err es ->
            case fst.validated of
                Ok _ ->
                    next ( Err es, rst )

                Err e ->
                    next ( Err (List.concat [ e, es ]), rst )



-- REVERSING TUPLES


reverseTuple : ((a -> a) -> ( (), b ) -> ( c, d )) -> b -> c
reverseTuple size results =
    size identity ( (), results )
        |> Tuple.first


reverseSize1 : (( ( a, b ), c ) -> d) -> ( b, ( a, c ) ) -> d
reverseSize1 next ( s, ( fst, rest ) ) =
    next ( ( fst, s ), rest )


anotherReverseSize1 : (( ( a, b ), c ) -> d) -> ( b, ( a, c ) ) -> d
anotherReverseSize1 next ( s, ( fst, rest ) ) =
    next ( ( fst, s ), rest )



-- TOUCHING ALL FIELDS


touchAll : State state msg -> State state msg
touchAll (State internalState state_) =
    State
        { internalState
            | fields =
                Dict.map
                    (\_ v ->
                        { v
                            | interaction =
                                if v.interaction == Intact then
                                    Idle

                                else
                                    v.interaction
                        }
                    )
                    internalState.fields
        }
        state_



-- SUBMITTING A FORM


submit :
    ((() -> () -> ()) -> form -> state -> state)
    -> ((a -> a) -> ( Result error (), state ) -> ( Result c d, e ))
    -> ((f -> f) -> ( (), d ) -> ( g, h ))
    -> Form form
    -> State state msg
    -> Result (State state msg) g
submit validateSize collectResultsSize reverseSize form_ state_ =
    let
        newState =
            validateAll validateSize form_ state_
    in
    newState
        |> collectResults collectResultsSize
        |> Result.map (reverseTuple reverseSize)
        |> Result.mapError (\_ -> touchAll newState)



-- CONVERTING TO ELEMENTS


renderAll :
    ((b -> () -> () -> ()) -> InternalState msg -> form -> state -> element)
    -> Form form
    -> State state msg
    -> element
renderAll size (Form form_) (State internalState state_) =
    size (\_ () () -> ()) internalState form_ state_


renderSize1 :
    (InternalState msg -> rest0 -> rest1 -> rest2)
    -> InternalState msg
    -> ( Field input delta output element msg, rest0 )
    -> ( Field.State input output, rest1 )
    -> ( element, rest2 )
renderSize1 next internalState form_ state_ =
    Internals.mapBoth2
        (\(Field { index, renderer, deltaMsg, id, label }) { input, validated } ->
            renderer
                { input = input
                , status =
                    Dict.get index internalState.fields
                        |> Maybe.map .interaction
                        |> Maybe.map
                            (\i ->
                                case i of
                                    Intact ->
                                        Field.Intact

                                    Changing _ ->
                                        Field.Changing

                                    Idle ->
                                        Field.Idle

                                    Loading ->
                                        Field.Loading
                            )
                        |> Maybe.withDefault Field.Intact
                , parsed =
                    validated
                        |> Result.toMaybe
                        |> Maybe.map Tuple.first
                , feedback =
                    case validated of
                        Ok ( _, feedback ) ->
                            feedback

                        Err feedback ->
                            feedback
                , delta = \delta -> Field.Delta { debounce = 0, cmdName = "" } delta |> deltaMsg
                , effectfulDelta = \eff delta -> Field.Delta { debounce = 0, cmdName = eff } delta |> deltaMsg
                , debouncedDelta = \db delta -> Field.Delta { debounce = db, cmdName = "" } delta |> deltaMsg
                , debouncedEffectfulDelta = \db eff delta -> Field.Delta { debounce = db, cmdName = eff } delta |> deltaMsg
                , focusMsg = deltaMsg Field.Focused
                , focused = internalState.focused == Just index
                , id = id
                , label = label
                }
        )
        (next internalState)
        form_
        state_


collectElements : ((a -> a) -> ( List element, restElements ) -> ( d, e )) -> restElements -> d
collectElements size elements =
    size identity ( [], elements )
        |> Tuple.first


collectElementsSize1 : (( List element, restElements ) -> next) -> ( List element, ( element, restElements ) ) -> next
collectElementsSize1 next ( s, ( fst, rst ) ) =
    next ( fst :: s, rst )


view :
    { a | submitMsg : Maybe msg, submitRenderer : msg -> element, layout : List element -> element }
    ->
        ((f -> () -> () -> ())
         -> InternalState msg
         -> form
         -> state
         -> restElements
        )
    -> ((h -> h) -> ( List element, restElements ) -> ( List element, restElements2 ))
    -> Form form
    -> State state msg
    -> element
view config renderSize collectElementsSize form_ state_ =
    renderAll renderSize form_ state_
        |> collectElements collectElementsSize
        |> (\list ->
                case config.submitMsg of
                    Just msg ->
                        config.submitRenderer msg :: list

                    Nothing ->
                        list
           )
        |> List.reverse
        |> config.layout
