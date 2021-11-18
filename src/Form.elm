module Form exposing
    ( Form
    , InternalMsg
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


type InternalMsg msg
    = Focused Int
    | CmdRequested String Int
    | ChangeDetected Int Float String Time.Posix
    | ChangeCompleted Int String Time.Posix



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
        , formMsg : InternalMsg msg -> msg
        }



-- CREATING FORMS


form : (InternalMsg msg -> msg) -> Builder (b -> b) (c -> c) (d -> d) (e -> e) (f -> f) (g -> g) (h -> h) (i -> i) () (Element msg) msg
form formMsg =
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
        , formMsg = formMsg
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
        , formMsg = bdr.formMsg
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
        , formMsg = bdr.formMsg
        }


withSubmit : msg -> Builder b c d e f g h i fields element msg -> Builder b c d e f g h i fields element msg
withSubmit msg (Builder bdr) =
    Builder { bdr | submitMsg = Just msg }


done (Builder bdr) =
    let
        config =
            { layout = bdr.layout
            , submitRenderer = bdr.submitRenderer
            , formMsg = bdr.formMsg
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
    , updateField = updateField bdr.collectCmdsSize bdr.formMsg form_
    , updateForm = updateForm bdr.formMsg
    , viewFields = renderAll config bdr.renderSize form_
    , view = view config bdr.renderSize bdr.collectElementsSize form_
    }



-- INDEXES FOR SETTING FIELDS


selectField0 : a -> a
selectField0 =
    identity


selectField1 : (b -> a -> rest2) -> ( c, b ) -> ( d, a ) -> ( d, rest2 )
selectField1 mapRest ( this0, rest0 ) ( this1, rest1 ) =
    Internals.mapBoth2 (\_ fieldState -> identity fieldState) mapRest ( this0, rest0 ) ( this1, rest1 )


countField0 : Int -> Int
countField0 =
    (+) 0


countField1 : Int -> Int
countField1 =
    (+) 1


i0 : a -> a
i0 =
    identity


i1 : ( (( c, b ) -> ( d, a ) -> ( d, rest2 )) -> e, Int -> Int ) -> ( (b -> a -> rest2) -> e, Int -> Int )
i1 =
    compose ( selectField1, countField1 )


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


compose : ( a -> b, c -> d ) -> ( b -> e, d -> f ) -> ( a -> e, c -> f )
compose ( a, b ) ( a1, b1 ) =
    ( a >> a1, b >> b1 )



-- SETTING FIELDS


updateForm : (InternalMsg msg -> msg) -> InternalMsg msg -> State state msg -> ( State state msg, Cmd msg )
updateForm formMsg msg (State internalState state) =
    let
        ( internalState2, cmd ) =
            case msg of
                Focused fieldIndex ->
                    ( { internalState
                        | focused = Just fieldIndex
                      }
                    , Cmd.none
                    )

                CmdRequested cmdName fieldIndex ->
                    ( internalState
                    , Dict.get fieldIndex internalState.fields
                        |> Maybe.andThen (.cmd >> Dict.get cmdName)
                        |> Maybe.withDefault Cmd.none
                    )

                ChangeDetected fieldIndex debounce cmd_ time ->
                    Dict.get fieldIndex internalState.fields
                        |> Maybe.map
                            (\field ->
                                ( { internalState
                                    | fields =
                                        Dict.insert fieldIndex
                                            { field | interaction = Changing time }
                                            internalState.fields
                                  }
                                , Task.perform (\() -> formMsg <| ChangeCompleted fieldIndex cmd_ time) (Process.sleep debounce)
                                )
                            )
                        |> Maybe.withDefault ( internalState, Cmd.none )

                ChangeCompleted fieldIndex cmdName time ->
                    Dict.get fieldIndex internalState.fields
                        |> Maybe.map
                            (\s ->
                                case s.interaction of
                                    Changing lastTouched ->
                                        if lastTouched == time then
                                            ( { internalState
                                                | fields =
                                                    Dict.insert fieldIndex
                                                        { s
                                                            | interaction =
                                                                if cmdName == "" then
                                                                    Idle

                                                                else
                                                                    Loading
                                                        }
                                                        internalState.fields
                                              }
                                            , Dict.get cmdName s.cmd
                                                |> Maybe.withDefault Cmd.none
                                            )

                                        else
                                            ( internalState, Cmd.none )

                                    _ ->
                                        ( internalState, Cmd.none )
                            )
                        |> Maybe.withDefault ( internalState, Cmd.none )
    in
    ( State internalState2 state, cmd )


updateField :
    ((a -> ( Dict.Dict String (Cmd msg), (), () ) -> ( Dict.Dict String (Cmd msg), (), () ))
     -> Int
     -> ( Dict.Dict String (Cmd msg), form, state )
     -> ( Dict.Dict String (Cmd msg), form2, state2 )
    )
    -> (InternalMsg msg -> msg)
    -> Form form
    ->
        (( b -> b, Int -> Int )
         ->
            ( (( Field input delta output element msg, fields )
               -> ( Field.State input output, fieldStates )
               -> ( Field.State input output, fieldStates )
              )
              -> form
              -> state
              -> state
            , Int -> Int
            )
        )
    -> Field.Delta delta
    -> State state msg
    -> ( State state msg, Cmd msg )
updateField collectCmdsSize formMsg (Form form_) index (Field.Delta ctx delta) (State internalState fieldStates) =
    let
        ( selectField, countField ) =
            index ( selectField0, countField0 )

        indexOfUpdatedField =
            countField 0

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

        updatedCmd =
            collectCmds indexOfUpdatedField collectCmdsSize form_ newFieldStates

        newInternalState =
            { internalState
                | fields =
                    Dict.update indexOfUpdatedField
                        (Maybe.map (\s -> { s | cmd = updatedCmd, interaction = Idle }))
                        internalState.fields
            }
    in
    ( State newInternalState newFieldStates
    , if ctx.debounce == 0 then
        Dict.get ctx.cmdName updatedCmd |> Maybe.withDefault Cmd.none

      else
        Task.perform (formMsg << ChangeDetected indexOfUpdatedField ctx.debounce ctx.cmdName) Time.now
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


accumulateErrors : a -> List (Maybe Field.Error) -> Result (List Field.Error) a
accumulateErrors a list =
    case List.filterMap identity list of
        [] ->
            Ok a

        errors ->
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
-- collectResults : ((a -> a) -> ( Result error (), results ) -> ( collectedResult, d )) -> results -> collectedResult


collectResults size (State _ state_) =
    size identity ( Ok (), state_ )
        |> Tuple.first



-- collectResultsSize1 :
--     (( Result (List Field.Error) ( value, b ), restResults ) -> d)
--     -> ( Result (List Field.Error) b, ( Result (List Field.Error) value, restResults ) )
--     -> d


collectResultsSize1 next ( s, ( fst, rst ) ) =
    case s of
        Ok tuple ->
            case fst.validated of
                Ok okF ->
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
    config
    -> ((b -> c -> () -> () -> ()) -> config -> InternalState msg -> form -> state -> element)
    -> Form form
    -> State state msg
    -> element
renderAll config size (Form form_) (State internalState state_) =
    size (\_ _ () () -> ()) config internalState form_ state_


renderSize1 :
    ({ a | formMsg : InternalMsg msg -> msg } -> InternalState msg -> rest0 -> rest1 -> rest2)
    -> { a | formMsg : InternalMsg msg -> msg }
    -> InternalState msg
    -> ( Field input delta output element msg, rest0 )
    -> ( Field.State input output, rest1 )
    -> ( element, rest2 )
renderSize1 next config internalState form_ state_ =
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
                , focused = internalState.focused == Just index
                , requestCmdMsg = \cmdName -> config.formMsg (CmdRequested cmdName index)
                , focusMsg = config.formMsg (Focused index)
                , delta = \delta -> Field.Delta { debounce = 0, cmdName = "" } delta |> deltaMsg
                , effectfulDelta = \eff delta -> Field.Delta { debounce = 0, cmdName = eff } delta |> deltaMsg
                , debouncedDelta = \db delta -> Field.Delta { debounce = db, cmdName = "" } delta |> deltaMsg
                , debouncedEffectfulDelta = \db eff delta -> Field.Delta { debounce = db, cmdName = eff } delta |> deltaMsg
                , parsed = validated
                , id = id
                , label = label
                }
        )
        (next config internalState)
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
        ((f -> g -> () -> () -> ())
         -> { a | submitMsg : Maybe msg, submitRenderer : msg -> element, layout : List element -> element }
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
    renderAll config renderSize form_ state_
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
