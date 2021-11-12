module Form exposing
    ( Form
    , Index
    , InternalMsg
    , State
    , collectCmdSize1
    , collectCmds
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
    { fields :
        Dict.Dict
            Int
            { touched : Bool
            , lastTouched : Maybe Time.Posix
            , cmd : Cmd msg
            , typingTimeout : Float
            }
    , focused : Maybe Int
    }


type alias Index input delta output element msg restFields restFieldStates form state =
    (( Field input delta output element msg, restFields )
     -> ( Field.State input, restFieldStates )
     -> ( Field.State input, restFieldStates )
    )
    -> form
    -> state
    -> state


type InternalMsg
    = Focused Int
    | CmdRequested Int
    | TypingDetected Int Time.Posix
    | TypingTimedOut Int Time.Posix



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
        , formMsg : InternalMsg -> msg
        }



-- CREATING FORMS


form : (InternalMsg -> msg) -> Builder (b -> b) (c -> c) (d -> d) (e -> e) (f -> f) (g -> g) (h -> h) (i -> i) () (Element msg) msg
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
                , Element.Border.width 1
                , Element.Border.color (Element.rgb255 200 200 200)
                , Element.Border.rounded 5
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
                        { touched = False
                        , lastTouched = Nothing
                        , cmd = Cmd.none
                        , typingTimeout = fld.typingTimeout
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
    , viewFields = viewElements config bdr.validateSize bdr.renderSize form_
    , view = view config bdr.validateSize bdr.renderSize bdr.collectElementsSize form_
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


i1 : ( (( c, b ) -> ( d, a ) -> ( d, rest2 )) -> e, Int -> f ) -> ( (b -> a -> rest2) -> e, Int -> f )
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


updateForm : (InternalMsg -> msg) -> InternalMsg -> State state msg -> ( State state msg, Cmd msg )
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

                CmdRequested fieldIndex ->
                    ( internalState
                    , Dict.get fieldIndex internalState.fields
                        |> Maybe.map .cmd
                        |> Maybe.withDefault Cmd.none
                    )

                TypingDetected f time ->
                    Maybe.map
                        (\field ->
                            ( { internalState | fields = Dict.insert f { field | lastTouched = Just time } internalState.fields }
                            , Task.perform (\() -> formMsg <| TypingTimedOut f time) (Process.sleep field.typingTimeout)
                            )
                        )
                        (Dict.get f internalState.fields)
                        |> Maybe.withDefault ( internalState, Cmd.none )

                TypingTimedOut f time ->
                    Dict.get f internalState.fields
                        |> Maybe.map
                            (\s ->
                                if s.lastTouched == Just time then
                                    ( { internalState | fields = Dict.insert f { s | lastTouched = Nothing } internalState.fields }
                                    , s.cmd
                                    )

                                else
                                    ( internalState, Cmd.none )
                            )
                        |> Maybe.withDefault ( internalState, Cmd.none )
    in
    ( State internalState2 state, cmd )


updateField collectCmdsSize formMsg (Form form_) index delta (State internalState fieldStates) =
    let
        ( selectField, countField ) =
            index ( selectField0, countField0 )

        indexOfUpdatedField =
            countField 0

        newFieldStates =
            selectField
                (Internals.mapBoth2
                    (\(Field f) fieldState -> { fieldState | input = f.updater delta fieldState.input })
                    (\_ fieldState -> fieldState)
                )
                form_
                fieldStates

        updatedCmd =
            collectCmds indexOfUpdatedField collectCmdsSize form_ newFieldStates

        newInternalState =
            { internalState
                | fields =
                    Dict.update indexOfUpdatedField
                        (Maybe.map
                            (\s ->
                                { s
                                    | touched = True
                                    , cmd = updatedCmd
                                }
                            )
                        )
                        internalState.fields
            }
    in
    ( State newInternalState newFieldStates
    , if newFieldStates == fieldStates then
        Cmd.none

      else
        Task.perform (formMsg << TypingDetected indexOfUpdatedField) Time.now
    )



-- EXTRACTING STATE


stateSize1 : (restFields -> restFieldStates) -> ( Field input delta output element msg, restFields ) -> ( Field.State input, restFieldStates )
stateSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state msg -> results
validateAll size (Form form_) (State _ state_) =
    size (\() () -> ()) form_ state_


validateSize1 :
    (rest0 -> rest1 -> rest2)
    -> ( Field input delta output element msg, rest0 )
    -> ( Field.State input, rest1 )
    -> ( Result (List Field.Error) output, rest2 )
validateSize1 next form_ state_ =
    Internals.mapBoth2 parseAndValidate next form_ state_


parseAndValidate : Field input delta output element msg -> Field.State input -> Result (List Field.Error) output
parseAndValidate (Field { parser, validators }) data =
    data.input
        |> parser
        |> Result.mapError List.singleton
        |> Result.andThen
            (\parsed ->
                validators
                    |> List.map (\v -> v parsed)
                    |> accumulateErrors parsed
            )


accumulateErrors : a -> List (Maybe Field.Error) -> Result (List Field.Error) a
accumulateErrors a list =
    case List.filterMap identity list of
        [] ->
            Ok a

        errors ->
            Err errors


collectCmds : Int -> ((b -> ( Cmd msg, (), () ) -> ( Cmd msg, (), () )) -> Int -> ( Cmd msg, form, state ) -> ( Cmd msg, e, f )) -> form -> state -> Cmd msg
collectCmds fieldIndex size form_ state_ =
    size (\_ ( cmd, (), () ) -> ( cmd, (), () )) fieldIndex ( Cmd.none, form_, state_ )
        |> (\( cmd, _, _ ) -> cmd)


collectCmdSize1 : (Int -> ( Cmd msg, b, a ) -> c) -> Int -> ( Cmd msg, ( Field d delta output element msg, b ), ( { e | input : d }, a ) ) -> c
collectCmdSize1 next fieldIndex ( currentCmd, ( Field field, restFields ), ( { input }, restFieldStates ) ) =
    if fieldIndex == field.index then
        next fieldIndex ( field.loadCmd input, restFields, restFieldStates )

    else
        next fieldIndex ( currentCmd, restFields, restFieldStates )



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


collectResults : ((a -> a) -> ( Result error (), results ) -> ( collectedResult, d )) -> results -> collectedResult
collectResults size results =
    size identity ( Ok (), results )
        |> Tuple.first


collectResultsSize1 :
    (( Result (List Field.Error) ( value, b ), restResults ) -> d)
    -> ( Result (List Field.Error) b, ( Result (List Field.Error) value, restResults ) )
    -> d
collectResultsSize1 next ( s, ( fst, rst ) ) =
    case s of
        Ok tuple ->
            case fst of
                Ok okF ->
                    next ( Ok ( okF, tuple ), rst )

                Err e ->
                    next ( Err e, rst )

        Err es ->
            case fst of
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
    State { internalState | fields = Dict.map (\_ v -> { v | touched = True }) internalState.fields } state_



-- SUBMITTING A FORM


submit :
    ((() -> () -> ()) -> form -> state -> b)
    -> ((a -> a) -> ( Result error (), b ) -> ( Result c d, e ))
    -> ((f -> f) -> ( (), d ) -> ( g, h ))
    -> Form form
    -> State state msg
    -> Result (State state msg) g
submit validateSize collectResultsSize reverseSize form_ state_ =
    validateAll validateSize form_ state_
        |> collectResults collectResultsSize
        |> Result.map (reverseTuple reverseSize)
        |> Result.mapError (\_ -> touchAll state_)



-- CONVERTING TO ELEMENTS


renderAll : a -> ((b -> c -> () -> () -> () -> ()) -> a -> InternalState msg -> form -> state -> e -> d) -> Form form -> State state msg -> e -> d
renderAll config size (Form form_) (State internalState state_) results =
    size (\_ _ () () () -> ()) config internalState form_ state_ results


renderSize1 :
    ({ a | formMsg : InternalMsg -> msg } -> InternalState msg -> rest -> rest1 -> rest2 -> rest3)
    -> { a | formMsg : InternalMsg -> msg }
    -> InternalState msg
    -> ( Field input delta output element msg, rest )
    -> ( { e | input : input }, rest1 )
    -> ( Result (List Field.Error) output, rest2 )
    -> ( element, rest3 )
renderSize1 next config internalState form_ state_ results =
    Internals.mapBoth3
        (\(Field { index, renderer, deltaMsg, id, label }) { input } parsed ->
            renderer
                { input = input
                , touched =
                    Dict.get index internalState.fields
                        |> Maybe.map .touched
                        |> Maybe.withDefault False
                , typing =
                    Dict.get index internalState.fields
                        |> Maybe.map (\{ lastTouched } -> lastTouched /= Nothing)
                        |> Maybe.withDefault False
                , focused = internalState.focused == Just index
                , requestCmdMsg = config.formMsg (CmdRequested index)
                , focusMsg = config.formMsg (Focused index)
                , deltaMsg = deltaMsg
                , parsed = parsed
                , id = id
                , label = label
                }
        )
        (next config internalState)
        form_
        state_
        results


viewElements :
    config
    -> ((() -> () -> ()) -> form -> state -> results)
    ->
        ((b -> c -> () -> () -> () -> ())
         -> config
         -> InternalState msg
         -> form
         -> state
         -> results
         -> elements
        )
    -> Form form
    -> State state msg
    -> elements
viewElements config validateSize renderSize form_ state_ =
    state_
        |> validateAll validateSize form_
        |> renderAll config renderSize form_ state_


collectElements : ((a -> a) -> ( List element, restElements ) -> ( d, e )) -> restElements -> d
collectElements size elements =
    size identity ( [], elements )
        |> Tuple.first


collectElementsSize1 : (( List element, restElements ) -> next) -> ( List element, ( element, restElements ) ) -> next
collectElementsSize1 next ( s, ( fst, rst ) ) =
    next ( fst :: s, rst )


view :
    { a | submitMsg : Maybe b, submitRenderer : b -> c, layout : List c -> d }
    -> ((() -> () -> ()) -> form -> state -> e)
    ->
        ((f -> g -> () -> () -> () -> ())
         -> { a | submitMsg : Maybe b, submitRenderer : b -> c, layout : List c -> d }
         -> InternalState msg
         -> form
         -> state
         -> e
         -> restElements
        )
    -> ((h -> h) -> ( List element, restElements ) -> ( List c, i ))
    -> Form form
    -> State state msg
    -> d
view config validateSize renderSize collectElementsSize form_ state_ =
    viewElements config validateSize renderSize form_ state_
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
