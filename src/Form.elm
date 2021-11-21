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
import Process
import Task
import Time



-- TYPES


type Form form
    = Form form


type State state
    = State state



-- FORM CONFIG


type Builder a b c d e f g h fields element msg
    = Builder
        { reverseSize : a
        , anotherReverseSize : b
        , stateSize : c
        , validateSize : d
        , collectResultsSize : e
        , renderSize : f
        , collectElementsSize : g
        , fieldStateFocuser : h
        , fieldCount : Int
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
        , renderSize = identity
        , fieldStateFocuser = identity
        , fieldCount = 0
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
    Builder
        { reverseSize = bdr.reverseSize >> reverseSize1
        , anotherReverseSize = bdr.anotherReverseSize >> anotherReverseSize1
        , stateSize = bdr.stateSize >> stateSize1
        , validateSize = bdr.validateSize >> validateSize1
        , collectResultsSize = bdr.collectResultsSize >> collectResultsSize1
        , collectElementsSize = bdr.collectElementsSize >> collectElementsSize1
        , renderSize = bdr.renderSize >> renderSize1
        , fieldStateFocuser = bdr.fieldStateFocuser >> fieldStateFocuser1
        , fieldCount = bdr.fieldCount + 1
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
        , collectResultsSize = bdr.collectResultsSize
        , collectElementsSize = bdr.collectElementsSize
        , fieldStateFocuser = bdr.fieldStateFocuser
        , fieldCount = bdr.fieldCount
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
    { init = State fieldsState
    , submit = submit bdr.validateSize bdr.collectResultsSize bdr.anotherReverseSize form_
    , updateField = updateField bdr.fieldStateFocuser form_
    , viewFields = renderAll bdr.renderSize form_
    , view = view config bdr.renderSize bdr.collectElementsSize form_
    }



-- INDEXES FOR SETTING FIELDS


fieldStateFocuser1 : (b -> y) -> ( { a | focused : Bool }, b ) -> ( { a | focused : Bool }, y )
fieldStateFocuser1 =
    Tuple.mapBoth (\fs -> { fs | focused = False })


fieldAndFieldStateGetter0 : a -> a
fieldAndFieldStateGetter0 =
    identity


fieldAndFieldStateGetter1 : (b -> a -> c) -> ( d, b ) -> ( e, a ) -> c
fieldAndFieldStateGetter1 next ( _, restFields ) ( _, restFieldStates ) =
    next restFields restFieldStates


getFieldAndFieldState : ((( a, b ) -> ( c, d ) -> ( a, c )) -> e -> f -> g) -> e -> f -> g
getFieldAndFieldState getter form_ state_ =
    getter
        (\( field_, _ ) ( fieldState, _ ) -> ( field_, fieldState ))
        form_
        state_


fieldStateSetter0 : a -> a
fieldStateSetter0 =
    identity


fieldStateSetter1 : (b -> y) -> ( a, b ) -> ( a, y )
fieldStateSetter1 mapRest ( this, rest ) =
    Tuple.mapBoth identity mapRest ( this, rest )


i0 : a -> a
i0 =
    identity


i1 =
    compose ( fieldStateSetter1, fieldAndFieldStateGetter1 )


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


updateField fieldStateFocuser (Form form_) index wrappedDelta (State fieldStates) =
    let
        ( fieldStateSetter, fieldAndFieldStateGetter ) =
            index ( fieldStateSetter0, fieldAndFieldStateGetter0 )

        ( Field field_, fieldState ) =
            getFieldAndFieldState fieldAndFieldStateGetter form_ fieldStates

        updateState fs =
            State
                (fieldStateSetter
                    (Tuple.mapBoth (\_ -> fs) identity)
                    fieldStates
                )

        unchanged =
            ( State fieldStates, Cmd.none )
    in
    case wrappedDelta of
        Field.Focused ->
            let
                newFieldStates =
                    fieldStateFocuser (\() -> ()) fieldStates
                        |> fieldStateSetter (Tuple.mapBoth (\_ -> { fieldState | focused = True }) identity)
            in
            ( State newFieldStates
            , Cmd.none
            )

        Field.UpdateInput ctx delta ->
            ( updateState
                { fieldState | input = field_.updater delta fieldState.input }
            , Task.perform
                (\time -> field_.deltaMsg (Field.TransitionToChanging ctx time))
                Time.now
            )

        Field.TransitionToChanging ctx time ->
            ( updateState
                { fieldState | status = Field.Changing_ time }
            , Task.perform
                (\() -> field_.deltaMsg <| Field.TransitionToLoading ctx time)
                (Process.sleep ctx.debounce)
            )

        Field.TransitionToLoading ctx time ->
            case fieldState.status of
                Field.Changing_ lastTouched ->
                    if time == lastTouched then
                        ( updateState
                            { fieldState | status = Field.Loading_ }
                        , field_.loadCmd
                            |> Dict.get ctx.cmdName
                            |> Maybe.map (\toCmd -> toCmd fieldState.input)
                            |> Maybe.withDefault (Task.perform (\() -> field_.deltaMsg Field.TransitionToParsing) (Process.sleep 0))
                        )

                    else
                        unchanged

                _ ->
                    unchanged

        Field.ExecuteLoading delta ->
            ( updateState
                { fieldState | input = field_.updater delta fieldState.input }
            , Task.perform
                (\() -> field_.deltaMsg Field.TransitionToParsing)
                (Process.sleep 0)
            )

        Field.TransitionToParsing ->
            ( updateState
                { fieldState | status = Field.Parsing_ }
            , Task.perform
                (\() -> field_.deltaMsg Field.ExecuteParsing)
                (Process.sleep 20)
              {- this 20ms delay is a hack; we need to wait until the next
                 AnimationFrame before starting the parsing step, otherwise the
                 UI will lock up before we can show the "parsing" icon
              -}
            )

        Field.ExecuteParsing ->
            ( updateState
                (parseAndValidate (Field field_) fieldState)
            , Task.perform
                (\() -> field_.deltaMsg Field.TransitionToIdle)
                (Process.sleep 0)
            )

        Field.TransitionToIdle ->
            ( updateState
                { fieldState | status = Field.Idle_ }
            , Cmd.none
            )



-- EXTRACTING STATE


stateSize1 : (restFields -> restFieldStates) -> ( Field input delta output element msg, restFields ) -> ( Field.State input output, restFieldStates )
stateSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> state) -> Form form -> State state -> State state
validateAll size (Form form_) (State state_) =
    State (size (\() () -> ()) form_ state_)


validateSize1 :
    (rest0 -> rest1 -> rest2)
    -> ( Field input delta output element msg, rest0 )
    -> ( Field.State input output, rest1 )
    -> ( Field.State input output, rest2 )
validateSize1 next form_ state_ =
    Internals.mapBoth2
        (\field fieldState ->
            case fieldState.status of
                Field.Intact_ ->
                    parseAndValidate field { fieldState | status = Field.Idle_ }

                _ ->
                    fieldState
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



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


collectResults size (State state_) =
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



-- SUBMITTING A FORM


submit :
    ((() -> () -> ()) -> form -> state -> state)
    -> ((a -> a) -> ( Result error (), state ) -> ( Result c d, e ))
    -> ((f -> f) -> ( (), d ) -> ( g, h ))
    -> Form form
    -> State state
    -> Result (State state) g
submit validateSize collectResultsSize reverseSize form_ state_ =
    let
        newState =
            validateAll validateSize form_ state_
    in
    newState
        |> collectResults collectResultsSize
        |> Result.map (reverseTuple reverseSize)
        |> Result.mapError (\_ -> newState)



-- CONVERTING TO ELEMENTS


renderAll :
    ((b -> () -> () -> ()) -> form -> state -> element)
    -> Form form
    -> State state
    -> element
renderAll size (Form form_) (State state_) =
    size (\_ () () -> ()) form_ state_


renderSize1 :
    (rest0 -> rest1 -> rest2)
    -> ( Field input delta output element msg, rest0 )
    -> ( Field.State input output, rest1 )
    -> ( element, rest2 )
renderSize1 next form_ state_ =
    Internals.mapBoth2
        (\(Field { renderer, deltaMsg, id, label }) { input, validated, status, focused } ->
            renderer
                { input = input
                , status =
                    case status of
                        Field.Intact_ ->
                            Field.Intact

                        Field.Changing_ _ ->
                            Field.Changing

                        Field.Idle_ ->
                            Field.Idle

                        Field.Loading_ ->
                            Field.Loading

                        Field.Parsing_ ->
                            Field.Parsing
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
                , delta = \delta -> Field.UpdateInput { debounce = 0, cmdName = "" } delta |> deltaMsg
                , effectfulDelta = \eff delta -> Field.UpdateInput { debounce = 0, cmdName = eff } delta |> deltaMsg
                , debouncedDelta = \db delta -> Field.UpdateInput { debounce = db, cmdName = "" } delta |> deltaMsg
                , debouncedEffectfulDelta = \db eff delta -> Field.UpdateInput { debounce = db, cmdName = eff } delta |> deltaMsg
                , focusMsg = deltaMsg Field.Focused
                , focused = focused
                , id = id
                , label = label
                }
        )
        next
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
        ((b -> () -> () -> ())
         -> form
         -> state
         -> restElements
        )
    -> ((h -> h) -> ( List element, restElements ) -> ( List element, restElements2 ))
    -> Form form
    -> State state
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
