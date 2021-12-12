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
    )

import Field exposing (Field(..))
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
        }



-- CREATING FORMS


form : Builder (b -> b) (c -> c) (d -> d) (e -> e) (f -> f) (g -> g) (h -> h) (i -> i) () element msg
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
        }


done (Builder bdr) =
    let
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
    , viewList = view bdr.renderSize bdr.collectElementsSize form_
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

        Field.UpdateInput delta ->
            let
                ( newInput, cmd, opts ) =
                    field_.updater delta fieldState.input

                ( debounce, shouldValidate ) =
                    List.foldl
                        (\opt ( d, sv ) ->
                            case opt of
                                Field.Debounce millis ->
                                    ( millis, sv )

                                Field.DontValidate ->
                                    ( d, False )
                        )
                        ( 0, True )
                        opts

                newFieldState =
                    { fieldState | input = newInput }

                shouldSendCmd =
                    cmd /= Cmd.none

                ctx =
                    { debounce = debounce
                    , shouldSendCmd = shouldSendCmd
                    , shouldValidate = shouldValidate
                    , delta = delta
                    }
            in
            if debounce /= 0 then
                --transition to debouncing
                ( updateState newFieldState
                , Task.perform (\time -> field_.deltaMsg (Field.StartDebouncing ctx time)) Time.now
                )

            else if shouldSendCmd then
                -- send the Cmd!
                ( updateState { newFieldState | status = Field.Loading_ }
                , Cmd.map (field_.deltaMsg << Field.ExecuteLoading ctx) cmd
                )

            else if shouldValidate then
                -- transition to parsing
                ( updateState { newFieldState | status = Field.Parsing_ }
                , Task.perform (\() -> field_.deltaMsg Field.ExecuteParsing) (Process.sleep 20)
                )

            else
                -- transition to idle
                ( updateState { newFieldState | status = Field.Idle_ }
                , Cmd.none
                )

        Field.StartDebouncing ctx time ->
            ( updateState { fieldState | status = Field.Debouncing_ time }
            , Task.perform
                (\() -> field_.deltaMsg <| Field.CheckDebouncingTimeout ctx time)
                (Process.sleep ctx.debounce)
            )

        Field.CheckDebouncingTimeout ctx time ->
            case fieldState.status of
                Field.Debouncing_ lastTouched ->
                    if time == lastTouched then
                        if ctx.shouldSendCmd then
                            let
                                ( _, cmd, _ ) =
                                    field_.updater ctx.delta fieldState.input
                            in
                            -- send the Cmd!
                            ( updateState { fieldState | status = Field.Loading_ }
                            , Cmd.map (field_.deltaMsg << Field.ExecuteLoading ctx) cmd
                            )

                        else if ctx.shouldValidate then
                            -- transition to parsing
                            ( updateState { fieldState | status = Field.Parsing_ }
                            , Task.perform (\() -> field_.deltaMsg Field.ExecuteParsing) (Process.sleep 20)
                            )

                        else
                            -- transition to idle
                            ( updateState { fieldState | status = Field.Idle_ }
                            , Cmd.none
                            )

                    else
                        unchanged

                _ ->
                    unchanged

        Field.ExecuteLoading ctx newDelta ->
            let
                ( newInput, newCmd, _ ) =
                    field_.updater newDelta fieldState.input

                newFieldState =
                    { fieldState | input = newInput }
            in
            if newCmd /= Cmd.none then
                -- send the next Cmd!
                ( updateState { newFieldState | status = Field.Loading_ }
                , Cmd.map (field_.deltaMsg << Field.ExecuteLoading ctx) newCmd
                )

            else if ctx.shouldValidate then
                -- transition to parsing
                ( updateState { newFieldState | status = Field.Parsing_ }
                , Task.perform (\() -> field_.deltaMsg Field.ExecuteParsing) (Process.sleep 20)
                )

            else
                -- transition to idle
                ( updateState { newFieldState | status = Field.Idle_ }
                , Cmd.none
                )

        Field.ExecuteParsing ->
            ( { fieldState | status = Field.Idle_ }
                |> parseAndValidate (Field field_)
                |> updateState
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
    mapBoth2
        (\field fieldState ->
            case fieldState.validated of
                Field.Intact ->
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
            case
                fieldState.input
                    |> parser
                    |> Result.mapError List.singleton
                    |> Result.andThen
                        (\parsed ->
                            validators
                                |> List.map (\v -> v parsed)
                                |> accumulateErrors parsed
                        )
            of
                Ok ( output, feedback ) ->
                    Field.Passed output feedback

                Err feedback ->
                    Field.Failed feedback
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
                Field.Passed okF _ ->
                    next ( Ok ( okF, tuple ), rst )

                Field.Failed e ->
                    next ( Err e, rst )

                Field.Intact ->
                    next ( Err [], rst )

        Err es ->
            case fst.validated of
                Field.Passed _ _ ->
                    next ( Err es, rst )

                Field.Failed e ->
                    next ( Err (List.concat [ e, es ]), rst )

                Field.Intact ->
                    next ( Err es, rst )



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
    mapBoth2
        (\(Field { renderer, deltaMsg, id, label }) { input, validated, status, focused } ->
            renderer
                { input = input
                , status =
                    case status of
                        Field.Debouncing_ _ ->
                            Field.Debouncing

                        Field.Idle_ ->
                            Field.Idle

                        Field.Loading_ ->
                            Field.Loading

                        Field.Parsing_ ->
                            Field.Parsing
                , parsed = validated
                , delta = \delta -> Field.UpdateInput delta |> deltaMsg
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
    ((b -> () -> () -> ())
     -> form
     -> state
     -> restElements
    )
    -> ((h -> h) -> ( List element, restElements ) -> ( List element, restElements2 ))
    -> Form form
    -> State state
    -> List element
view renderSize collectElementsSize form_ state_ =
    renderAll renderSize form_ state_
        |> collectElements collectElementsSize
        |> List.reverse


mapBoth2 : (this0 -> this1 -> this2) -> (rest0 -> rest1 -> rest2) -> ( this0, rest0 ) -> ( this1, rest1 ) -> ( this2, rest2 )
mapBoth2 mapThis mapRest ( this0, rest0 ) ( this1, rest1 ) =
    ( mapThis this0 this1, mapRest rest0 rest1 )
