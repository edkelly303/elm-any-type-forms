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
        , fieldStateUnfocuser : h
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
        , fieldStateUnfocuser = identity
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
        , fieldStateUnfocuser = bdr.fieldStateUnfocuser >> fieldStateUnfocuser1
        , fields = ( Field fld, bdr.fields )
        }


done (Builder bdr) =
    let
        reversedFields =
            reverseTuple bdr.reverseSize bdr.fields

        form_ =
            Form reversedFields

        fieldStates =
            bdr.stateSize (\() -> ()) reversedFields
    in
    { init = State fieldStates
    , submit = submit bdr.validateSize bdr.collectResultsSize bdr.anotherReverseSize form_
    , initializeField = initializeField form_
    , updateField = updateField bdr.fieldStateUnfocuser form_
    , viewFields = renderAll bdr.renderSize form_
    , viewList = view bdr.renderSize bdr.collectElementsSize form_
    }



-- INDEXES FOR SETTING FIELDS


fieldStateUnfocuser1 : (rest -> rest) -> ( Field.State input output, rest ) -> ( Field.State input output, rest )
fieldStateUnfocuser1 =
    Tuple.mapBoth (\fs -> { fs | focused = False })


fieldAndFieldStateGetter0 : rest -> rest
fieldAndFieldStateGetter0 =
    identity


fieldAndFieldStateGetter1 :
    (restFields -> restFieldStates -> ( Field.Field input2 delta2 output2 element2 msg2, Field.State input2 output2 ))
    -> ( Field.Field input delta output element msg, restFields )
    -> ( Field.State input output, restFieldStates )
    -> ( Field.Field input2 delta2 output2 element2 msg2, Field.State input2 output2 )
fieldAndFieldStateGetter1 next ( _, restFields ) ( _, restFieldStates ) =
    next restFields restFieldStates


getFieldAndFieldState :
    ((( Field.Field input delta output element msg, restFields )
      -> ( Field.State input output, restFieldStates )
      -> ( Field.Field input delta output element msg, Field.State input output )
     )
     -> form
     -> state
     -> ( Field.Field input delta output element msg, Field.State input output )
    )
    -> form
    -> state
    -> ( Field.Field input delta output element msg, Field.State input output )
getFieldAndFieldState getter form_ state_ =
    getter
        (\( field_, _ ) ( fieldState, _ ) -> ( field_, fieldState ))
        form_
        state_


fieldStateSetter0 : rest -> rest
fieldStateSetter0 =
    identity


fieldStateSetter1 :
    (rest -> rest)
    -> ( Field.State input output, rest )
    -> ( Field.State input output, rest )
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


compose : ( fst -> fst2, snd -> snd2 ) -> ( fst2 -> fst3, snd2 -> snd3 ) -> ( fst -> fst3, snd -> snd3 )
compose ( fst1To2, snd1To2 ) ( fst2To3, snd2To3 ) =
    ( fst1To2 >> fst2To3, snd1To2 >> snd2To3 )



-- SETTING FIELDS


initializeField :
    Form form
    ->
        (( rest -> rest, Field.State input output -> Field.State input output )
         ->
            ( (( Field.State input output, rest ) -> ( Field.State input output, rest )) -> state -> state
            , (( Field input delta output element msg, restFields )
               -> ( Field.State input output, restFieldStates )
               -> ( Field input delta output element msg, Field.State input output )
              )
              -> form
              -> state
              -> ( Field input delta output element msg, Field.State input output )
            )
        )
    -> input
    -> State state
    -> State state
initializeField (Form form_) index input (State fieldStates) =
    let
        ( fieldStateSetter, fieldAndFieldStateGetter ) =
            index ( fieldStateSetter0, fieldAndFieldStateGetter0 )

        ( _, fieldState ) =
            getFieldAndFieldState fieldAndFieldStateGetter form_ fieldStates

        newFieldState =
            { fieldState | input = input }
    in
    State
        (fieldStateSetter
            (Tuple.mapBoth (\_ -> newFieldState) identity)
            fieldStates
        )


updateField :
    ((() -> ()) -> fieldStates -> fieldStates)
    -> Form form
    ->
        (( a -> a, b -> b )
         ->
            ( (( Field.State input output, restFieldStates ) -> ( Field.State input output, restFieldStates )) -> fieldStates -> fieldStates
            , (( Field input delta output element msg, restFields )
               -> ( Field.State input output, restFieldStates )
               -> ( Field input delta output element msg, Field.State input output )
              )
              -> form
              -> fieldStates
              -> ( Field input delta output element msg, Field.State input output )
            )
        )
    -> Field.Delta delta
    -> State fieldStates
    -> ( State fieldStates, Cmd msg )
updateField fieldStateUnfocuser (Form form_) index wrappedDelta (State fieldStates) =
    let
        ( fieldStateSetter, fieldAndFieldStateGetter ) =
            index ( fieldStateSetter0, fieldAndFieldStateGetter0 )

        ( field_, fieldState ) =
            getFieldAndFieldState fieldAndFieldStateGetter form_ fieldStates

        updateState fs fss =
            State
                (fieldStateSetter
                    (Tuple.mapBoth (\_ -> fs) identity)
                    fss
                )

        ( newFieldState, cmd ) =
            Field.update field_ wrappedDelta fieldState
    in
    case wrappedDelta of
        Field.Focused ->
            let
                unfocusedFieldStates =
                    fieldStateUnfocuser (\() -> ()) fieldStates
            in
            ( updateState newFieldState unfocusedFieldStates, cmd )

        _ ->
            ( updateState newFieldState fieldStates, cmd )



-- EXTRACTING STATE


stateSize1 : (restFields -> restFieldStates) -> ( Field input delta output element msg, restFields ) -> ( Field.State input output, restFieldStates )
stateSize1 next form_ =
    Tuple.mapBoth Field.init next form_



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
                    Field.parseAndValidate field { fieldState | status = Field.Idle_ }

                _ ->
                    fieldState
        )
        next
        form_
        state_



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
        Field.view
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
