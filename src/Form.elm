module Form exposing
    ( Form
    , Index
    , InternalMsg
    , State
    , done
    , form
    , i0
    , i1
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
    = State InternalState state


type alias InternalState =
    Dict.Dict Int { touched : Bool, focused : Bool, lastTouched : Maybe Time.Posix }


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
    | Blurred Int
    | TypingDetected Int Time.Posix
    | TypingTimedOut Int Time.Posix



-- FORM CONFIG


type Builder a b c d e f g fields element msg
    = Builder
        { reverseSize : a
        , anotherReverseSize : b
        , stateSize : c
        , validateSize : d
        , collectSize : e
        , renderSize : f
        , collectElementsSize : g
        , fieldCount : Int
        , formState : InternalState
        , fields : fields
        , layout : List element -> element
        , submitMsg : Maybe msg
        , submitRenderer : msg -> element
        , formMsg : InternalMsg -> msg
        }



-- CREATING FORMS


form : (InternalMsg -> msg) -> Builder (b -> b) (c -> c) (d -> d) (e -> e) (f -> f) (g -> g) (h -> h) () (Element msg) msg
form formMsg =
    Builder
        { reverseSize = identity
        , anotherReverseSize = identity
        , stateSize = identity
        , validateSize = identity
        , collectSize = identity
        , renderSize = identity
        , collectElementsSize = identity
        , fieldCount = 0
        , formState = Dict.empty
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


withField :
    Field input delta output element msg
    ->
        Builder
            (a -> ( ( b, c ), d ) -> e)
            (f -> ( ( g, h ), i ) -> j)
            (k -> restFields -> restFieldStates)
            (l -> rest0 -> rest1 -> rest2)
            (m
             -> ( Result (List Field.Error) ( value, n ), restResults )
             -> o
            )
            (p -> { q | formMsg : InternalMsg -> msg } -> InternalState -> rest -> t -> u -> rest3)
            (v -> ( List w, restElements ) -> next)
            fields
            x
            y
    ->
        Builder
            (a -> ( c, ( b, d ) ) -> e)
            (f -> ( h, ( g, i ) ) -> j)
            (k -> ( Field z a1 b1 c1 d1, restFields ) -> ( Field.State z, restFieldStates ))
            (l -> ( Field e1 f1 g1 h1 i1, rest0 ) -> ( Field.State e1, rest1 ) -> ( Result (List Field.Error) g1, rest2 ))
            (m -> ( Result (List Field.Error) n, ( Result (List Field.Error) value, restResults ) ) -> o)
            (p
             -> { q | formMsg : InternalMsg -> msg }
             -> InternalState
             -> ( Field l1 m1 n1 o1 msg, rest )
             -> ( Field.State l1, t )
             -> ( Result (List Field.Error) n1, u )
             -> ( o1, rest3 )
            )
            (v
             -> ( List w, ( w, restElements ) )
             -> next
            )
            ( Field input delta output element msg, fields )
            x
            y
withField (Field fld) (Builder bdr) =
    Builder
        { reverseSize = bdr.reverseSize >> reverseSize1
        , anotherReverseSize = bdr.anotherReverseSize >> anotherReverseSize1
        , stateSize = bdr.stateSize >> stateSize1
        , validateSize = bdr.validateSize >> validateSize1
        , collectSize = bdr.collectSize >> collectSize1
        , renderSize = bdr.renderSize >> renderSize1
        , collectElementsSize = bdr.collectElementsSize >> collectElementsSize1
        , fieldCount = bdr.fieldCount + 1
        , formState =
            Dict.insert bdr.fieldCount
                { touched = False
                , focused = False
                , lastTouched = Nothing
                }
                bdr.formState
        , fields = ( Field { fld | index = bdr.fieldCount }, bdr.fields )
        , layout = bdr.layout
        , submitMsg = bdr.submitMsg
        , submitRenderer = bdr.submitRenderer
        , formMsg = bdr.formMsg
        }


withRenderer :
    { layout : List element2 -> element2, submit : msg -> element2 }
    -> Builder b c d e f g h fields element msg
    -> Builder b c d e f g h fields element2 msg
withRenderer args (Builder bdr) =
    Builder
        { reverseSize = bdr.reverseSize
        , anotherReverseSize = bdr.anotherReverseSize
        , stateSize = bdr.stateSize
        , validateSize = bdr.validateSize
        , collectSize = bdr.collectSize
        , renderSize = bdr.renderSize
        , collectElementsSize = bdr.collectElementsSize
        , fieldCount = bdr.fieldCount
        , formState = bdr.formState
        , fields = bdr.fields
        , layout = args.layout
        , submitRenderer = args.submit
        , submitMsg = bdr.submitMsg
        , formMsg = bdr.formMsg
        }


withSubmit : msg -> Builder b c d e f g h fields element msg -> Builder b c d e f g h fields element msg
withSubmit msg (Builder bdr) =
    Builder { bdr | submitMsg = Just msg }


done :
    Builder
        ((a -> a) -> ( (), fields ) -> ( form, d ))
        ((b -> b) -> ( (), e ) -> ( f, g ))
        ((() -> ()) -> form -> state)
        ((() -> () -> ()) -> form -> state -> k)
        ((l -> l) -> ( Result error (), k ) -> ( Result m e, n ))
        ((o -> p -> () -> () -> () -> ())
         ->
            { layout : List element -> element
            , submitRenderer : msg -> element
            , formMsg : InternalMsg -> msg
            , submitMsg : Maybe msg
            }
         -> InternalState
         -> form
         -> state
         -> k
         -> q
        )
        ((r -> r) -> ( List s, q ) -> ( List element, t ))
        fields
        element
        msg
    ->
        { init : State state
        , submit : State state -> Result (State state) f
        , updateField :
            (( x -> x, Int -> Int )
             ->
                ( (( Field input delta output element msg, c1 )
                   -> ( Field.State input, e1 )
                   -> ( Field.State input, e1 )
                  )
                  -> form
                  -> state
                  -> state
                , Int -> Int
                )
            )
            -> delta
            -> State state
            -> ( State state, Cmd msg )
        , update : InternalMsg -> State state -> ( State state, Cmd msg )
        , viewFields : State state -> q
        , view : State state -> element
        }
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
    , submit = submit bdr.validateSize bdr.collectSize bdr.anotherReverseSize form_
    , updateField = update bdr.formMsg form_
    , update = internalUpdate bdr.formMsg
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


compose : ( a -> b, c -> d ) -> ( b -> e, d -> f ) -> ( a -> e, c -> f )
compose ( a, b ) ( a1, b1 ) =
    ( a >> a1, b >> b1 )



-- SETTING FIELDS


internalUpdate : (InternalMsg -> msg) -> InternalMsg -> State state -> ( State state, Cmd msg )
internalUpdate formMsg msg (State internalState state) =
    let
        ( internalState2, cmd ) =
            case msg of
                Focused f ->
                    ( Dict.update f (Maybe.map (\s -> { s | focused = True })) internalState, Cmd.none )

                Blurred f ->
                    ( Dict.update f (Maybe.map (\s -> { s | focused = False })) internalState, Cmd.none )

                TypingDetected f time ->
                    ( Dict.update f (Maybe.map (\s -> { s | lastTouched = Just time })) internalState
                    , Task.perform (\() -> formMsg <| TypingTimedOut f time) (Process.sleep 500)
                    )

                TypingTimedOut f time ->
                    Dict.get f internalState
                        |> Maybe.map
                            (\s ->
                                if s.lastTouched == Just time then
                                    ( Dict.insert f { s | lastTouched = Nothing } internalState
                                    , Cmd.none
                                    )

                                else
                                    ( internalState, Cmd.none )
                            )
                        |> Maybe.withDefault ( internalState, Cmd.none )
    in
    ( State internalState2 state, cmd )


update :
    (InternalMsg -> msg)
    -> Form form
    ->
        (( a -> a, Int -> Int )
         ->
            ( (( Field input delta output element msg, restFields )
               -> ( Field.State input, restStates )
               -> ( Field.State input, restStates )
              )
              -> form
              -> state
              -> state
            , Int -> Int
            )
        )
    -> delta
    -> State state
    -> ( State state, Cmd msg )
update formMsg (Form form_) index delta (State dict state_) =
    let
        ( selectField, countField ) =
            index ( selectField0, countField0 )

        indexOfUpdatedField =
            countField 0
    in
    ( State
        (Dict.update indexOfUpdatedField (Maybe.map (\s -> { s | touched = True })) dict)
        (selectField
            (Internals.mapBoth2
                (\(Field f) fieldState -> { fieldState | input = f.updater delta fieldState.input })
                (\_ fieldState -> fieldState)
            )
            form_
            state_
        )
    , Task.perform (formMsg << TypingDetected indexOfUpdatedField) Time.now
    )



-- EXTRACTING STATE


stateSize1 : (restFields -> restFieldStates) -> ( Field input delta output element msg, restFields ) -> ( Field.State input, restFieldStates )
stateSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state -> results
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



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


collectResults : ((a -> a) -> ( Result error (), results ) -> ( collectedResult, d )) -> results -> collectedResult
collectResults size results =
    size identity ( Ok (), results )
        |> Tuple.first


collectSize1 :
    (( Result (List Field.Error) ( value, b ), restResults ) -> d)
    -> ( Result (List Field.Error) b, ( Result (List Field.Error) value, restResults ) )
    -> d
collectSize1 next ( s, ( fst, rst ) ) =
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


touchAll : State state -> State state
touchAll (State dict state_) =
    State (Dict.map (\_ v -> { v | touched = True }) dict) state_



-- SUBMITTING A FORM


submit :
    ((() -> () -> ()) -> form -> state -> b)
    -> ((a -> a) -> ( Result error (), b ) -> ( Result c d, e ))
    -> ((f -> f) -> ( (), d ) -> ( g, h ))
    -> Form form
    -> State state
    -> Result (State state) g
submit validateSize collectSize reverseSize form_ state_ =
    validateAll validateSize form_ state_
        |> collectResults collectSize
        |> Result.map (reverseTuple reverseSize)
        |> Result.mapError (\_ -> touchAll state_)



-- CONVERTING TO ELEMENTS


renderAll : a -> ((b -> c -> () -> () -> () -> ()) -> a -> InternalState -> form -> state -> e -> d) -> Form form -> State state -> e -> d
renderAll config size (Form form_) (State internalState state_) results =
    size (\_ _ () () () -> ()) config internalState form_ state_ results


renderSize1 :
    ({ a | formMsg : InternalMsg -> msg } -> InternalState -> rest -> rest1 -> rest2 -> rest3)
    -> { a | formMsg : InternalMsg -> msg }
    -> InternalState
    -> ( Field input delta output element msg, rest )
    -> ( { e | input : input }, rest1 )
    -> ( Result (List Field.Error) output, rest2 )
    -> ( element, rest3 )
renderSize1 next config internalState form_ state_ results =
    Internals.mapBoth3
        (\(Field { index, renderer, msg, id, label }) { input } parsed ->
            renderer
                { input = input
                , touched =
                    Dict.get index internalState
                        |> Maybe.map .touched
                        |> Maybe.withDefault False
                , focused =
                    Dict.get index internalState
                        |> Maybe.map .focused
                        |> Maybe.withDefault False
                , typing =
                    Dict.get index internalState
                        |> Maybe.map (\{ lastTouched } -> lastTouched /= Nothing)
                        |> Maybe.withDefault False
                , onBlurMsg = config.formMsg (Blurred index)
                , onFocusMsg = config.formMsg (Focused index)
                , msg = msg
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
         -> InternalState
         -> form
         -> state
         -> results
         -> elements
        )
    -> Form form
    -> State state
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
         -> InternalState
         -> form
         -> state
         -> e
         -> restElements
        )
    -> ((h -> h) -> ( List element, restElements ) -> ( List c, i ))
    -> Form form
    -> State state
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
