module Form exposing
    ( Form
    , Index
    , InternalMsg
    , State
    , done
    , form
    , idx0
    , idx1
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



-- TYPES


type Form form
    = Form form


type State state
    = State InternalState state


type alias InternalState =
    Dict.Dict Int { touched : Bool, focused : Bool }


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
    | Noop



-- FORM CONFIG


type Builder a b c d e f g fields element msg
    = Builder
        { reverseSize : a
        , reverseSize2 : b
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
        , reverseSize2 = identity
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


withField (Field fld) (Builder frm) =
    Builder
        { reverseSize = frm.reverseSize >> reverseSize1
        , reverseSize2 = frm.reverseSize2 >> reverseSize1a
        , stateSize = frm.stateSize >> stateSize1
        , validateSize = frm.validateSize >> validateSize1
        , collectSize = frm.collectSize >> collectSize1
        , renderSize = frm.renderSize >> renderSize1
        , collectElementsSize = frm.collectElementsSize >> collectElementsSize1
        , fieldCount = frm.fieldCount + 1
        , formState = Dict.insert frm.fieldCount { touched = False, focused = False } frm.formState
        , fields = ( Field { fld | index = frm.fieldCount }, frm.fields )
        , layout = frm.layout
        , submitMsg = frm.submitMsg
        , submitRenderer = frm.submitRenderer
        , formMsg = frm.formMsg
        }


withRenderer :
    { layout : List element2 -> element2, submit : msg -> element2 }
    -> Builder b c d e f g h fields element msg
    -> Builder b c d e f g h fields element2 msg
withRenderer args (Builder frm) =
    Builder
        { reverseSize = frm.reverseSize
        , reverseSize2 = frm.reverseSize2
        , stateSize = frm.stateSize
        , validateSize = frm.validateSize
        , collectSize = frm.collectSize
        , renderSize = frm.renderSize
        , collectElementsSize = frm.collectElementsSize
        , fieldCount = frm.fieldCount
        , formState = frm.formState
        , fields = frm.fields
        , layout = args.layout
        , submitRenderer = args.submit
        , submitMsg = frm.submitMsg
        , formMsg = frm.formMsg
        }


withSubmit : msg -> Builder b c d e f g h fields element msg -> Builder b c d e f g h fields element msg
withSubmit msg (Builder frm) =
    Builder { frm | submitMsg = Just msg }


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
                   -> ( { d1 | input : input }, e1 )
                   -> ( { d1 | input : input }, e1 )
                  )
                  -> form
                  -> state
                  -> state
                , Int -> Int
                )
            )
            -> delta
            -> State state
            -> State state
        , update : InternalMsg -> State state -> State state
        , viewFields : State state -> q
        , view : State state -> element
        }
done (Builder frm) =
    let
        config =
            { layout = frm.layout
            , submitRenderer = frm.submitRenderer
            , formMsg = frm.formMsg
            , submitMsg = frm.submitMsg
            }

        reversedFields =
            reverseTuple frm.reverseSize frm.fields

        form_ =
            Form reversedFields

        fieldsState =
            frm.stateSize (\() -> ()) reversedFields
    in
    { init = State frm.formState fieldsState
    , submit = submit frm.validateSize frm.collectSize frm.reverseSize2 form_
    , updateField = update form_
    , update = internalUpdate
    , viewFields = viewElements config frm.validateSize frm.renderSize form_
    , view = view config frm.validateSize frm.renderSize frm.collectElementsSize form_
    }



-- INDEXES FOR SETTING FIELDS


i0 : a -> a
i0 =
    identity


i1 : (b -> a -> rest2) -> ( c, b ) -> ( d, a ) -> ( d, rest2 )
i1 mapRest ( this0, rest0 ) ( this1, rest1 ) =
    Internals.mapBoth2 (\_ fieldState -> identity fieldState) mapRest ( this0, rest0 ) ( this1, rest1 )


c0 : Int -> Int
c0 =
    (+) 0


c1 : Int -> Int
c1 =
    (+) 1


idx0 : a -> a
idx0 =
    identity


idx1 : ( (( c, b ) -> ( d, a ) -> ( d, rest2 )) -> e, Int -> f ) -> ( (b -> a -> rest2) -> e, Int -> f )
idx1 =
    compose ( i1, c1 )


compose : ( a -> b, c -> d ) -> ( b -> e, d -> f ) -> ( a -> e, c -> f )
compose ( a, b ) ( a1, b1 ) =
    ( a >> a1, b >> b1 )



-- SETTING FIELDS


internalUpdate : InternalMsg -> State state -> State state
internalUpdate msg (State internalState state) =
    State
        (case msg of
            Focused f ->
                Dict.update f (Maybe.map (\s -> { s | focused = True })) internalState

            Blurred f ->
                Dict.update f (Maybe.map (\s -> { s | focused = False })) internalState

            Noop ->
                internalState
        )
        state


update :
    Form form
    ->
        (( a -> a, Int -> Int )
         ->
            ( (( Field input delta output element msg, d )
               -> ( { e | input : input }, f )
               -> ( { e | input : input }, f )
              )
              -> form
              -> state
              -> state
            , Int -> Int
            )
        )
    -> delta
    -> State state
    -> State state
update (Form form_) index delta (State dict state_) =
    let
        ( i, c ) =
            index ( i0, c0 )

        indexOfUpdatedField =
            c 0
    in
    State
        (Dict.update indexOfUpdatedField (Maybe.map (\internalState -> { internalState | touched = True })) dict)
        (i
            (Internals.mapBoth2
                (\(Field f) fieldState -> { fieldState | input = f.updater delta fieldState.input })
                (\_ fieldState -> fieldState)
            )
            form_
            state_
        )


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



-- EXTRACTING STATE


stateSize1 : (restFields -> restFieldStates) -> ( Field input delta output element msg, restFields ) -> ( Field.State input, restFieldStates )
stateSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_



-- VALIDATING ALL FIELDS


validateAll size (Form form_) (State _ state_) =
    size (\() () -> ()) form_ state_


validateSize1 next form_ state_ =
    Internals.mapBoth2 parseAndValidate next form_ state_



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


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


reverseSize1a : (( ( a, b ), c ) -> d) -> ( b, ( a, c ) ) -> d
reverseSize1a next ( s, ( fst, rest ) ) =
    next ( ( fst, s ), rest )



-- TOUCHING ALL FIELDS


touchAll : State state -> State state
touchAll (State dict state_) =
    State (Dict.map (\_ v -> { v | touched = True }) dict) state_



-- SUBMITTING A FORM


submit validateSize collectSize reverseSize form_ state_ =
    validateAll validateSize form_ state_
        |> collectResults collectSize
        |> Result.map (reverseTuple reverseSize)
        |> Result.mapError (\_ -> touchAll state_)



-- CONVERTING TO ELEMENTS


renderAll config size (Form form_) (State internalState state_) results =
    size (\_ _ () () () -> ()) config internalState form_ state_ results


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
