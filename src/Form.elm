module Form exposing
    ( Form
    , Index
    , InternalMsg
    , State
    , defaultConfig
    , end
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
    , field
    , form
    , i0
    , i1
    , i2
    , i3
    , i4
    , i5
    , idx0
    , idx1
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


type alias Config element msg =
    { layout : List element -> element
    , submitMsg : Maybe msg
    , submitRenderer : msg -> element
    , formMsg : InternalMsg -> msg
    }


defaultConfig : (InternalMsg -> msg) -> Config (Element msg) msg
defaultConfig toMsg =
    { layout =
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
                [ Element.padding 10
                , Element.Border.width 1
                , Element.Border.rounded 5
                ]
                { label = Element.text "Submit", onPress = Just msg }
    , formMsg = toMsg
    }


withRenderer : { layout : List element2 -> element2, submit : msg -> element2 } -> Config element msg -> Config element2 msg
withRenderer args config =
    { layout = args.layout
    , submitRenderer = args.submit
    , submitMsg = config.submitMsg
    , formMsg = config.formMsg
    }


withSubmit : msg -> Config element msg -> Config element msg
withSubmit msg config =
    { config | submitMsg = Just msg }



-- CREATING FORMS


form config numberOfFields next =
    let
        { stateSize, validateSize, collectSize, reverseSize, renderSize, collectElementsSize, countSize } =
            numberOfFields
                { stateSize = identity
                , validateSize = identity
                , collectSize = identity
                , reverseSize = identity
                , renderSize = identity
                , collectElementsSize = identity
                , countSize = identity
                }
    in
    Internals.foldr
        ( ( Form (), 0 )
        , \( form_, count ) ->
            { init = init countSize stateSize form_
            , submit = submit validateSize collectSize reverseSize form_
            , update = update count form_
            , internalUpdate = internalUpdate
            , viewFields = viewElements config validateSize renderSize form_
            , view = view config validateSize renderSize collectElementsSize form_
            }
        )
        next


end : ( state, state -> output ) -> output
end =
    Internals.end


f1 { stateSize, validateSize, collectSize, reverseSize, renderSize, collectElementsSize, countSize } =
    { stateSize = stateSize >> stateSize1
    , validateSize = validateSize >> validateSize1
    , collectSize = collectSize >> collectSize1
    , reverseSize = reverseSize >> reverseSize1
    , renderSize = renderSize >> renderSize1
    , collectElementsSize = collectElementsSize >> collectElementsSize1
    , countSize = countSize >> countSize1
    }


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


field : Field input delta output element msg -> ( ( Form ( Field input delta output element msg, form ), Int ) -> c, finish ) -> (( ( Form form, Int ) -> c, finish ) -> a) -> a
field (Field field_) next =
    Internals.step0r (\( Form fields, fieldNumber ) -> ( Form ( Field { field_ | index = fieldNumber }, fields ), fieldNumber + 1 )) next



-- INDEXES FOR SETTING FIELDS


i0 : a -> a
i0 =
    identity


i1 : (b -> a -> rest2) -> ( c, b ) -> ( d, a ) -> ( d, rest2 )
i1 mapRest ( this0, rest0 ) ( this1, rest1 ) =
    Internals.mapBoth2 (\_ fieldState -> identity fieldState) mapRest ( this0, rest0 ) ( this1, rest1 )


i2 : (b -> a -> rest2) -> ( c, ( d, b ) ) -> ( e, ( f, a ) ) -> ( e, ( f, rest2 ) )
i2 =
    i1 >> i1


i3 : (b -> a -> rest2) -> ( c, ( d, ( e, b ) ) ) -> ( f, ( g, ( h, a ) ) ) -> ( f, ( g, ( h, rest2 ) ) )
i3 =
    i2 >> i1


i4 : (b -> a -> rest2) -> ( c, ( d, ( e, ( f, b ) ) ) ) -> ( g, ( h, ( i, ( j, a ) ) ) ) -> ( g, ( h, ( i, ( j, rest2 ) ) ) )
i4 =
    i3 >> i1


i5 : (b -> a -> rest2) -> ( c, ( d, ( e, ( f, ( g, b ) ) ) ) ) -> ( h, ( i, ( j, ( k, ( l, a ) ) ) ) ) -> ( h, ( i, ( j, ( k, ( l, rest2 ) ) ) ) )
i5 =
    i4 >> i1


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


update totalNumberOfFields (Form form_) index delta (State dict state_) =
    let
        ( i, c ) =
            index ( i0, c0 )

        highestFieldIndex =
            totalNumberOfFields - 1

        indexOfUpdatedField =
            highestFieldIndex - c 0
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


init : ((a -> a) -> ( Int, form ) -> ( Int, d )) -> ((() -> ()) -> form -> b) -> Form form -> State b
init countSize stateSize (Form form_) =
    let
        numberOfFields =
            countFields countSize form_

        range =
            List.range 0 (numberOfFields - 1)

        dict =
            List.foldl (\i d -> Dict.insert i { touched = False, focused = False } d) Dict.empty range
    in
    State dict (stateSize (\() -> ()) form_)


countFields : ((a -> a) -> ( Int, b ) -> ( c, d )) -> b -> c
countFields size fields =
    size identity ( 0, fields )
        |> Tuple.first


countSize1 : (( Int, b ) -> a) -> ( Int, ( c, b ) ) -> a
countSize1 next ( s, ( fst, rst ) ) =
    next ( s + 1, rst )



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state -> results
validateAll size (Form form_) (State _ state_) =
    size (\() () -> ()) form_ state_


validateSize1 :
    (restFields -> restFieldStates -> restResults)
    -> ( Field input delta output element msg, restFields )
    -> ( Field.State input, restFieldStates )
    -> ( Result (List Field.Error) output, restResults )
validateSize1 next form_ state_ =
    Internals.mapBoth2 parseAndValidate next form_ state_



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


collectResults :
    ((a -> a) -> ( Result (List Field.Error) (), restResults ) -> ( c, d ))
    -> restResults
    -> c
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



-- TOUCHING ALL FIELDS


touchAll : State state -> State state
touchAll (State dict state_) =
    State (Dict.map (\_ v -> { v | touched = True }) dict) state_



-- SUBMITTING A FORM


submit :
    ((() -> () -> ()) -> form -> state -> restResults)
    -> ((a -> a) -> ( Result (List Field.Error) (), restResults ) -> ( Result b c, d ))
    -> ((e -> e) -> ( (), c ) -> ( f, g ))
    -> Form form
    -> State state
    -> Result (State state) f
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
