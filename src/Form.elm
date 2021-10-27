module Form exposing
    ( Form
    , Index
    , State
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
    )

import Element exposing (Element)
import Field exposing (Field(..))
import Internals



-- TYPES


type Form form
    = Form form


type State state
    = State state


type alias Index input delta output msg restFields restFieldStates form state =
    (( Field input delta output msg, restFields )
     -> ( Field.State input, restFieldStates )
     -> ( Field.State input, restFieldStates )
    )
    -> form
    -> state
    -> state



-- CREATING FORMS


form countFields next =
    let
        { stateSize, validateSize, touchSize, collectSize, reverseSize, renderSize, collectElementsSize } =
            countFields
                { stateSize = identity
                , validateSize = identity
                , touchSize = identity
                , collectSize = identity
                , reverseSize = identity
                , renderSize = identity
                , collectElementsSize = identity
                }
    in
    Internals.foldr
        ( Form ()
        , \form_ ->
            { init = init stateSize form_
            , submit = submit validateSize collectSize reverseSize touchSize form_
            , update = update form_
            , viewElements = viewElements validateSize renderSize form_
            , view = view validateSize renderSize collectElementsSize form_
            }
        )
        next


end : ( state, state -> output ) -> output
end =
    Internals.end


f1 { stateSize, validateSize, touchSize, collectSize, reverseSize, renderSize, collectElementsSize } =
    { stateSize = stateSize >> sSize1
    , validateSize = validateSize >> validateSize1
    , touchSize = touchSize >> touchSize1
    , collectSize = collectSize >> collectSize1
    , reverseSize = reverseSize >> reverseSize1
    , renderSize = renderSize >> renderSize1
    , collectElementsSize = collectElementsSize >> collectElementsSize1
    }


f2 =
    f1 >> f1


f3 =
    f2 >> f1


f4 =
    f2 >> f2


f5 =
    f3 >> f2


f6 =
    f3 >> f3


f7 =
    f4 >> f3


f8 =
    f4 >> f4


f9 =
    f5 >> f4


f10 =
    f5 >> f5


field : Field input delta output msg -> ( Form ( Field input delta output msg, form ) -> c, finish ) -> (( Form form -> c, finish ) -> fields) -> fields
field field_ next =
    Internals.step0r (\(Form fields) -> Form ( field_, fields )) next



-- INDEXES FOR SETTING FIELDS


i0 : a -> a
i0 =
    identity


i1 :
    (restFields -> restFieldStates -> restFieldStates)
    -> ( Field i0 d o m, restFields )
    -> ( Field.State i0, restFieldStates )
    -> ( Field.State i0, restFieldStates )
i1 mapRest ( this0, rest0 ) ( this1, rest1 ) =
    Internals.mapBoth2 (\_ fieldState -> identity fieldState) mapRest ( this0, rest0 ) ( this1, rest1 )


i2 :
    (restFields -> restFieldStates -> restFieldStates)
    -> ( Field i0 d o m, ( Field i1 b c e, restFields ) )
    -> ( Field.State i0, ( Field.State i1, restFieldStates ) )
    -> ( Field.State i0, ( Field.State i1, restFieldStates ) )
i2 =
    i1 >> i1


i3 :
    (restFields -> restFieldStates -> restFieldStates)
    -> ( Field i0 d o m, ( Field i1 b c e, ( Field i2 g h j, restFields ) ) )
    -> ( Field.State i0, ( Field.State i1, ( Field.State i2, restFieldStates ) ) )
    -> ( Field.State i0, ( Field.State i1, ( Field.State i2, restFieldStates ) ) )
i3 =
    i2 >> i1


i4 :
    (restFields -> restFieldStates -> restFieldStates)
    -> ( Field i0 d o m, ( Field i1 b c e, ( Field i2 f g h, ( Field i3 i j k, restFields ) ) ) )
    -> ( Field.State i0, ( Field.State i1, ( Field.State i2, ( Field.State i3, restFieldStates ) ) ) )
    -> ( Field.State i0, ( Field.State i1, ( Field.State i2, ( Field.State i3, restFieldStates ) ) ) )
i4 =
    i3 >> i1


i5 :
    (restFields -> restFieldStates -> restFieldStates)
    -> ( Field i0 d o m, ( Field i1 b c e, ( Field i2 g h j, ( Field i3 l n p, ( Field i4 r s t, restFields ) ) ) ) )
    -> ( Field.State i0, ( Field.State i1, ( Field.State i2, ( Field.State i3, ( Field.State i4, restFieldStates ) ) ) ) )
    -> ( Field.State i0, ( Field.State i1, ( Field.State i2, ( Field.State i3, ( Field.State i4, restFieldStates ) ) ) ) )
i5 =
    i4 >> i1



-- SETTING FIELDS


update :
    Form form
    -> Index input delta output msg restFields restFieldStates form state
    -> delta
    -> State state
    -> State state
update (Form form_) index delta (State state_) =
    State
        (index
            (Internals.mapBoth2
                (\(Field f) fieldState -> { fieldState | input = f.updater delta fieldState.input, touched = True })
                (\_ fieldState -> fieldState)
            )
            form_
            state_
        )


parseAndValidate : Field input delta output msg -> Field.State input -> Result (List Field.Error) output
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


sSize1 : (restFields -> restFieldStates) -> ( Field input delta output msg, restFields ) -> ( Field.State input, restFieldStates )
sSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_


init : ((() -> ()) -> form -> state) -> Form form -> State state
init size (Form form_) =
    State (size (\() -> ()) form_)



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state -> results
validateAll size (Form form_) (State state_) =
    size (\() () -> ()) form_ state_


validateSize1 :
    (restFields -> restFieldStates -> restResults)
    -> ( Field input delta output msg, restFields )
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


touch : Field.State input -> Field.State input
touch f =
    { f | touched = True }


touchSize1 : (b -> y) -> ( Field.State input, b ) -> ( Field.State input, y )
touchSize1 next state_ =
    Tuple.mapBoth touch next state_


touchAll : ((() -> ()) -> state -> state) -> State state -> State state
touchAll size (State state_) =
    State (size (\() -> ()) state_)



-- SUBMITTING A FORM


submit :
    ((() -> () -> ()) -> form -> state -> b)
    -> ((a -> a) -> ( Result (List Field.Error) (), b ) -> ( Result (List Field.Error) d, e ))
    -> ((f -> f) -> ( (), d ) -> ( g, h ))
    -> ((() -> ()) -> state -> state)
    -> Form form
    -> State state
    -> Result (State state) g
submit validateSize collectSize reverseSize touchSize form_ state_ =
    validateAll validateSize form_ state_
        |> collectResults collectSize
        |> Result.map (reverseTuple reverseSize)
        |> Result.mapError (\_ -> touchAll touchSize state_)



-- CONVERTING TO ELEMENTS


renderAll :
    ((() -> () -> () -> ()) -> form -> state -> results -> elements)
    -> Form form
    -> State state
    -> results
    -> elements
renderAll size (Form form_) (State state_) results =
    size (\() () () -> ()) form_ state_ results


renderSize1 :
    (rest -> rest1 -> rest2 -> rest3)
    -> ( Field input delta output msg, rest )
    -> ( Field.State input, rest1 )
    -> ( Result (List Field.Error) output, rest2 )
    -> ( Element msg, rest3 )
renderSize1 next form_ state_ results =
    Internals.mapBoth3
        (\(Field { renderer, msg, id, label }) { input, touched } parsed ->
            renderer
                { input = input
                , touched = touched
                , msg = msg
                , parsed = parsed
                , id = id
                , label = label
                }
        )
        next
        form_
        state_
        results


viewElements :
    ((() -> () -> ()) -> form -> state -> results)
    -> ((() -> () -> () -> ()) -> form -> state -> results -> elements)
    -> Form form
    -> State state
    -> elements
viewElements validateSize renderSize form_ state_ =
    state_
        |> validateAll validateSize form_
        |> renderAll renderSize form_ state_


collectElements : ((a -> a) -> ( List (Element msg), restElements ) -> ( d, e )) -> restElements -> d
collectElements size elements =
    size identity ( [], elements )
        |> Tuple.first


collectElementsSize1 : (( List (Element msg), restElements ) -> next) -> ( List (Element msg), ( Element msg, restElements ) ) -> next
collectElementsSize1 next ( s, ( fst, rst ) ) =
    next ( fst :: s, rst )


view :
    ((() -> () -> ()) -> form -> state -> results)
    -> ((() -> () -> () -> ()) -> form -> state -> results -> c)
    ->
        ((a -> a)
         -> ( List (Element msg), c )
         -> ( List (Element msg), e )
        )
    -> Form form
    -> State state
    -> Element msg
view validateSize renderSize collectElementsSize form_ state =
    viewElements validateSize renderSize form_ state
        |> collectElements collectElementsSize
        |> List.reverse
        |> Element.column [ Element.spacing 10 ]
