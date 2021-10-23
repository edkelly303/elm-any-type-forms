module Form exposing
    ( Form
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
    , i10
    , i2
    , i3
    , i4
    , i5
    , i6
    , i7
    , i8
    , i9
    )

import Field exposing (..)
import Internals exposing (..)



-- TYPES


type alias Form form =
    Internals.Form form


type alias State state =
    Internals.State state



-- CREATING FORMS


form countFields next =
    let
        { stateSize, validateSize, touchSize } =
            countFields
                { stateSize = identity
                , validateSize = identity
                , touchSize = identity
                }
    in
    foldr
        ( Form ()
        , \form_ ->
            { init = init stateSize form_
            , state = state
            , submit = \state_ -> ( touchAll touchSize state_, validateAll validateSize form_ state_ )
            , validate = validate form_
            , get = get
            , set = set
            }
        )
        next


end : ( state, state -> output ) -> output
end =
    Internals.end


f1 { stateSize, validateSize, touchSize } =
    { stateSize = stateSize >> sSize1
    , validateSize = validateSize >> validateSize1
    , touchSize = touchSize >> touchSize1
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


field : Field i o -> ( Form ( Field i o, form ) -> c, finish ) -> (( Form form -> c, finish ) -> output) -> output
field field_ next =
    step0r (\(Form fields) -> Form ( field_, fields )) next



-- NASTY TUPLE MAGIC


map2 : (a -> b -> c) -> (rest -> rest1 -> rest2) -> ( a, rest ) -> ( b, rest1 ) -> ( c, rest2 )
map2 mapA mapB ( a, b ) ( a1, b1 ) =
    ( mapA a a1, mapB b b1 )



-- INDEXES


compose : { setIdx : a -> b, getIdx : a1 -> b1 } -> { setIdx : b -> c, getIdx : b1 -> c1 } -> { setIdx : a -> c, getIdx : a1 -> c1 }
compose ab bc =
    { setIdx = ab.setIdx >> bc.setIdx
    , getIdx = ab.getIdx >> bc.getIdx
    }


i0 : { setIdx : a -> c, getIdx : b -> c1 } -> { setIdx : a -> c, getIdx : b -> c1 }
i0 =
    compose { setIdx = setIndex0, getIdx = getIndex0 }


i1 : { setIdx : (( val, rest ) -> ( val, rest )) -> c, getIdx : ( b, d ) -> c1 } -> { setIdx : (rest -> rest) -> c, getIdx : ( ( a, b ), ( e, d ) ) -> c1 }
i1 =
    compose { setIdx = setIndex1, getIdx = getIndex1 }


i2 : { setIdx : (( val, ( a, rest ) ) -> ( val, ( a, rest ) )) -> c, getIdx : ( b, d ) -> c1 } -> { setIdx : (rest -> rest) -> c, getIdx : ( ( e, ( f, b ) ), ( g, ( h, d ) ) ) -> c1 }
i2 =
    i1 >> i1


i3 : { setIdx : (( val, ( a, ( b, rest ) ) ) -> ( val, ( a, ( b, rest ) ) )) -> c, getIdx : ( d, e ) -> c1 } -> { setIdx : (rest -> rest) -> c, getIdx : ( ( f, ( g, ( h, d ) ) ), ( i, ( j, ( k, e ) ) ) ) -> c1 }
i3 =
    i2 >> i1


i4 : { setIdx : (( val, ( a, ( b, ( c, rest ) ) ) ) -> ( val, ( a, ( b, ( c, rest ) ) ) )) -> d, getIdx : ( e, f ) -> c1 } -> { setIdx : (rest -> rest) -> d, getIdx : ( ( g, ( h, ( i, ( j, e ) ) ) ), ( k, ( l, ( m, ( n, f ) ) ) ) ) -> c1 }
i4 =
    i3 >> i1


i5 : { setIdx : (( val, ( a, ( b, ( c, ( d, rest ) ) ) ) ) -> ( val, ( a, ( b, ( c, ( d, rest ) ) ) ) )) -> e, getIdx : ( f, g ) -> c1 } -> { setIdx : (rest -> rest) -> e, getIdx : ( ( h, ( i, ( j, ( k, ( l, f ) ) ) ) ), ( m, ( n, ( o, ( p, ( q, g ) ) ) ) ) ) -> c1 }
i5 =
    i4 >> i1


i6 : { setIdx : (( val, ( a, ( b, ( c, ( d, ( e, rest ) ) ) ) ) ) -> ( val, ( a, ( b, ( c, ( d, ( e, rest ) ) ) ) ) )) -> f, getIdx : ( g, h ) -> c1 } -> { setIdx : (rest -> rest) -> f, getIdx : ( ( i, ( j, ( k, ( l, ( m, ( n, g ) ) ) ) ) ), ( o, ( p, ( q, ( r, ( s, ( t, h ) ) ) ) ) ) ) -> c1 }
i6 =
    i5 >> i1


i7 : { setIdx : (( val, ( a, ( b, ( c, ( d, ( e, ( f, rest ) ) ) ) ) ) ) -> ( val, ( a, ( b, ( c, ( d, ( e, ( f, rest ) ) ) ) ) ) )) -> g, getIdx : ( h, i ) -> c1 } -> { setIdx : (rest -> rest) -> g, getIdx : ( ( j, ( k, ( l, ( m, ( n, ( o, ( p, h ) ) ) ) ) ) ), ( q, ( r, ( s, ( t, ( u, ( v, ( w, i ) ) ) ) ) ) ) ) -> c1 }
i7 =
    i6 >> i1


i8 : { setIdx : (( val, ( a, ( b, ( c, ( d, ( e, ( f, ( g, rest ) ) ) ) ) ) ) ) -> ( val, ( a, ( b, ( c, ( d, ( e, ( f, ( g, rest ) ) ) ) ) ) ) )) -> h, getIdx : ( i, j ) -> c1 } -> { setIdx : (rest -> rest) -> h, getIdx : ( ( k, ( l, ( m, ( n, ( o, ( p, ( q, ( r, i ) ) ) ) ) ) ) ), ( s, ( t, ( u, ( v, ( w, ( x, ( y, ( z, j ) ) ) ) ) ) ) ) ) -> c1 }
i8 =
    i7 >> i1


i9 : { setIdx : (( val, ( a, ( b, ( c, ( d, ( e, ( f, ( g, ( h, rest ) ) ) ) ) ) ) ) ) -> ( val, ( a, ( b, ( c, ( d, ( e, ( f, ( g, ( h, rest ) ) ) ) ) ) ) ) )) -> i, getIdx : ( j, k ) -> c1 } -> { setIdx : (rest -> rest) -> i, getIdx : ( ( l, ( m, ( n, ( o, ( p, ( q, ( r, ( s, ( t, j ) ) ) ) ) ) ) ) ), ( u, ( v, ( w, ( x, ( y, ( z, ( a1, ( b1, ( c1, k ) ) ) ) ) ) ) ) ) ) -> c1 }
i9 =
    i8 >> i1


i10 : { setIdx : (( val, ( a, ( b, ( c, ( d, ( e, ( f, ( g, ( h, ( i, rest ) ) ) ) ) ) ) ) ) ) -> ( val, ( a, ( b, ( c, ( d, ( e, ( f, ( g, ( h, ( i, rest ) ) ) ) ) ) ) ) ) )) -> j, getIdx : ( k, l ) -> c1 } -> { setIdx : (rest -> rest) -> j, getIdx : ( ( m, ( n, ( o, ( p, ( q, ( r, ( s, ( t, ( u, ( v, k ) ) ) ) ) ) ) ) ) ), ( w, ( x, ( y, ( z, ( a1, ( b1, ( c1, ( d1, ( b1, ( c1, l ) ) ) ) ) ) ) ) ) ) ) -> c1 }
i10 =
    i9 >> i1


getIndex0 : a -> a
getIndex0 =
    identity


getIndex1 : ( ( a, b ), ( c, d ) ) -> ( b, d )
getIndex1 ( f, s ) =
    ( Tuple.second f, Tuple.second s )


setIndex0 : a -> a
setIndex0 =
    identity


setIndex1 : (rest -> rest) -> ( val, rest ) -> ( val, rest )
setIndex1 mapRest ( val, rest ) =
    Tuple.mapBoth identity mapRest ( val, rest )



-- SETTING SINGLE FIELDS


set toIndex newVal (State state_) =
    let
        index =
            toIndex { setIdx = setIndex0, getIdx = getIndex0 }
    in
    State (index.setIdx (Tuple.mapBoth (\d -> { d | data = newVal, touched = True }) identity) state_)



-- GETTING SINGLE FIELDS


get toIndex (State state_) =
    let
        index =
            toIndex { setIdx = setIndex0, getIdx = getIndex0 }
    in
    index.getIdx ( state_, state_ )
        |> Tuple.first
        |> Tuple.first
        |> .data



-- VALIDATING SINGLE FIELDS


validate (Form form_) toIndex (State state_) =
    let
        index =
            toIndex { setIdx = setIndex0, getIdx = getIndex0 }
    in
    index.getIdx ( form_, state_ )
        |> Tuple.mapBoth Tuple.first Tuple.first
        |> (\( field_, s_ ) -> parseAndValidate field_ s_)


parseAndValidate : Field input output -> { data : input, touched : Bool } -> Result (List Error) output
parseAndValidate (Field { parser, validators }) data =
    data.data
        |> parser
        |> Result.mapError List.singleton
        |> Result.andThen
            (\parsed ->
                validators
                    |> List.map (\v -> v parsed)
                    |> accumulateErrors parsed
            )


accumulateErrors : a -> List (Maybe Error) -> Result (List Error) a
accumulateErrors a list =
    case List.filterMap identity list of
        [] ->
            Ok a

        errors ->
            Err errors



-- EXTRACTING STATE


sSize1 : (b -> y) -> ( Field input output, b ) -> ( { data : input, touched : Bool }, y )
sSize1 next form_ =
    Tuple.mapBoth (\(Field { data, touched }) -> { data = data, touched = touched }) next form_


init : ((() -> ()) -> form -> state) -> Form form -> State state
init size (Form form_) =
    State (size (\() -> ()) form_)


state : State state -> state
state (State state_) =
    state_



-- VALIDATING ALL FIELDS


validateSize1 : (rest -> rest1 -> rest2) -> ( Field input output, rest ) -> ( { data : input, touched : Bool }, rest1 ) -> ( Result (List Error) output, rest2 )
validateSize1 next form_ state_ =
    map2 parseAndValidate next form_ state_


validateAll : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state -> results
validateAll size (Form form_) (State state_) =
    size (\() () -> ()) form_ state_



-- TOUCHING ALL FIELDS


touch : { a | touched : Bool } -> { a | touched : Bool }
touch f =
    { f | touched = True }


touchSize1 : (rest -> rest1) -> ( { a | touched : Bool }, rest ) -> ( { a | touched : Bool }, rest1 )
touchSize1 next state_ =
    Tuple.mapBoth touch next state_


touchAll : ((() -> ()) -> state -> state1) -> State state -> State state1
touchAll size (State state_) =
    State (size (\() -> ()) state_)
