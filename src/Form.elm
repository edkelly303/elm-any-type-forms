module Form exposing
    ( Error(..)
    , State(..)
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
    , floatMustBeGreaterThan
    , floatMustBeLessThan
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
    , init
    , intMustBeGreaterThan
    , intMustBeLessThan
    , stringMustBeLongerThan
    , stringMustBeShorterThan
    , stringMustNotContain
    , stringToFloat
    , stringToInt
    , stringToString
    , testForm
    )


type Error
    = NotValidInt
    | NotValidFloat
    | IntTooLow
    | IntTooHigh
    | FloatTooLow
    | FloatTooHigh
    | StringTooShort
    | StringTooLong
    | StringMustNotContain String


type Field input output
    = Field
        { data : input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        }


type Form form
    = Form form


type State state
    = State state



-- CREATING FORMS


form countFields next =
    let
        { stateSize, validateSize } =
            countFields
                { stateSize = identity
                , validateSize = identity
                }
    in
    foldr
        ( Form ()
        , \form_ ->
            { init = init stateSize form_
            , submit = submit validateSize form_
            , validate = validate form_
            , get = get
            , set = set
            }
        )
        next


f1 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, rest ) -> ( input, rest1 ), validateSize : c -> ( Field f g, d ) -> ( f, e ) -> ( Result (List Error) g, rest2 ) }
f1 { stateSize, validateSize } =
    { stateSize = stateSize >> sSize1, validateSize = validateSize >> vSize1 }


f2 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, rest ) ) -> ( input, ( f, rest1 ) ), validateSize : c -> ( Field h i, ( Field j k, d ) ) -> ( h, ( j, e ) ) -> ( Result (List Error) i, ( Result (List Error) k, rest2 ) ) }
f2 =
    f1 >> f1


f3 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, rest ) ) ) -> ( input, ( f, ( h, rest1 ) ) ), validateSize : c -> ( Field j k, ( Field l m, ( Field n o, d ) ) ) -> ( j, ( l, ( n, e ) ) ) -> ( Result (List Error) k, ( Result (List Error) m, ( Result (List Error) o, rest2 ) ) ) }
f3 =
    f2 >> f1


f4 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, rest ) ) ) ) -> ( input, ( f, ( h, ( j, rest1 ) ) ) ), validateSize : c -> ( Field l m, ( Field n o, ( Field p q, ( Field r s, d ) ) ) ) -> ( l, ( n, ( p, ( r, e ) ) ) ) -> ( Result (List Error) m, ( Result (List Error) o, ( Result (List Error) q, ( Result (List Error) s, rest2 ) ) ) ) }
f4 =
    f2 >> f2


f5 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, ( Field l m, rest ) ) ) ) ) -> ( input, ( f, ( h, ( j, ( l, rest1 ) ) ) ) ), validateSize : c -> ( Field n o, ( Field p q, ( Field r s, ( Field t u, ( Field v w, d ) ) ) ) ) -> ( n, ( p, ( r, ( t, ( v, e ) ) ) ) ) -> ( Result (List Error) o, ( Result (List Error) q, ( Result (List Error) s, ( Result (List Error) u, ( Result (List Error) w, rest2 ) ) ) ) ) }
f5 =
    f3 >> f2


f6 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, ( Field l m, ( Field n o, rest ) ) ) ) ) ) -> ( input, ( f, ( h, ( j, ( l, ( n, rest1 ) ) ) ) ) ), validateSize : c -> ( Field p q, ( Field r s, ( Field t u, ( Field v w, ( Field x y, ( Field z a1, d ) ) ) ) ) ) -> ( p, ( r, ( t, ( v, ( x, ( z, e ) ) ) ) ) ) -> ( Result (List Error) q, ( Result (List Error) s, ( Result (List Error) u, ( Result (List Error) w, ( Result (List Error) y, ( Result (List Error) a1, rest2 ) ) ) ) ) ) }
f6 =
    f3 >> f3


f7 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, ( Field l m, ( Field n o, ( Field p q, rest ) ) ) ) ) ) ) -> ( input, ( f, ( h, ( j, ( l, ( n, ( p, rest1 ) ) ) ) ) ) ), validateSize : c -> ( Field r s, ( Field t u, ( Field v w, ( Field x y, ( Field z a1, ( Field b1 c1, ( Field d1 e1, d ) ) ) ) ) ) ) -> ( r, ( t, ( v, ( x, ( z, ( b1, ( d1, e ) ) ) ) ) ) ) -> ( Result (List Error) s, ( Result (List Error) u, ( Result (List Error) w, ( Result (List Error) y, ( Result (List Error) a1, ( Result (List Error) c1, ( Result (List Error) e1, rest2 ) ) ) ) ) ) ) }
f7 =
    f4 >> f3


f8 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, ( Field l m, ( Field n o, ( Field p q, ( Field r s, rest ) ) ) ) ) ) ) ) -> ( input, ( f, ( h, ( j, ( l, ( n, ( p, ( r, rest1 ) ) ) ) ) ) ) ), validateSize : c -> ( Field t u, ( Field v w, ( Field x y, ( Field z a1, ( Field b1 c1, ( Field d1 e1, ( Field f1 g1, ( Field h1 i1, d ) ) ) ) ) ) ) ) -> ( t, ( v, ( x, ( z, ( b1, ( d1, ( f1, ( h1, e ) ) ) ) ) ) ) ) -> ( Result (List Error) u, ( Result (List Error) w, ( Result (List Error) y, ( Result (List Error) a1, ( Result (List Error) c1, ( Result (List Error) e1, ( Result (List Error) g1, ( Result (List Error) i1, rest2 ) ) ) ) ) ) ) ) }
f8 =
    f4 >> f4


f9 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, ( Field l m, ( Field n o, ( Field p q, ( Field r s, ( Field t u, rest ) ) ) ) ) ) ) ) ) -> ( input, ( f, ( h, ( j, ( l, ( n, ( p, ( r, ( t, rest1 ) ) ) ) ) ) ) ) ), validateSize : c -> ( Field v w, ( Field x y, ( Field z a1, ( Field b1 c1, ( Field d1 e1, ( Field f1 g1, ( Field h1 i1, ( Field j1 k1, ( Field l1 m1, d ) ) ) ) ) ) ) ) ) -> ( v, ( x, ( z, ( b1, ( d1, ( f1, ( h1, ( j1, ( l1, e ) ) ) ) ) ) ) ) ) -> ( Result (List Error) w, ( Result (List Error) y, ( Result (List Error) a1, ( Result (List Error) c1, ( Result (List Error) e1, ( Result (List Error) g1, ( Result (List Error) i1, ( Result (List Error) k1, ( Result (List Error) m1, rest2 ) ) ) ) ) ) ) ) ) }
f9 =
    f5 >> f4


f10 : { a | stateSize : b -> rest -> rest1, validateSize : c -> d -> e -> rest2 } -> { stateSize : b -> ( Field input output, ( Field f g, ( Field h i, ( Field j k, ( Field l m, ( Field n o, ( Field p q, ( Field r s, ( Field t u, ( Field v w, rest ) ) ) ) ) ) ) ) ) ) -> ( input, ( f, ( h, ( j, ( l, ( n, ( p, ( r, ( t, ( v, rest1 ) ) ) ) ) ) ) ) ) ), validateSize : c -> ( Field x y, ( Field z a1, ( Field b1 c1, ( Field d1 e1, ( Field f1 g1, ( Field h1 i1, ( Field j1 k1, ( Field l1 m1, ( Field n1 o1, ( Field p1 q1, d ) ) ) ) ) ) ) ) ) ) -> ( x, ( z, ( b1, ( d1, ( f1, ( h1, ( j1, ( l1, ( n1, ( p1, e ) ) ) ) ) ) ) ) ) ) -> ( Result (List Error) y, ( Result (List Error) a1, ( Result (List Error) c1, ( Result (List Error) e1, ( Result (List Error) g1, ( Result (List Error) i1, ( Result (List Error) k1, ( Result (List Error) m1, ( Result (List Error) o1, ( Result (List Error) q1, rest2 ) ) ) ) ) ) ) ) ) ) }
f10 =
    f5 >> f5



-- CREATING FIELDS


field : i -> (i -> Result Error o) -> List (o -> Maybe Error) -> ( Form ( Field i o, form ) -> c, finish ) -> (( Form form -> c, finish ) -> output) -> output
field data parser validators next =
    step0r (\(Form f) -> Form ( Field { data = data, parser = parser, validators = validators }, f )) next


stringToString : String -> List (String -> Maybe Error) -> ( Form ( Field String String, form ) -> c, finish ) -> (( Form form -> c, finish ) -> output) -> output
stringToString data validators =
    field data Ok validators


stringToFloat : String -> List (Float -> Maybe Error) -> ( Form ( Field String Float, form ) -> c, finish ) -> (( Form form -> c, finish ) -> output) -> output
stringToFloat data validators =
    field data (String.toFloat >> Result.fromMaybe NotValidFloat) validators


stringToInt : String -> List (Int -> Maybe Error) -> ( Form ( Field String Int, form ) -> c, finish ) -> (( Form form -> c, finish ) -> output) -> output
stringToInt data validators =
    field data (String.toInt >> Result.fromMaybe NotValidInt) validators



-- NASTY MAGICAL FOLD STUFF


fold : ( state, finish ) -> (( state, finish ) -> output) -> output
fold ( base, finish ) next =
    next ( base, finish )


foldr : ( a, b ) -> (( b, (a -> c) -> c ) -> output) -> output
foldr ( base, finish ) next =
    fold ( finish, \g -> g base ) next


step0r : (a -> b) -> ( b -> c, finish ) -> (( a -> c, finish ) -> output) -> output
step0r h =
    step0 (\g -> g << h)


step0 : (state -> state2) -> ( state, finish ) -> (( state2, finish ) -> output) -> output
step0 next ( state, finish ) =
    fold ( next state, finish )


end : ( state, state -> output ) -> output
end ( state, finish ) =
    finish state



-- CREATING VALIDATORS


ifTrueThenJust : a -> Bool -> Maybe a
ifTrueThenJust a bool =
    if bool then
        Just a

    else
        Nothing


stringMustBeLongerThan : Int -> String -> Maybe Error
stringMustBeLongerThan numberOfChars str =
    (String.length str <= numberOfChars)
        |> ifTrueThenJust StringTooShort


stringMustBeShorterThan : Int -> String -> Maybe Error
stringMustBeShorterThan numberOfChars str =
    (String.length str >= numberOfChars)
        |> ifTrueThenJust StringTooLong


stringMustNotContain : String -> String -> Maybe Error
stringMustNotContain content str =
    String.contains content str
        |> ifTrueThenJust (StringMustNotContain content)


intMustBeGreaterThan : Int -> Int -> Maybe Error
intMustBeGreaterThan tooLow int_ =
    (int_ <= tooLow)
        |> ifTrueThenJust IntTooLow


intMustBeLessThan : Int -> Int -> Maybe Error
intMustBeLessThan tooHigh int_ =
    (int_ >= tooHigh)
        |> ifTrueThenJust IntTooHigh


floatMustBeGreaterThan : Float -> Float -> Maybe Error
floatMustBeGreaterThan tooLow float_ =
    (float_ <= tooLow)
        |> ifTrueThenJust FloatTooLow


floatMustBeLessThan : Float -> Float -> Maybe Error
floatMustBeLessThan tooHigh float_ =
    (float_ >= tooHigh)
        |> ifTrueThenJust FloatTooHigh



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



-- SETTING FIELDS


set toIndex newVal (State state_) =
    let
        index =
            toIndex { setIdx = setIndex0, getIdx = getIndex0 }
    in
    State (index.setIdx (Tuple.mapBoth (always newVal) identity) state_)



-- GETTING AND VALIDATING SINGLE FIELDS


get toIndex (State state_) =
    let
        index =
            toIndex { setIdx = setIndex0, getIdx = getIndex0 }
    in
    index.getIdx ( state_, state_ )
        |> Tuple.first
        |> Tuple.first


validate (Form form_) toIndex (State state_) =
    let
        index =
            toIndex { setIdx = setIndex0, getIdx = getIndex0 }
    in
    index.getIdx ( form_, state_ )
        |> Tuple.mapBoth Tuple.first Tuple.first
        |> (\( field_, data ) -> parseAndValidate field_ data)


parseAndValidate : Field input output -> input -> Result (List Error) output
parseAndValidate (Field { parser, validators }) data =
    data
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


sSize1 : (rest -> rest1) -> ( Field input output, rest ) -> ( input, rest1 )
sSize1 next form_ =
    Tuple.mapBoth (\(Field { data }) -> data) next form_


init : ((() -> ()) -> form -> state) -> Form form -> State state
init size (Form form_) =
    State (size (\() -> ()) form_)



-- VALIDATING ALL FIELDS


vSize1 : (rest -> rest1 -> rest2) -> ( Field input output, rest ) -> ( input, rest1 ) -> ( Result (List Error) output, rest2 )
vSize1 next form_ state_ =
    map2 parseAndValidate next form_ state_


submit : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state -> results
submit size (Form form_) (State state_) =
    size (\() () -> ()) form_ state_



-- TESTS


testForm :
    { get :
        ({ getIdx : e -> e, setIdx : d -> d }
         -> { c | getIdx : ( state3, state3 ) -> ( ( b3, b5 ), b4 ) }
        )
        -> State state3
        -> b3
    , init : State ( String, ( String, ( String, ( String, () ) ) ) )
    , set :
        ({ getIdx : j -> j, setIdx : i -> i }
         -> { f | setIdx : (( a1, y1 ) -> ( x, y1 )) -> state2 -> state1 }
        )
        -> x
        -> State state2
        -> State state1
    , submit :
        State ( String, ( String, ( String, ( String, () ) ) ) )
        ->
            ( Result (List Error) String
            , ( Result (List Error) Float
              , ( Result (List Error) Int, ( Result (List Error) Int, () ) )
              )
            )
    , validate :
        ({ getIdx : b2 -> b2, setIdx : a -> a }
         ->
            { g
                | getIdx :
                    ( ( Field String String
                      , ( Field String Float
                        , ( Field String Int, ( Field String Int, () ) )
                        )
                      )
                    , state
                    )
                    -> ( ( Field y output, b1 ), ( y, b ) )
            }
        )
        -> State state
        -> Result (List Error) output
    }
testForm =
    form f4
        (stringToString "hello"
            [ stringMustNotContain "hell"
            , stringMustBeLongerThan 10
            ]
        )
        (stringToFloat "2"
            [ floatMustBeGreaterThan 1
            ]
        )
        (stringToInt "wow"
            [ intMustBeGreaterThan 0
            ]
        )
        (stringToInt "wow"
            [ intMustBeGreaterThan 0
            ]
        )
        end
