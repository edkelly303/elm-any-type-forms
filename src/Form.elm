module Form exposing
    ( Form
    , State
    , collectResults
    , collectSize1
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
    , reverseSize1
    , reverseTuple
    )

import Element exposing (Element)
import Field exposing (Field(..))
import Internals exposing (..)



-- TYPES


type Form form
    = Form form


type State state
    = State state



-- CREATING FORMS


form :
    ({ stateSize : a -> a
     , validateSize : b -> b
     , touchSize : c -> c
     , collectSize : d -> d
     , reverseSize : e -> e
     , renderSize : f -> f
     }
     ->
        { g
            | stateSize : (() -> ()) -> form -> state
            , validateSize : (() -> () -> ()) -> form -> state -> i
            , touchSize : (() -> ()) -> state -> state
            , collectSize : (j -> j) -> ( Result (List Field.Error) (), i ) -> ( Result (List Field.Error) l, m )
            , reverseSize : (n -> n) -> ( (), l ) -> ( o, p )
            , renderSize : (() -> () -> () -> ()) -> form -> state -> i -> elements
        }
    )
    ->
        (( Form form
           ->
            { init : State state
            , submit : State state -> Result (State state) o
            , set : ((( Field.State input, rest ) -> ( Field.State input, rest )) -> state -> state) -> input -> State state -> State state
            , toEls : State state -> elements
            }
         , (Form () -> r) -> r
         )
         -> output
        )
    -> output
form countFields next =
    let
        { stateSize, validateSize, touchSize, collectSize, reverseSize, renderSize } =
            countFields
                { stateSize = identity
                , validateSize = identity
                , touchSize = identity
                , collectSize = identity
                , reverseSize = identity
                , renderSize = identity
                }
    in
    foldr
        ( Form ()
        , \form_ ->
            { init = init stateSize form_
            , submit = submit validateSize collectSize reverseSize touchSize form_
            , set = set
            , toEls = toEls validateSize renderSize form_
            }
        )
        next


end : ( state, state -> output ) -> output
end =
    Internals.end


f1 { stateSize, validateSize, touchSize, collectSize, reverseSize, renderSize } =
    { stateSize = stateSize >> sSize1
    , validateSize = validateSize >> validateSize1
    , touchSize = touchSize >> touchSize1
    , collectSize = collectSize >> collectSize1
    , reverseSize = reverseSize >> reverseSize1
    , renderSize = renderSize >> renderSize1
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


field : Field input output msg -> ( Form ( Field input output msg, form ) -> c, finish ) -> (( Form form -> c, finish ) -> fields) -> fields
field field_ next =
    step0r (\(Form fields) -> Form ( field_, fields )) next



-- INDEXES FOR SETTING FIELDS


i0 : a -> a
i0 =
    identity


i1 : (rest -> rest) -> ( zero, rest ) -> ( zero, rest )
i1 mapRest ( val, rest ) =
    Tuple.mapBoth identity mapRest ( val, rest )


i2 : (rest -> rest) -> ( zero, ( one, rest ) ) -> ( zero, ( one, rest ) )
i2 =
    i1 >> i1


i3 : (rest -> rest) -> ( zero, ( one, ( two, rest ) ) ) -> ( zero, ( one, ( two, rest ) ) )
i3 =
    i2 >> i1


i4 : (rest -> rest) -> ( zero, ( one, ( two, ( three, rest ) ) ) ) -> ( zero, ( one, ( two, ( three, rest ) ) ) )
i4 =
    i3 >> i1


i5 : (rest -> rest) -> ( zero, ( one, ( two, ( three, ( four, rest ) ) ) ) ) -> ( zero, ( one, ( two, ( three, ( four, rest ) ) ) ) )
i5 =
    i4 >> i1



-- SETTING FIELDS


set : ((( Field.State input, rest ) -> ( Field.State input, rest )) -> state -> state) -> input -> State state -> State state
set index newVal (State state_) =
    State (index (Tuple.mapBoth (\d -> { d | input = newVal, touched = True }) identity) state_)


parseAndValidate : Field input output msg -> Field.State input -> Result (List Field.Error) output
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


sSize1 : (b -> y) -> ( Field input output msg, b ) -> ( Field.State input, y )
sSize1 next form_ =
    Tuple.mapBoth Field.initialize next form_


init : ((() -> ()) -> form -> state) -> Form form -> State state
init size (Form form_) =
    State (size (\() -> ()) form_)



-- VALIDATING ALL FIELDS


validateAll : ((() -> () -> ()) -> form -> state -> results) -> Form form -> State state -> results
validateAll size (Form form_) (State state_) =
    size (\() () -> ()) form_ state_


validateSize1 : (rest -> rest1 -> rest2) -> ( Field input output msg, rest ) -> ( Field.State input, rest1 ) -> ( Result (List Field.Error) output, rest2 )
validateSize1 next form_ state_ =
    map2 parseAndValidate next form_ state_



-- COLLECTING THE RESULTS FROM ALL VALIDATED FIELDS INTO ONE RESULT


collectResults : ((a -> a) -> ( Result (List Field.Error) (), b ) -> ( c, d )) -> b -> c
collectResults size results =
    size identity ( Ok (), results )
        |> Tuple.first


collectSize1 : (( Result (List Field.Error) ( value, b ), c ) -> d) -> ( Result (List Field.Error) b, ( Result (List Field.Error) value, c ) ) -> d
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
    -> ( Field input output msg, rest )
    -> ( Field.State input, rest1 )
    -> ( Result (List Field.Error) output, rest2 )
    -> ( Element msg, rest3 )
renderSize1 next form_ state_ results =
    map3
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


toEls :
    ((() -> () -> ()) -> form -> state -> results)
    -> ((() -> () -> () -> ()) -> form -> state -> results -> elements)
    -> Form form
    -> State state
    -> elements
toEls validateSize renderSize form_ state_ =
    state_
        |> validateAll validateSize form_
        |> renderAll renderSize form_ state_
