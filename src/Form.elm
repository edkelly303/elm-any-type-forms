module Form exposing
    ( Form
    , Index
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
    = State (Dict.Dict Int { touched : Bool }) state


type alias Index input delta output element msg restFields restFieldStates form state =
    (( Field input delta output element msg, restFields )
     -> ( Field.State input, restFieldStates )
     -> ( Field.State input, restFieldStates )
    )
    -> form
    -> state
    -> state



-- FORM CONFIG


type alias Config element msg =
    { layout : List element -> element
    , submitMsg : Maybe msg
    , submitRenderer : msg -> element
    }


defaultConfig : Config (Element msg) msg
defaultConfig =
    { layout = Element.column [ Element.spacing 10, Element.padding 10 ]
    , submitMsg = Nothing
    , submitRenderer =
        \msg ->
            Element.Input.button
                [ Element.padding 10
                , Element.Border.width 1
                , Element.Border.rounded 5
                ]
                { label = Element.text "Submit", onPress = Just msg }
    }


withRenderer : { layout : List element2 -> element2, submit : msg -> element2 } -> Config element msg -> Config element2 msg
withRenderer args config =
    { layout = args.layout, submitRenderer = args.submit, submitMsg = config.submitMsg }


withSubmit : msg -> Config element msg -> Config element msg
withSubmit msg config =
    { config | submitMsg = Just msg }



-- CREATING FORMS


form :
    { a | submitMsg : Maybe b, submitRenderer : b -> c, layout : List c -> d }
    ->
        ({ stateSize : e -> e
         , validateSize : f -> f
         , collectSize : g -> g
         , reverseSize : h -> h
         , renderSize : i -> i
         , collectElementsSize : j -> j
         , countSize : k -> k
         }
         ->
            { l
                | stateSize : (() -> ()) -> form -> m
                , validateSize : (() -> () -> ()) -> form -> state -> restResults
                , collectSize : (n -> n) -> ( Result (List Field.Error) (), restResults ) -> ( Result o p, q )
                , reverseSize : (r -> r) -> ( (), p ) -> ( s, t )
                , renderSize : (u -> () -> () -> () -> ()) -> Dict.Dict Int { touched : Bool } -> form -> state -> restResults -> v
                , collectElementsSize : (w -> w) -> ( List element, v ) -> ( List c, x )
                , countSize : (y -> y) -> ( Int, form ) -> ( Int, z )
            }
        )
    ->
        (( ( Form form, Int )
           ->
            { init : State m
            , submit : State state -> Result (State state) s
            , update :
                (( b1 -> b1, Int -> Int )
                 ->
                    ( (( Field c1 d1 output e1 msg, f1 )
                       -> ( { g1 | input : c1 }, h1 )
                       -> ( { g1 | input : c1 }, h1 )
                      )
                      -> form
                      -> j1
                      -> k1
                    , Int -> Int
                    )
                )
                -> d1
                -> State j1
                -> State k1
            , viewFields : State state -> v
            , view : State state -> d
            }
         , (( Form (), Int ) -> m1) -> m1
         )
         -> n1
        )
    -> n1
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
            , viewFields = viewElements validateSize renderSize form_
            , view = view config validateSize renderSize collectElementsSize form_
            }
        )
        next


end : ( state, state -> output ) -> output
end =
    Internals.end


f1 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, g ), h ) -> i
        , reverseSize : j -> ( ( k, l ), m ) -> n
        , renderSize :
            o
            -> Dict.Dict Int { p | touched : Bool }
            -> rest
            -> rest1
            -> rest2
            -> rest3
        , collectElementsSize : q -> ( List element, restElements ) -> next
        , countSize : r -> ( Int, s ) -> t
    }
    ->
        { stateSize : b -> ( Field input delta output u msg, restFields ) -> ( Field.State input, restFieldStates )
        , validateSize : c -> ( Field v w x y z, d ) -> ( Field.State v, e ) -> ( Result (List Field.Error) x, restResults )
        , collectSize : f -> ( Result (List Field.Error) g, ( Result (List Field.Error) value, h ) ) -> i
        , reverseSize : j -> ( l, ( k, m ) ) -> n
        , renderSize :
            o
            -> Dict.Dict Int { p | touched : Bool }
            -> ( Field a1 b1 c1 d1 e1, rest )
            -> ( { f1 | input : a1 }, rest1 )
            -> ( Result (List Field.Error) c1, rest2 )
            -> ( d1, rest3 )
        , collectElementsSize : q -> ( List element, ( element, restElements ) ) -> next
        , countSize : r -> ( Int, ( g1, s ) ) -> t
        }
f1 { stateSize, validateSize, collectSize, reverseSize, renderSize, collectElementsSize, countSize } =
    { stateSize = stateSize >> stateSize1
    , validateSize = validateSize >> validateSize1
    , collectSize = collectSize >> collectSize1
    , reverseSize = reverseSize >> reverseSize1
    , renderSize = renderSize >> renderSize1
    , collectElementsSize = collectElementsSize >> collectElementsSize1
    , countSize = countSize >> countSize1
    }


f2 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, h ) ), i ) -> j
        , reverseSize : k -> ( ( l, ( m, n ) ), o ) -> p
        , renderSize : q -> Dict.Dict Int { r | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : s -> ( List element, restElements ) -> next
        , countSize : t -> ( Int, u ) -> v
    }
    ->
        { stateSize : b -> ( Field input delta output w msg, ( Field x y z a1 b1, restFields ) ) -> ( Field.State input, ( Field.State x, restFieldStates ) )
        , validateSize : c -> ( Field c1 d1 e1 f1 g1, ( Field h1 i1 j1 k1 l1, d ) ) -> ( Field.State c1, ( Field.State h1, e ) ) -> ( Result (List Field.Error) e1, ( Result (List Field.Error) j1, restResults ) )
        , collectSize : f -> ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, i ) ) ) -> j
        , reverseSize : k -> ( n, ( m, ( l, o ) ) ) -> p
        , renderSize : q -> Dict.Dict Int { r | touched : Bool } -> ( Field n1 o1 p1 q1 r1, ( Field s1 t1 u1 v1 w1, rest ) ) -> ( { x1 | input : n1 }, ( { y1 | input : s1 }, rest1 ) ) -> ( Result (List Field.Error) p1, ( Result (List Field.Error) u1, rest2 ) ) -> ( q1, ( v1, rest3 ) )
        , collectElementsSize : s -> ( List element, ( element, ( element, restElements ) ) ) -> next
        , countSize : t -> ( Int, ( z1, ( a2, u ) ) ) -> v
        }
f2 =
    f1 >> f1


f3 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, i ) ) ), j ) -> k
        , reverseSize : l -> ( ( m, ( n, ( o, p ) ) ), q ) -> r
        , renderSize : s -> Dict.Dict Int { t | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : u -> ( List element, restElements ) -> next
        , countSize : v -> ( Int, w ) -> x
    }
    ->
        { stateSize : b -> ( Field input delta output y msg, ( Field z a1 b1 c1 d1, ( Field e1 f1 g1 h1 i1, restFields ) ) ) -> ( Field.State input, ( Field.State z, ( Field.State e1, restFieldStates ) ) )
        , validateSize : c -> ( Field j1 k1 l1 m1 n1, ( Field o1 p1 q1 r1 s1, ( Field t1 u1 v1 w1 x1, d ) ) ) -> ( Field.State j1, ( Field.State o1, ( Field.State t1, e ) ) ) -> ( Result (List Field.Error) l1, ( Result (List Field.Error) q1, ( Result (List Field.Error) v1, restResults ) ) )
        , collectSize : f -> ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, j ) ) ) ) -> k
        , reverseSize : l -> ( p, ( o, ( n, ( m, q ) ) ) ) -> r
        , renderSize : s -> Dict.Dict Int { t | touched : Bool } -> ( Field z1 a2 b2 c2 d2, ( Field e2 f2 g2 h2 i2, ( Field j2 k2 l2 m2 n2, rest ) ) ) -> ( { o2 | input : z1 }, ( { p2 | input : e2 }, ( { q2 | input : j2 }, rest1 ) ) ) -> ( Result (List Field.Error) b2, ( Result (List Field.Error) g2, ( Result (List Field.Error) l2, rest2 ) ) ) -> ( c2, ( h2, ( m2, rest3 ) ) )
        , collectElementsSize : u -> ( List element, ( element, ( element, ( element, restElements ) ) ) ) -> next
        , countSize : v -> ( Int, ( r2, ( s2, ( t2, w ) ) ) ) -> x
        }
f3 =
    f2 >> f1


f4 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, j ) ) ) ), k ) -> l
        , reverseSize : m -> ( ( n, ( o, ( p, ( q, r ) ) ) ), s ) -> t
        , renderSize : u -> Dict.Dict Int { v | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : w -> ( List element, restElements ) -> next
        , countSize : x -> ( Int, y ) -> z
    }
    ->
        { stateSize : b -> ( Field input delta output a1 msg, ( Field b1 c1 d1 e1 f1, ( Field g1 h1 i1 j1 k1, ( Field l1 m1 n1 o1 p1, restFields ) ) ) ) -> ( Field.State input, ( Field.State b1, ( Field.State g1, ( Field.State l1, restFieldStates ) ) ) )
        , validateSize : c -> ( Field q1 r1 s1 t1 u1, ( Field v1 w1 x1 y1 z1, ( Field a2 b2 c2 d2 e2, ( Field f2 g2 h2 i2 j2, d ) ) ) ) -> ( Field.State q1, ( Field.State v1, ( Field.State a2, ( Field.State f2, e ) ) ) ) -> ( Result (List Field.Error) s1, ( Result (List Field.Error) x1, ( Result (List Field.Error) c2, ( Result (List Field.Error) h2, restResults ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, k ) ) ) ) ) -> l
        , reverseSize : m -> ( r, ( q, ( p, ( o, ( n, s ) ) ) ) ) -> t
        , renderSize : u -> Dict.Dict Int { v | touched : Bool } -> ( Field l2 m2 n2 o2 p2, ( Field q2 r2 s2 t2 u2, ( Field v2 w2 x2 y2 z2, ( Field a3 b3 c3 d3 e3, rest ) ) ) ) -> ( { f3 | input : l2 }, ( { g3 | input : q2 }, ( { h3 | input : v2 }, ( { i3 | input : a3 }, rest1 ) ) ) ) -> ( Result (List Field.Error) n2, ( Result (List Field.Error) s2, ( Result (List Field.Error) x2, ( Result (List Field.Error) c3, rest2 ) ) ) ) -> ( o2, ( t2, ( y2, ( d3, rest3 ) ) ) )
        , collectElementsSize : w -> ( List element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) -> next
        , countSize : x -> ( Int, ( j3, ( k3, ( l3, ( m3, y ) ) ) ) ) -> z
        }
f4 =
    f3 >> f1


f5 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, ( j, k ) ) ) ) ), l ) -> m
        , reverseSize : n -> ( ( o, ( p, ( q, ( r, ( s, t ) ) ) ) ), u ) -> v
        , renderSize : w -> Dict.Dict Int { x | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : y -> ( List element, restElements ) -> next
        , countSize : z -> ( Int, a1 ) -> b1
    }
    ->
        { stateSize : b -> ( Field input delta output c1 msg, ( Field d1 e1 f1 g1 h1, ( Field i1 j1 k1 l1 m1, ( Field n1 o1 p1 q1 r1, ( Field s1 t1 u1 v1 w1, restFields ) ) ) ) ) -> ( Field.State input, ( Field.State d1, ( Field.State i1, ( Field.State n1, ( Field.State s1, restFieldStates ) ) ) ) )
        , validateSize : c -> ( Field x1 y1 z1 a2 b2, ( Field c2 d2 e2 f2 g2, ( Field h2 i2 j2 k2 l2, ( Field m2 n2 o2 p2 q2, ( Field r2 s2 t2 u2 v2, d ) ) ) ) ) -> ( Field.State x1, ( Field.State c2, ( Field.State h2, ( Field.State m2, ( Field.State r2, e ) ) ) ) ) -> ( Result (List Field.Error) z1, ( Result (List Field.Error) e2, ( Result (List Field.Error) j2, ( Result (List Field.Error) o2, ( Result (List Field.Error) t2, restResults ) ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) k, ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, l ) ) ) ) ) ) -> m
        , reverseSize : n -> ( t, ( s, ( r, ( q, ( p, ( o, u ) ) ) ) ) ) -> v
        , renderSize : w -> Dict.Dict Int { x | touched : Bool } -> ( Field x2 y2 z2 a3 b3, ( Field c3 d3 e3 f3 g3, ( Field h3 i3 j3 k3 l3, ( Field m3 n3 o3 p3 q3, ( Field r3 s3 t3 u3 v3, rest ) ) ) ) ) -> ( { w3 | input : x2 }, ( { x3 | input : c3 }, ( { y3 | input : h3 }, ( { z3 | input : m3 }, ( { a4 | input : r3 }, rest1 ) ) ) ) ) -> ( Result (List Field.Error) z2, ( Result (List Field.Error) e3, ( Result (List Field.Error) j3, ( Result (List Field.Error) o3, ( Result (List Field.Error) t3, rest2 ) ) ) ) ) -> ( a3, ( f3, ( k3, ( p3, ( u3, rest3 ) ) ) ) )
        , collectElementsSize : y -> ( List element, ( element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) ) -> next
        , countSize : z -> ( Int, ( b4, ( c4, ( d4, ( e4, ( f4, a1 ) ) ) ) ) ) -> b1
        }
f5 =
    f4 >> f1


f6 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, ( j, ( k, l ) ) ) ) ) ), m ) -> n
        , reverseSize : o -> ( ( p, ( q, ( r, ( s, ( t, ( u, v ) ) ) ) ) ), w ) -> x
        , renderSize : y -> Dict.Dict Int { z | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : a1 -> ( List element, restElements ) -> next
        , countSize : b1 -> ( Int, c1 ) -> d1
    }
    ->
        { stateSize : b -> ( Field input delta output e1 msg, ( Field f1 g1 h1 i1 j1, ( Field k1 l1 m1 n1 o1, ( Field p1 q1 r1 s1 t1, ( Field u1 v1 w1 x1 y1, ( Field z1 a2 b2 c2 d2, restFields ) ) ) ) ) ) -> ( Field.State input, ( Field.State f1, ( Field.State k1, ( Field.State p1, ( Field.State u1, ( Field.State z1, restFieldStates ) ) ) ) ) )
        , validateSize : c -> ( Field e2 f2 g2 h2 i2, ( Field j2 k2 l2 m2 n2, ( Field o2 p2 q2 r2 s2, ( Field t2 u2 v2 w2 x2, ( Field y2 z2 a3 b3 c3, ( Field d3 e3 f3 g3 h3, d ) ) ) ) ) ) -> ( Field.State e2, ( Field.State j2, ( Field.State o2, ( Field.State t2, ( Field.State y2, ( Field.State d3, e ) ) ) ) ) ) -> ( Result (List Field.Error) g2, ( Result (List Field.Error) l2, ( Result (List Field.Error) q2, ( Result (List Field.Error) v2, ( Result (List Field.Error) a3, ( Result (List Field.Error) f3, restResults ) ) ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) l, ( Result (List Field.Error) k, ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, m ) ) ) ) ) ) ) -> n
        , reverseSize : o -> ( v, ( u, ( t, ( s, ( r, ( q, ( p, w ) ) ) ) ) ) ) -> x
        , renderSize : y -> Dict.Dict Int { z | touched : Bool } -> ( Field j3 k3 l3 m3 n3, ( Field o3 p3 q3 r3 s3, ( Field t3 u3 v3 w3 x3, ( Field y3 z3 a4 b4 c4, ( Field d4 e4 f4 g4 h4, ( Field i4 j4 k4 l4 m4, rest ) ) ) ) ) ) -> ( { n4 | input : j3 }, ( { o4 | input : o3 }, ( { p4 | input : t3 }, ( { q4 | input : y3 }, ( { r4 | input : d4 }, ( { s4 | input : i4 }, rest1 ) ) ) ) ) ) -> ( Result (List Field.Error) l3, ( Result (List Field.Error) q3, ( Result (List Field.Error) v3, ( Result (List Field.Error) a4, ( Result (List Field.Error) f4, ( Result (List Field.Error) k4, rest2 ) ) ) ) ) ) -> ( m3, ( r3, ( w3, ( b4, ( g4, ( l4, rest3 ) ) ) ) ) )
        , collectElementsSize : a1 -> ( List element, ( element, ( element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) ) ) -> next
        , countSize : b1 -> ( Int, ( t4, ( u4, ( v4, ( w4, ( x4, ( y4, c1 ) ) ) ) ) ) ) -> d1
        }
f6 =
    f5 >> f1


f7 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, ( j, ( k, ( l, m ) ) ) ) ) ) ), n ) -> o
        , reverseSize : p -> ( ( q, ( r, ( s, ( t, ( u, ( v, ( w, x ) ) ) ) ) ) ), y ) -> z
        , renderSize : a1 -> Dict.Dict Int { b1 | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : c1 -> ( List element, restElements ) -> next
        , countSize : d1 -> ( Int, e1 ) -> f1
    }
    ->
        { stateSize : b -> ( Field input delta output g1 msg, ( Field h1 i1 j1 k1 l1, ( Field m1 n1 o1 p1 q1, ( Field r1 s1 t1 u1 v1, ( Field w1 x1 y1 z1 a2, ( Field b2 c2 d2 e2 f2, ( Field g2 h2 i2 j2 k2, restFields ) ) ) ) ) ) ) -> ( Field.State input, ( Field.State h1, ( Field.State m1, ( Field.State r1, ( Field.State w1, ( Field.State b2, ( Field.State g2, restFieldStates ) ) ) ) ) ) )
        , validateSize : c -> ( Field l2 m2 n2 o2 p2, ( Field q2 r2 s2 t2 u2, ( Field v2 w2 x2 y2 z2, ( Field a3 b3 c3 d3 e3, ( Field f3 g3 h3 i3 j3, ( Field k3 l3 m3 n3 o3, ( Field p3 q3 r3 s3 t3, d ) ) ) ) ) ) ) -> ( Field.State l2, ( Field.State q2, ( Field.State v2, ( Field.State a3, ( Field.State f3, ( Field.State k3, ( Field.State p3, e ) ) ) ) ) ) ) -> ( Result (List Field.Error) n2, ( Result (List Field.Error) s2, ( Result (List Field.Error) x2, ( Result (List Field.Error) c3, ( Result (List Field.Error) h3, ( Result (List Field.Error) m3, ( Result (List Field.Error) r3, restResults ) ) ) ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) m, ( Result (List Field.Error) l, ( Result (List Field.Error) k, ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, n ) ) ) ) ) ) ) ) -> o
        , reverseSize : p -> ( x, ( w, ( v, ( u, ( t, ( s, ( r, ( q, y ) ) ) ) ) ) ) ) -> z
        , renderSize : a1 -> Dict.Dict Int { b1 | touched : Bool } -> ( Field v3 w3 x3 y3 z3, ( Field a4 b4 c4 d4 e4, ( Field f4 g4 h4 i4 j4, ( Field k4 l4 m4 n4 o4, ( Field p4 q4 r4 s4 t4, ( Field u4 v4 w4 x4 y4, ( Field z4 a5 b5 c5 d5, rest ) ) ) ) ) ) ) -> ( { e5 | input : v3 }, ( { f5 | input : a4 }, ( { g5 | input : f4 }, ( { h5 | input : k4 }, ( { i5 | input : p4 }, ( { j5 | input : u4 }, ( { k5 | input : z4 }, rest1 ) ) ) ) ) ) ) -> ( Result (List Field.Error) x3, ( Result (List Field.Error) c4, ( Result (List Field.Error) h4, ( Result (List Field.Error) m4, ( Result (List Field.Error) r4, ( Result (List Field.Error) w4, ( Result (List Field.Error) b5, rest2 ) ) ) ) ) ) ) -> ( y3, ( d4, ( i4, ( n4, ( s4, ( x4, ( c5, rest3 ) ) ) ) ) ) )
        , collectElementsSize : c1 -> ( List element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) ) ) ) -> next
        , countSize : d1 -> ( Int, ( l5, ( m5, ( n5, ( o5, ( p5, ( q5, ( r5, e1 ) ) ) ) ) ) ) ) -> f1
        }
f7 =
    f6 >> f1


f8 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, ( j, ( k, ( l, ( m, n ) ) ) ) ) ) ) ), o ) -> p
        , reverseSize : q -> ( ( r, ( s, ( t, ( u, ( v, ( w, ( x, ( y, z ) ) ) ) ) ) ) ), a1 ) -> b1
        , renderSize : c1 -> Dict.Dict Int { d1 | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : e1 -> ( List element, restElements ) -> next
        , countSize : f1 -> ( Int, g1 ) -> h1
    }
    ->
        { stateSize : b -> ( Field input delta output i1 msg, ( Field j1 k1 l1 m1 n1, ( Field o1 p1 q1 r1 s1, ( Field t1 u1 v1 w1 x1, ( Field y1 z1 a2 b2 c2, ( Field d2 e2 f2 g2 h2, ( Field i2 j2 k2 l2 m2, ( Field n2 o2 p2 q2 r2, restFields ) ) ) ) ) ) ) ) -> ( Field.State input, ( Field.State j1, ( Field.State o1, ( Field.State t1, ( Field.State y1, ( Field.State d2, ( Field.State i2, ( Field.State n2, restFieldStates ) ) ) ) ) ) ) )
        , validateSize : c -> ( Field s2 t2 u2 v2 w2, ( Field x2 y2 z2 a3 b3, ( Field c3 d3 e3 f3 g3, ( Field h3 i3 j3 k3 l3, ( Field m3 n3 o3 p3 q3, ( Field r3 s3 t3 u3 v3, ( Field w3 x3 y3 z3 a4, ( Field b4 c4 d4 e4 f4, d ) ) ) ) ) ) ) ) -> ( Field.State s2, ( Field.State x2, ( Field.State c3, ( Field.State h3, ( Field.State m3, ( Field.State r3, ( Field.State w3, ( Field.State b4, e ) ) ) ) ) ) ) ) -> ( Result (List Field.Error) u2, ( Result (List Field.Error) z2, ( Result (List Field.Error) e3, ( Result (List Field.Error) j3, ( Result (List Field.Error) o3, ( Result (List Field.Error) t3, ( Result (List Field.Error) y3, ( Result (List Field.Error) d4, restResults ) ) ) ) ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) n, ( Result (List Field.Error) m, ( Result (List Field.Error) l, ( Result (List Field.Error) k, ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, o ) ) ) ) ) ) ) ) ) -> p
        , reverseSize : q -> ( z, ( y, ( x, ( w, ( v, ( u, ( t, ( s, ( r, a1 ) ) ) ) ) ) ) ) ) -> b1
        , renderSize : c1 -> Dict.Dict Int { d1 | touched : Bool } -> ( Field h4 i4 j4 k4 l4, ( Field m4 n4 o4 p4 q4, ( Field r4 s4 t4 u4 v4, ( Field w4 x4 y4 z4 a5, ( Field b5 c5 d5 e5 f5, ( Field g5 h5 i5 j5 k5, ( Field l5 m5 n5 o5 p5, ( Field q5 r5 s5 t5 u5, rest ) ) ) ) ) ) ) ) -> ( { v5 | input : h4 }, ( { w5 | input : m4 }, ( { x5 | input : r4 }, ( { y5 | input : w4 }, ( { z5 | input : b5 }, ( { a6 | input : g5 }, ( { b6 | input : l5 }, ( { c6 | input : q5 }, rest1 ) ) ) ) ) ) ) ) -> ( Result (List Field.Error) j4, ( Result (List Field.Error) o4, ( Result (List Field.Error) t4, ( Result (List Field.Error) y4, ( Result (List Field.Error) d5, ( Result (List Field.Error) i5, ( Result (List Field.Error) n5, ( Result (List Field.Error) s5, rest2 ) ) ) ) ) ) ) ) -> ( k4, ( p4, ( u4, ( z4, ( e5, ( j5, ( o5, ( t5, rest3 ) ) ) ) ) ) ) )
        , collectElementsSize : e1 -> ( List element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) ) ) ) ) -> next
        , countSize : f1 -> ( Int, ( d6, ( e6, ( f6, ( g6, ( h6, ( i6, ( j6, ( k6, g1 ) ) ) ) ) ) ) ) ) -> h1
        }
f8 =
    f7 >> f1


f9 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, ( j, ( k, ( l, ( m, ( n, o ) ) ) ) ) ) ) ) ), p ) -> q
        , reverseSize : r -> ( ( s, ( t, ( u, ( v, ( w, ( x, ( y, ( z, ( a1, b1 ) ) ) ) ) ) ) ) ), c1 ) -> d1
        , renderSize : e1 -> Dict.Dict Int { f1 | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : g1 -> ( List element, restElements ) -> next
        , countSize : h1 -> ( Int, i1 ) -> j1
    }
    ->
        { stateSize : b -> ( Field input delta output k1 msg, ( Field l1 m1 n1 o1 p1, ( Field q1 r1 s1 t1 u1, ( Field v1 w1 x1 y1 z1, ( Field a2 b2 c2 d2 e2, ( Field f2 g2 h2 i2 j2, ( Field k2 l2 m2 n2 o2, ( Field p2 q2 r2 s2 t2, ( Field u2 v2 w2 x2 y2, restFields ) ) ) ) ) ) ) ) ) -> ( Field.State input, ( Field.State l1, ( Field.State q1, ( Field.State v1, ( Field.State a2, ( Field.State f2, ( Field.State k2, ( Field.State p2, ( Field.State u2, restFieldStates ) ) ) ) ) ) ) ) )
        , validateSize : c -> ( Field z2 a3 b3 c3 d3, ( Field e3 f3 g3 h3 i3, ( Field j3 k3 l3 m3 n3, ( Field o3 p3 q3 r3 s3, ( Field t3 u3 v3 w3 x3, ( Field y3 z3 a4 b4 c4, ( Field d4 e4 f4 g4 h4, ( Field i4 j4 k4 l4 m4, ( Field n4 o4 p4 q4 r4, d ) ) ) ) ) ) ) ) ) -> ( Field.State z2, ( Field.State e3, ( Field.State j3, ( Field.State o3, ( Field.State t3, ( Field.State y3, ( Field.State d4, ( Field.State i4, ( Field.State n4, e ) ) ) ) ) ) ) ) ) -> ( Result (List Field.Error) b3, ( Result (List Field.Error) g3, ( Result (List Field.Error) l3, ( Result (List Field.Error) q3, ( Result (List Field.Error) v3, ( Result (List Field.Error) a4, ( Result (List Field.Error) f4, ( Result (List Field.Error) k4, ( Result (List Field.Error) p4, restResults ) ) ) ) ) ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) o, ( Result (List Field.Error) n, ( Result (List Field.Error) m, ( Result (List Field.Error) l, ( Result (List Field.Error) k, ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, p ) ) ) ) ) ) ) ) ) ) -> q
        , reverseSize : r -> ( b1, ( a1, ( z, ( y, ( x, ( w, ( v, ( u, ( t, ( s, c1 ) ) ) ) ) ) ) ) ) ) -> d1
        , renderSize : e1 -> Dict.Dict Int { f1 | touched : Bool } -> ( Field t4 u4 v4 w4 x4, ( Field y4 z4 a5 b5 c5, ( Field d5 e5 f5 g5 h5, ( Field i5 j5 k5 l5 m5, ( Field n5 o5 p5 q5 r5, ( Field s5 t5 u5 v5 w5, ( Field x5 y5 z5 a6 b6, ( Field c6 d6 e6 f6 g6, ( Field h6 i6 j6 k6 l6, rest ) ) ) ) ) ) ) ) ) -> ( { m6 | input : t4 }, ( { n6 | input : y4 }, ( { o6 | input : d5 }, ( { p6 | input : i5 }, ( { q6 | input : n5 }, ( { r6 | input : s5 }, ( { s6 | input : x5 }, ( { t6 | input : c6 }, ( { u6 | input : h6 }, rest1 ) ) ) ) ) ) ) ) ) -> ( Result (List Field.Error) v4, ( Result (List Field.Error) a5, ( Result (List Field.Error) f5, ( Result (List Field.Error) k5, ( Result (List Field.Error) p5, ( Result (List Field.Error) u5, ( Result (List Field.Error) z5, ( Result (List Field.Error) e6, ( Result (List Field.Error) j6, rest2 ) ) ) ) ) ) ) ) ) -> ( w4, ( b5, ( g5, ( l5, ( q5, ( v5, ( a6, ( f6, ( k6, rest3 ) ) ) ) ) ) ) ) )
        , collectElementsSize : g1 -> ( List element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) ) ) ) ) ) -> next
        , countSize : h1 -> ( Int, ( v6, ( w6, ( x6, ( y6, ( z6, ( a7, ( b7, ( c7, ( d7, i1 ) ) ) ) ) ) ) ) ) ) -> j1
        }
f9 =
    f8 >> f1


f10 :
    { a
        | stateSize : b -> restFields -> restFieldStates
        , validateSize : c -> d -> e -> restResults
        , collectSize : f -> ( Result (List Field.Error) ( value, ( g, ( h, ( i, ( j, ( k, ( l, ( m, ( n, ( o, p ) ) ) ) ) ) ) ) ) ), q ) -> r
        , reverseSize : s -> ( ( t, ( u, ( v, ( w, ( x, ( y, ( z, ( a1, ( b1, ( c1, d1 ) ) ) ) ) ) ) ) ) ), e1 ) -> f1
        , renderSize : g1 -> Dict.Dict Int { h1 | touched : Bool } -> rest -> rest1 -> rest2 -> rest3
        , collectElementsSize : i1 -> ( List element, restElements ) -> next
        , countSize : j1 -> ( Int, k1 ) -> l1
    }
    ->
        { stateSize : b -> ( Field input delta output m1 msg, ( Field n1 o1 p1 q1 r1, ( Field s1 t1 u1 v1 w1, ( Field x1 y1 z1 a2 b2, ( Field c2 d2 e2 f2 g2, ( Field h2 i2 j2 k2 l2, ( Field m2 n2 o2 p2 q2, ( Field r2 s2 t2 u2 v2, ( Field w2 x2 y2 z2 a3, ( Field b3 c3 d3 e3 f3, restFields ) ) ) ) ) ) ) ) ) ) -> ( Field.State input, ( Field.State n1, ( Field.State s1, ( Field.State x1, ( Field.State c2, ( Field.State h2, ( Field.State m2, ( Field.State r2, ( Field.State w2, ( Field.State b3, restFieldStates ) ) ) ) ) ) ) ) ) )
        , validateSize : c -> ( Field g3 h3 i3 j3 k3, ( Field l3 m3 n3 o3 p3, ( Field q3 r3 s3 t3 u3, ( Field v3 w3 x3 y3 z3, ( Field a4 b4 c4 d4 e4, ( Field f4 g4 h4 i4 j4, ( Field k4 l4 m4 n4 o4, ( Field p4 q4 r4 s4 t4, ( Field u4 v4 w4 x4 y4, ( Field z4 a5 b5 c5 d5, d ) ) ) ) ) ) ) ) ) ) -> ( Field.State g3, ( Field.State l3, ( Field.State q3, ( Field.State v3, ( Field.State a4, ( Field.State f4, ( Field.State k4, ( Field.State p4, ( Field.State u4, ( Field.State z4, e ) ) ) ) ) ) ) ) ) ) -> ( Result (List Field.Error) i3, ( Result (List Field.Error) n3, ( Result (List Field.Error) s3, ( Result (List Field.Error) x3, ( Result (List Field.Error) c4, ( Result (List Field.Error) h4, ( Result (List Field.Error) m4, ( Result (List Field.Error) r4, ( Result (List Field.Error) w4, ( Result (List Field.Error) b5, restResults ) ) ) ) ) ) ) ) ) )
        , collectSize : f -> ( Result (List Field.Error) p, ( Result (List Field.Error) o, ( Result (List Field.Error) n, ( Result (List Field.Error) m, ( Result (List Field.Error) l, ( Result (List Field.Error) k, ( Result (List Field.Error) j, ( Result (List Field.Error) i, ( Result (List Field.Error) h, ( Result (List Field.Error) g, ( Result (List Field.Error) value, q ) ) ) ) ) ) ) ) ) ) ) -> r
        , reverseSize : s -> ( d1, ( c1, ( b1, ( a1, ( z, ( y, ( x, ( w, ( v, ( u, ( t, e1 ) ) ) ) ) ) ) ) ) ) ) -> f1
        , renderSize : g1 -> Dict.Dict Int { h1 | touched : Bool } -> ( Field f5 g5 h5 i5 j5, ( Field k5 l5 m5 n5 o5, ( Field p5 q5 r5 s5 t5, ( Field u5 v5 w5 x5 y5, ( Field z5 a6 b6 c6 d6, ( Field e6 f6 g6 h6 i6, ( Field j6 k6 l6 m6 n6, ( Field o6 p6 q6 r6 s6, ( Field t6 u6 v6 w6 x6, ( Field y6 z6 a7 b7 c7, rest ) ) ) ) ) ) ) ) ) ) -> ( { d7 | input : f5 }, ( { e7 | input : k5 }, ( { f7 | input : p5 }, ( { g7 | input : u5 }, ( { h7 | input : z5 }, ( { i7 | input : e6 }, ( { j7 | input : j6 }, ( { k7 | input : o6 }, ( { l7 | input : t6 }, ( { m7 | input : y6 }, rest1 ) ) ) ) ) ) ) ) ) ) -> ( Result (List Field.Error) h5, ( Result (List Field.Error) m5, ( Result (List Field.Error) r5, ( Result (List Field.Error) w5, ( Result (List Field.Error) b6, ( Result (List Field.Error) g6, ( Result (List Field.Error) l6, ( Result (List Field.Error) q6, ( Result (List Field.Error) v6, ( Result (List Field.Error) a7, rest2 ) ) ) ) ) ) ) ) ) ) -> ( i5, ( n5, ( s5, ( x5, ( c6, ( h6, ( m6, ( r6, ( w6, ( b7, rest3 ) ) ) ) ) ) ) ) ) )
        , collectElementsSize : i1 -> ( List element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, ( element, restElements ) ) ) ) ) ) ) ) ) ) ) -> next
        , countSize : j1 -> ( Int, ( n7, ( o7, ( p7, ( q7, ( r7, ( s7, ( t7, ( u7, ( v7, ( w7, k1 ) ) ) ) ) ) ) ) ) ) ) -> l1
        }
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


update :
    Int
    -> Form form
    ->
        (( a -> a, Int -> Int )
         ->
            ( (( Field b c output element msg, d )
               -> ( { e | input : b }, f )
               -> ( { e | input : b }, f )
              )
              -> form
              -> state
              -> h
            , Int -> Int
            )
        )
    -> c
    -> State state
    -> State h
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
        (Dict.insert indexOfUpdatedField { touched = True } dict)
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
            List.foldl (\i d -> Dict.insert i { touched = False } d) Dict.empty range
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


renderAll :
    ((a -> () -> () -> () -> ())
     -> Dict.Dict Int { touched : Bool }
     -> form
     -> state
     -> d
     -> b
    )
    -> Form form
    -> State state
    -> d
    -> b
renderAll size (Form form_) (State internalState state_) results =
    size (\_ () () () -> ()) internalState form_ state_ results


renderSize1 :
    (Dict.Dict Int { a | touched : Bool } -> rest -> rest1 -> rest2 -> rest3)
    -> Dict.Dict Int { a | touched : Bool }
    -> ( Field b delta output element msg, rest )
    -> ( { c | input : b }, rest1 )
    -> ( Result (List Field.Error) output, rest2 )
    -> ( element, rest3 )
renderSize1 next internalState form_ state_ results =
    Internals.mapBoth3
        (\(Field { index, renderer, msg, id, label }) { input } parsed ->
            renderer
                { input = input
                , touched =
                    Dict.get index internalState
                        |> Maybe.map .touched
                        |> Maybe.withDefault False
                , msg = msg
                , parsed = parsed
                , id = id
                , label = label
                }
        )
        (next internalState)
        form_
        state_
        results


viewElements :
    ((() -> () -> ()) -> form -> state -> d)
    ->
        ((a -> () -> () -> () -> ())
         -> Dict.Dict Int { touched : Bool }
         -> form
         -> state
         -> d
         -> b
        )
    -> Form form
    -> State state
    -> b
viewElements validateSize renderSize form_ state_ =
    state_
        |> validateAll validateSize form_
        |> renderAll renderSize form_ state_


collectElements : ((a -> a) -> ( List element, restElements ) -> ( d, e )) -> restElements -> d
collectElements size elements =
    size identity ( [], elements )
        |> Tuple.first


collectElementsSize1 : (( List element, restElements ) -> next) -> ( List element, ( element, restElements ) ) -> next
collectElementsSize1 next ( s, ( fst, rst ) ) =
    next ( fst :: s, rst )


view : { a | submitMsg : Maybe b, submitRenderer : b -> c, layout : List c -> d } -> ((() -> () -> ()) -> form -> state -> e) -> ((f -> () -> () -> () -> ()) -> Dict.Dict Int { touched : Bool } -> form -> state -> e -> restElements) -> ((g -> g) -> ( List element, restElements ) -> ( List c, h )) -> Form form -> State state -> d
view config validateSize renderSize collectElementsSize form_ state_ =
    viewElements validateSize renderSize form_ state_
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
