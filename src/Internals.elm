module Internals exposing (..)

-- NASTY TUPLE STUFF


mapBoth2 : (this0 -> this1 -> this2) -> (rest0 -> rest1 -> rest2) -> ( this0, rest0 ) -> ( this1, rest1 ) -> ( this2, rest2 )
mapBoth2 mapThis mapRest ( this0, rest0 ) ( this1, rest1 ) =
    ( mapThis this0 this1, mapRest rest0 rest1 )


mapBoth3 : (a -> b -> c -> d) -> (rest -> rest1 -> rest2 -> rest3) -> ( a, rest ) -> ( b, rest1 ) -> ( c, rest2 ) -> ( d, rest3 )
mapBoth3 mapA mapB ( a, b ) ( a1, b1 ) ( a2, b2 ) =
    ( mapA a a1 a2, mapB b b1 b2 )
