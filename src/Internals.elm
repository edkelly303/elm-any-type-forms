module Internals exposing (..)

-- NASTY TUPLE STUFF


map2 : (this0 -> this1 -> this2) -> (rest0 -> rest1 -> rest2) -> ( this0, rest0 ) -> ( this1, rest1 ) -> ( this2, rest2 )
map2 mapThis mapRest ( this0, rest0 ) ( this1, rest1 ) =
    ( mapThis this0 this1, mapRest rest0 rest1 )


map3 : (a -> b -> c -> d) -> (rest -> rest1 -> rest2 -> rest3) -> ( a, rest ) -> ( b, rest1 ) -> ( c, rest2 ) -> ( d, rest3 )
map3 mapA mapB ( a, b ) ( a1, b1 ) ( a2, b2 ) =
    ( mapA a a1 a2, mapB b b1 b2 )



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
