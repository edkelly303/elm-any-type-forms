module Internals exposing (..)


type Form form
    = Form form


type State state
    = State state



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
