module X exposing (..)


type End
    = End


start =
    { state = identity
    , updater = identity
    , before = identity
    , befores = identity
    , after = End
    , afters = End
    , makeSetters = identity
    }


field f rec =
    { state = rec.state << Tuple.pair f
    , updater = rec.updater >> recordStateUpdater
    , before = rec.before << Tuple.pair Nothing
    , befores = rec.befores << Tuple.pair rec.before
    , after = ( Nothing, rec.after )
    , afters = ( rec.after, rec.afters )
    , makeSetters = rec.makeSetters >> setterMaker
    }


makeSetters makeSetters_ befores afters =
    makeSetters_ (\End End -> End) (befores End) afters


setterMaker next ( before, befores ) ( after, afters ) =
    ( \value -> before ( Just value, after )
    , next befores afters
    )


end rec =
    let
        setters =
            makeSetters rec.makeSetters rec.befores rec.afters
    in
    { update = \delta state_ -> updateRecordStates rec.updater setters delta state_
    , state = rec.state End
    }


example =
    start
        |> field 1
        |> field "2"
        |> field 2.5
        |> end


updateRecordStates updater setters deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output _ End End -> output)
                { newStates = identity, newCmds = [] }
                setters
                deltas
                states
    in
    ( newStates End, Cmd.batch newCmds )


recordStateUpdater next { newStates, newCmds } ( setter, restSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        ( newState, newCmd ) =
            ( case delta of
                Nothing ->
                    state

                Just deltaPayload ->
                    deltaPayload
            , Cmd.none
            )

        cmd2 =
            Cmd.map setter newCmd
    in
    next
        { newStates = newStates << Tuple.pair newState
        , newCmds = cmd2 :: newCmds
        }
        restSetters
        restDeltas
        restStates
