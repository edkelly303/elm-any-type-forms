module Poc exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Process
import Task
import Time



-- USERLAND CODE
-- Userland type definitions


type alias Form =
    ( Field Int Int Int, ( Field Float Float Float, ( Field String String Int, End ) ) )


type Msg
    = FormMsg Form


type alias User =
    { age : Int, height : Float, weight : Int }



-- Userland form definitions


form =
    new User FormMsg
        |> field f0 int
        |> field f1 float
        |> field f2 string
        |> end



-- Userland widget definitions


int =
    { init = 100
    , update = \delta state -> state // delta
    , view = \{ toMsg, state } -> div [] [button [ onClick (toMsg 2) ] [ text (String.fromInt state) ]]
    , parse = Ok
    }


float =
    { init = 0.1
    , update = \delta state -> state + delta
    , view = \{ toMsg, state } -> div [] [button [ onClick (toMsg 2.1) ] [ text (String.fromFloat state) ]]
    , parse = \_ -> Err ["uh oh"]
    }


string =
    { init = ""
    , update = \delta state -> delta
    , view = \{ toMsg, state, output } -> 
        div [] 
            [ input 
                [ onInput toMsg
                , HA.value state 
                , HA.style "background-color" (case output of 
                    Ok _ -> "paleGreen"
                    Err _ -> "pink")
                ] 
                [ text state ]
            , case output of
                Ok _ -> 
                    text ""
                Err errs -> 
                    div [] (List.map text errs)
            ]
    , parse =
        \state ->
            case String.toInt state of
                Just i ->
                    Ok i

                Nothing ->
                    Err [ "not an int" ]
    }



-- Userland program plumbing


update : Msg -> Form -> ( Form, Cmd Msg )
update msg model =
    case msg of
        FormMsg payload ->
            form.update payload model


view : Form -> Html Msg
view model =
    let
        _ =
            Debug.log "User" (form.submit model)
    in
    div [] (form.view model)


main : Program () Form Msg
main =
    Browser.element
        { init = \() -> form.init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- LIBRARY CODE
-- Types


type End
    = End


type alias Field state delta output =
    { state : state
    , delta : Delta delta
    , output : Result (List String) output
    , lastTouched : Maybe Time.Posix
    }


type Delta delta
    = Update delta
    | StartDebouncing Time.Posix
    | ParseIfDebounced Time.Posix
    | Noop


type alias FieldBuilder state delta output element msg =
    { init : state
    , update : delta -> state -> state
    , view : { state : state, toMsg : delta -> msg } -> element
    , parse : state -> Result (List String) output
    }



-- Field setters


f0 =
    identity


f1 =
    Tuple.mapSecond


f2 =
    f1 >> f1



-- Step functions


update_ updater fields deltas states =
    updater (\End End End -> End) fields deltas states


updater1 next ( field_, fields ) ( delta, deltas ) ( state, states ) =
    ( case delta.delta of
        Update d ->
            ( { state | state = field_.update d state.state }
            , Task.perform (field_.toDelta << StartDebouncing) Time.now
            )

        StartDebouncing now ->
            ( { state | lastTouched = Just now }
            , Task.perform (\() -> field_.toDelta (ParseIfDebounced now)) (Process.sleep 1000)
            )

        ParseIfDebounced now ->
            ( if state.lastTouched == Just now then
                { state | output = field_.parse state.state }

              else
                state
            , Cmd.none
            )

        Noop ->
            ( state
            , Cmd.none
            )
    , next fields deltas states
    )


cmdStrip cmdStripper states0 =
    let
        ( ( states1, cmds ), _ ) =
            cmdStripper ( ( End, [] ), states0 )
    in
    ( states1, Cmd.batch cmds )


cmdStripper1 ( ( outState, outCmd ), ( ( state, cmd ), statesAndCmds ) ) =
    ( ( ( state, outState ), cmd :: outCmd ), statesAndCmds )


view_ viewer toMsg fields states =
    viewer (\_ End End -> End) toMsg fields states


viewer1 next toMsg ( field_, fields ) ( state, states ) =
    ( field_.view
        { toMsg = \x -> toMsg (field_.toDelta (Update x))
        , state = state.state
        , output = state.output
        }
    , next toMsg fields states
    )


combine combiner inits fields =
    combiner (\_ End -> End) inits fields


combiner1 next inits ( field_, fields ) =
    let
        set x =
            Tuple.mapFirst (\s -> { s | delta = x })
    in
    ( { update = field_.update
      , view = field_.view
      , parse = field_.parse
      , toDelta = \x -> field_.setter (set x) inits
      }
    , next inits fields
    )


toList toLister tuple =
    toLister ( [], tuple )
        |> Tuple.first
        |> List.reverse


toLister1 ( list, ( item, items ) ) =
    ( item :: list, items )


reverse reverser tuple =
    reverser ( End, tuple )
        |> Tuple.first


reverser1 ( s, ( fst, rest ) ) =
    ( ( fst, s ), rest )


unfurl unfurler toOutput states =
    unfurler ( Ok toOutput, states )
        |> Tuple.first


unfurler1 ( toOutput, ( state, states ) ) =
    ( case ( toOutput, state.output ) of
        ( Ok fn, Ok val ) ->
            Ok (fn val)

        ( Ok fn, Err errs ) ->
            Err errs

        ( Err errs, Ok val ) ->
            Err errs

        ( Err errs1, Err errs2 ) ->
            Err (errs1 ++ errs2)
    , states
    )



-- Form builder functions


new output toMsg =
    { unfurler = identity
    , combiner = identity
    , updater = identity
    , cmdStripper = identity
    , viewer = identity
    , toLister = identity
    , stateReverser = identity
    , fieldReverser = identity
    , inits = End
    , fields = End
    , emptyMsg = End
    , toMsg = toMsg
    , output = output
    }


field fieldSetter fb f =
    { unfurler = f.unfurler >> unfurler1
    , combiner = f.combiner >> combiner1
    , updater = f.updater >> updater1
    , cmdStripper = f.cmdStripper >> cmdStripper1
    , viewer = f.viewer >> viewer1
    , toLister = f.toLister >> toLister1
    , stateReverser = f.stateReverser >> reverser1
    , fieldReverser = f.fieldReverser >> reverser1
    , inits =
        ( { state = fb.init
          , delta = Noop
          , output = fb.parse fb.init
          , lastTouched = Nothing
          }
        , f.inits
        )
    , fields =
        ( { update = fb.update
          , view = fb.view
          , parse = fb.parse
          , setter = fieldSetter
          }
        , f.fields
        )
    , toMsg = f.toMsg
    , output = f.output
    }


end f =
    let
        inits =
            reverse f.stateReverser f.inits

        fields =
            combine f.combiner inits f.fields
                |> reverse f.fieldReverser
    in
    { init = ( inits, Cmd.none )
    , update =
        \deltas states ->
            update_ f.updater fields deltas states
                |> cmdStrip f.cmdStripper
                |> Tuple.mapFirst (reverse f.stateReverser)
                |> Tuple.mapSecond (Cmd.map f.toMsg)
    , view =
        \states ->
            view_ f.viewer f.toMsg fields states
                |> toList f.toLister
    , submit =
        \states ->
            unfurl f.unfurler f.output states
    }
