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

type Msg
    = UserFormUpdated UserFormFields


type alias UserFormFields =
    ( Field Int Int Int, ( Field Float Float Float, ( Field String String Int, End ) ) )


type alias User =
    { age : Int, height : Float, weight : Int }



-- Userland form definitions


form :
    { init : ( UserFormFields, Cmd Msg )
    , update : UserFormFields -> UserFormFields -> ( UserFormFields, Cmd Msg )
    , view : UserFormFields -> List (Html Msg)
    , submit : UserFormFields -> Result (List String) User
    }
form =
    new User UserFormUpdated
        |> field f0 int
        |> field f1 float
        |> field f2 string
        |> end



-- Userland widget definitions


int : FieldBuilder Int Int Int (Html msg) msg
int =
    { init = 100
    , update = \delta state -> state // delta
    , view = \{ toMsg, state } -> div [] [ button [ onClick (toMsg 2) ] [ text (String.fromInt state) ] ]
    , parse = Ok
    , validators = []
    }


float : FieldBuilder Float Float Float (Html msg) msg
float =
    { init = 0.1
    , update = \delta state -> state + delta
    , view = \{ toMsg, state } -> div [] [ button [ onClick (toMsg 2.1) ] [ text (String.fromFloat state) ] ]
    , parse = \_ -> Err [ "uh oh" ]
    , validators = []
    }


string : FieldBuilder String String Int (Html msg) msg
string =
    { init = ""
    , update = \delta _ -> delta
    , view =
        \{ toMsg, state, output, feedback } ->
            div []
                [ input
                    [ onInput toMsg
                    , HA.value state
                    , HA.style "background-color"
                        (case output of
                            Just _ ->
                                "paleGreen"

                            Nothing ->
                                "pink"
                        )
                    ]
                    [ text state ]
                , case feedback of
                    [] ->
                        text ""

                    _ ->
                        div [] (List.map text feedback)
                ]
    , parse =
        \state ->
            case String.toInt state of
                Just i ->
                    Ok i

                Nothing ->
                    Err [ "not an int" ]
    , validators =
        [ { check = \output -> output < 2, feedback = "must be greater than 1", fails = True }
        , { check = \output -> output == 7, feedback = "that's my lucky number", fails = False }
        ]
    }



-- Userland program plumbing


update : Msg -> UserFormFields -> ( UserFormFields, Cmd Msg )
update msg model =
    case msg of
        UserFormUpdated payload ->
            form.update payload model


view : UserFormFields -> Html Msg
view model =
    let
        _ =
            Debug.log "User" (form.submit model)
    in
    div [] (form.view model)


main : Program () UserFormFields Msg
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
    , output : Maybe output
    , lastTouched : Maybe Time.Posix
    , feedback : List String
    }


type Delta delta
    = Update delta
    | StartDebouncing Time.Posix
    | ParseIfDebounced Time.Posix
    | Noop


type alias FieldBuilder state delta output element msg =
    { init : state
    , update : delta -> state -> state
    , view :
        { state : state
        , toMsg : delta -> msg
        , feedback : List String
        , output : Maybe output
        }
        -> element
    , parse : state -> Result (List String) output
    , validators : List { check : output -> Bool, feedback : String, fails : Bool }
    }



-- Field setters


f0 =
    identity


f1 =
    Tuple.mapSecond


f2 =
    f1 >> f1



-- Step functions


update_ : ((End -> End -> End -> End) -> fields -> deltas -> states -> statesAndCmds) -> fields -> deltas -> states -> statesAndCmds
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
                let
                    parsed =
                        field_.parse state.state

                    ( output, feedback ) =
                        case parsed of
                            Err f ->
                                ( Nothing, f )

                            Ok val ->
                                validate field_.validators val
                in
                { state
                    | output = output
                    , feedback = feedback
                }

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


validate : List { check : val -> Bool, feedback : String, fails : Bool } -> val -> ( Maybe val, List String )
validate validators val =
    List.foldl
        (\{ check, feedback, fails } ( maybeVal, feedbacks ) ->
            if check val && fails then
                ( Nothing, feedback :: feedbacks )

            else if check val then
                ( maybeVal, feedback :: feedbacks )

            else
                ( maybeVal, feedbacks )
        )
        ( Just val, [] )
        validators


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
        , feedback = state.feedback
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
      , validators = field_.validators
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
        ( Ok fn, Just val ) ->
            Ok (fn val)

        ( Ok _, Nothing ) ->
            Err state.feedback

        ( Err errs, Just _ ) ->
            Err errs

        ( Err errs, Nothing ) ->
            Err (errs ++ state.feedback)
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
          , output = Nothing --fb.parse fb.init |> validate fb.validators
          , lastTouched = Nothing
          , feedback = []
          }
        , f.inits
        )
    , fields =
        ( { update = fb.update
          , view = fb.view
          , parse = fb.parse
          , validators = fb.validators
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
