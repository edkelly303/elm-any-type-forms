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
    , debounce = 0
    }


float : FieldBuilder Float Float Float (Html msg) msg
float =
    { init = 0.1
    , update = \delta state -> state + delta
    , view = \{ toMsg, state } -> div [] [ button [ onClick (toMsg 2.1) ] [ text (String.fromFloat state) ] ]
    , parse = \_ -> Err "uh oh"
    , validators = []
    , debounce = 0
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
                            Intact ->
                                "white"

                            Debouncing ->
                                "lightYellow"

                            Passed _ ->
                                "paleGreen"

                            Failed ->
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
                    Err "not an int"
    , validators =
        [ { check = \output -> output < 2, feedback = "must be greater than 1", fails = True }
        , { check = \output -> output == 7, feedback = "that's my lucky number", fails = False }
        ]
    , debounce = 500
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
    , output : Output output
    , lastTouched : Maybe Time.Posix
    , feedback : List String
    }


type Delta delta
    = StateUpdateRequested delta
    | DebouncingStarted Time.Posix
    | DebouncingChecked Time.Posix
    | Noop


type alias FieldBuilder state delta output element msg =
    { init : state
    , update : delta -> state -> state
    , view :
        { state : state
        , toMsg : delta -> msg
        , feedback : List String
        , output : Output output
        }
        -> element
    , parse : state -> Result String output
    , validators : List { check : output -> Bool, feedback : String, fails : Bool }
    , debounce : Float
    }


type Output output
    = Intact
    | Debouncing
    | Passed output
    | Failed



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
        StateUpdateRequested d ->
            let
                newState =
                    field_.update d state.state
            in
            if field_.debounce > 0 then
                ( { state | state = newState }
                , Task.perform (field_.toDelta << DebouncingStarted) Time.now
                )

            else
                ( parseAndValidate field_ state
                , Cmd.none
                )

        DebouncingStarted now ->
            ( { state
                | lastTouched = Just now
                , output = Debouncing
              }
            , Task.perform (\() -> field_.toDelta (DebouncingChecked now)) (Process.sleep field_.debounce)
            )

        DebouncingChecked now ->
            ( if state.lastTouched == Just now then
                parseAndValidate field_ state

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


parseAndValidate :
    { a | parse : state -> Result String output, validators : List { check : output -> Bool, feedback : String, fails : Bool } }
    -> Field state delta output
    -> Field state delta output
parseAndValidate field_ state =
    let
        ( output, feedback ) =
            case field_.parse state.state of
                Err f ->
                    ( Failed, [ f ] )

                Ok val ->
                    validate field_.validators val
    in
    { state
        | output = output
        , feedback = feedback
    }


validate : List { check : val -> Bool, feedback : String, fails : Bool } -> val -> ( Output val, List String )
validate validators val =
    List.foldl
        (\{ check, feedback, fails } ( outputVal, feedbacks ) ->
            if check val && fails then
                ( Failed, feedback :: feedbacks )

            else if check val then
                ( outputVal, feedback :: feedbacks )

            else
                ( outputVal, feedbacks )
        )
        ( Passed val, [] )
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
        { toMsg = \x -> toMsg (field_.toDelta (StateUpdateRequested x))
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
      , debounce = field_.debounce
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
    -- this may need a rethink to handle the Intact and Debouncing states explicitly...
    -- or more likely, to validate all fields before unfurling, so that no fields will be
    -- intact or debouncing by the time this function is called...
    ( case ( toOutput, state.output ) of
        ( Ok fn, Passed val ) ->
            Ok (fn val)

        ( Ok _, _ ) ->
            Err state.feedback

        ( Err errs, Passed _ ) ->
            Err errs

        ( Err errs, _ ) ->
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
          , output = Intact
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
          , debounce = fb.debounce
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



-- A SKETCH OF MULTI-FIELD VALIDATION


showIf : checker -> String -> Checker checker
showIf checker feedback =
    { check = checker, feedback = feedback, fails = False }


failIf : checker -> String -> Checker checker
failIf checker feedback =
    { check = checker, feedback = feedback, fails = True }


test : Result (List String) (List String)
test =
    multivalidate
        ( Err "f0 did not parse", ( Ok 1, ( Ok 1, End ) ) )
        [ failIf (\f1_ f2_ -> f1_ == f2_) "failed because f1 and f2 are equal!"
        , failIf (\f1_ f2_ -> f1_ <= f2_) "failed because f1 <= f2!"
        , showIf (\f1_ f2_ -> True) "this always gets shown if validation passes"
        , showIf (\f1_ f2_ -> f1_ == 2) "this gets shown if f1 == 2"
        ]
        (get v1 >> get v2)


v1 : ( a, b ) -> b
v1 =
    Tuple.second


v2 : ( a, ( b, c ) ) -> c
v2 =
    v1 >> v1


type Validator checker formData
    = Validator (Result (List String) (List (Checker checker))) formData


type alias Checker checker =
    { check : checker
    , fails : Bool
    , feedback : String
    }


multivalidate :
    formData
    -> List (Checker checker)
    -> (Validator checker formData -> Validator Bool formData)
    -> Result (List String) (List String)
multivalidate formData checkers fieldGetters =
    lift checkers formData
        |> fieldGetters
        |> done


lift : List (Checker checker) -> formData -> Validator checker formData
lift checkers formData =
    Validator (Ok checkers) formData


get :
    (formData -> ( Result String fieldData, restFields ))
    -> Validator (fieldData -> next) formData
    -> Validator next formData
get fieldGetter (Validator resultCheckers formData) =
    let
        resultFieldData =
            fieldGetter formData |> Tuple.first

        appliedCheckers =
            case ( resultCheckers, resultFieldData ) of
                ( Ok checkers, Ok fieldData ) ->
                    Ok
                        (List.map
                            (\checker ->
                                { check = checker.check fieldData
                                , feedback = checker.feedback
                                , fails = checker.fails
                                }
                            )
                            checkers
                        )

                ( Err previousErrs, Ok _ ) ->
                    Err previousErrs

                ( Ok _, Err parsingErr ) ->
                    Err [ parsingErr ]

                ( Err previousErrs, Err parsingErr ) ->
                    Err (parsingErr :: previousErrs)
    in
    Validator appliedCheckers formData


done : Validator Bool formData -> Result (List String) (List String)
done (Validator resultCheckers _) =
    case resultCheckers of
        Err e ->
            Err e

        Ok [] ->
            Ok []

        Ok checkers ->
            List.foldl
                (\{ check, feedback, fails } resultFeedbacks ->
                    case resultFeedbacks of
                        Err feedbacks ->
                            if check && fails then
                                Err (feedback :: feedbacks)

                            else
                                Err feedbacks

                        Ok feedbacks ->
                            if check then
                                if fails then
                                    Err [ feedback ]

                                else
                                    Ok (feedback :: feedbacks)

                            else
                                Ok feedbacks
                )
                (Ok [])
                checkers
