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


type Model
    = Editing UserFormFields
    | Completed User


type Msg
    = UserFormUpdated UserFormFields
    | Submitted
    | Back


type alias UserFormFields =
    ( Field Int Int Int, ( Field Float Float Float, ( Field String String Int, ( Field String String Int, End ) ) ) )


type alias User =
    { age : Int, height : Float, weight : Int, x : Int }



-- Userland form definitions


form :
    { init : ( UserFormFields, Cmd Msg )
    , update : UserFormFields -> UserFormFields -> ( UserFormFields, Cmd Msg )
    , view : UserFormFields -> List (Html Msg)
    , submit : UserFormFields -> Result UserFormFields User
    }
form =
    new User UserFormUpdated
        |> field f0 int
        |> field f1 float
        |> field f2 string
        |> field (f2 >> f1) string
        |> end

{-
SKETCH OF FINAL API

form =
    Form.new User UserFormUpdated
        |> Form.field f0 
            (Field.string "name"
                |> Field.debounce 500 
                |> Field.failIf String.isEmpty "Name cannot be blank"
            )
        |> Form.field f1
            (Field.password "password"
                |> Field.debounce 500 
                |> Field.failIf (\x -> String.length x < 12) "Password must be at least 12 characters"
            )
        |> Form.field f2
            (Field.password "confirmation"
                |> Field.debounce 500 
            )
        |> Form.failIf2 
            (\name password -> String.contains name password) 
            "Password shouldn't contain name" 
            (Form.hide f0)
            (Form.show f1)
        |> Form.failIf2 
            (\password confirmation -> password /= confirmation) 
            "Password and confirmation must match" 
            (Form.show f1)
            (Form.show f2)
        |> Form.end
-}

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
    , parse = Ok
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserFormUpdated delta ->
            case model of
                Editing form_ ->
                    form.update delta form_
                        |> Tuple.mapFirst Editing

                Completed _ ->
                    ( model, Cmd.none )

        Submitted ->
            case model of
                Editing form0 ->
                    case form.submit form0 of
                        Ok user ->
                            ( Completed user, Cmd.none )

                        Err form1 ->
                            ( Editing form1, Cmd.none )

                Completed _ ->
                    ( model, Cmd.none )

        Back ->
            form.init
                |> Tuple.mapFirst Editing


view : Model -> Html Msg
view model =
    case model of
        Editing form_ ->
            div []
                [ div [] (form.view form_)
                , button [ onClick Submitted ] [ text "submit" ]
                ]

        Completed user ->
            div []
                [ div [] [ text "Your user's data:" ]
                , div [] [ text ("Age: " ++ String.fromInt user.age) ]
                , div [] [ text ("Height: " ++ String.fromFloat user.height) ]
                , div [] [ text ("Weight: " ++ String.fromInt user.weight) ]
                , button [ onClick Back ] [ text "go back" ]
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> form.init |> Tuple.mapFirst Editing
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


set1 : (b -> y) -> ( a, b ) -> ( a, y )
set1 =
    Tuple.mapSecond


get1 : ( a, b ) -> b
get1 =
    Tuple.second


f0 :
    { get : b -> c, set : d -> e }
    -> { get : b -> c, set : d -> e }
f0 =
    composeIndices { get = identity, set = identity }


f1 :
    { get : b -> c, set : (( d, e ) -> ( d, y )) -> f }
    -> { get : ( g, b ) -> c, set : (e -> y) -> f }
f1 =
    composeIndices { get = get1, set = set1 }


f2 :
    { get : b -> c, set : (( d, ( e, f ) ) -> ( d, ( e, y ) )) -> g }
    -> { get : ( h, ( i, b ) ) -> c, set : (f -> y) -> g }
f2 =
    f1 >> f1


composeIndices :
    { get : b -> c, set : d -> e }
    -> { get : c -> g, set : e -> h }
    -> { get : b -> g, set : d -> h }
composeIndices idx1 idx2 =
    { get = idx1.get >> idx2.get
    , set = idx1.set >> idx2.set
    }


instantiateIndex : ({ get : a -> a, set : b -> b } -> index) -> index
instantiateIndex idx =
    idx { get = identity, set = identity }



-- Step functions


update_ : ((End -> End -> End -> End) -> fields -> deltas -> states -> statesAndCmds) -> fields -> deltas -> states -> statesAndCmds
update_ updater fields deltas states =
    updater (\End End End -> End) fields deltas states


updater1 next ( field_, fields ) ( delta, deltas ) ( state, states ) =
    ( case delta.delta of
        StateUpdateRequested d ->
            let
                newState =
                    { state | state = field_.update d state.state }
            in
            if field_.debounce > 0 then
                ( newState
                , Task.perform (field_.toDelta << DebouncingStarted) Time.now
                )

            else
                ( parseAndValidate field_ newState
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


validateAll validateAller fields states =
    validateAller (\End End -> End) fields states


validateAller1 next ( field_, fields ) ( state, states ) =
    ( parseAndValidate field_ state, next fields states )


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
    ( case ( toOutput, state.output ) of
        ( Ok fn, Passed val ) ->
            Ok (fn val)

        _ ->
            Err ()
    , states
    )



-- Form builder functions


new output toMsg =
    { unfurler = identity
    , combiner = identity
    , updater = identity
    , cmdStripper = identity
    , validateAller = identity
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


field idx fb f =
    { unfurler = f.unfurler >> unfurler1
    , combiner = f.combiner >> combiner1
    , updater = f.updater >> updater1
    , cmdStripper = f.cmdStripper >> cmdStripper1
    , validateAller = f.validateAller >> validateAller1
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
          , setter = instantiateIndex idx |> .set
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
            let
                validatedStates =
                    validateAll f.validateAller fields states
            in
            case unfurl f.unfurler f.output validatedStates of
                Ok output ->
                    Ok output

                Err () ->
                    Err validatedStates
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
        , showIf (\_ _ -> True) "this always gets shown if validation passes"
        , showIf (\f1_ _ -> f1_ == 2) "this gets shown if f1 == 2"
        ]
        (get f1 >> get f2)


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
    Validator (Ok checkers) formData
        |> fieldGetters
        |> done


get :
    ({ get : a -> a, set : b -> b } -> { c | get : formData -> ( Result String d, e ) })
    -> Validator (d -> f) formData
    -> Validator f formData
get idx (Validator resultCheckers formData) =
    let
        fieldGetter =
            instantiateIndex idx |> .get

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


-- ANOTHER API SKETCH FOR MULTIVALIDATION
--
-- We would have failIf2, failIf3 etc., as well as showIf2, showIf3 and so on.
-- Perhaps these could actually be implemented using the more complex applicative 
-- API above, and then we could also offer failIfN and showIfN to support
-- checkers with arbitrary numbers of arguments 

form_failIf2 check feedback idx1 idx2 fb =
    let
        get1 = 
            instantiateIndex idx1 |> .get
        
        get2 = 
            instantiateIndex idx2 |> .get

        v = \state -> 
            case ( get1 state, get2 state ) of
                ( Ok arg1, Ok arg2 ) ->
                    if check arg1 arg2 then 
                        Err [ feedback ]
                    else 
                        Ok []
                
                ( Err f1, Err f2 ) ->
                    Err (f1 ++ f2)
                
                ( Err f, _ ) -> 
                    Err f
                
                ( _, Err f ) -> 
                    Err f
    in
    { fb | validators = v :: fb.validators }
