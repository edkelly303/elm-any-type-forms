module Poc exposing (main)

import Browser
import Html as H exposing (..)
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
    ( Field String String Int
    , ( Field String String Float
      , ( Field String String String
        , ( Field String String String, End )
        )
      )
    )


type alias User =
    { age : Int
    , height : Float
    , weight : String
    , x : String
    }



-- Userland form definitions


form :
    { init : ( UserFormFields, Cmd Msg )
    , update : UserFormFields -> UserFormFields -> ( UserFormFields, Cmd Msg )
    , view : UserFormFields -> List (Html Msg)
    , submit : UserFormFields -> Result UserFormFields User
    }
form =
    form_new User UserFormUpdated
        |> form_field f0 (field_int "Age")
        |> form_field f1 (field_float "Height")
        |> form_field f2
            (field_string "Name"
                |> field_debounce 500
                |> field_failIf (\name -> String.length name < 2) "name must be at least 2 characters"
            )
        |> form_field f3 (field_string "Pet's name")
        |> form_failIf2
            (\int flt -> int > round flt)
            "height must be greater than age"
            f0
            f1
        |> form_failIf2
            (\int name -> String.length name < int)
            "name must be at least as long as age"
            f0
            f2
        |> form_end



-- Userland widget definitions


field_int : String -> FieldBuilder String String Int (Html msg) msg
field_int id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse =
        \input ->
            case String.toInt input of
                Just i ->
                    Ok i

                Nothing ->
                    Err "not an int"
    , validators = []
    , debounce = 0
    }


field_float : String -> FieldBuilder String String Float (Html msg) msg
field_float id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse =
        \input ->
            case String.toFloat input of
                Just f ->
                    Ok f

                Nothing ->
                    Err "not an int"
    , validators = []
    , debounce = 0
    }


field_string_view state =
    div [ HA.style "margin-bottom" "30px" ]
        [ div [ HA.style "margin-bottom" "10px" ] [ text state.id ]
        , H.input
            [ onInput state.toMsg
            , HA.value state.input
            , HA.style "background-color"
                (case state.output of
                    Intact ->
                        "white"

                    Debouncing ->
                        "lightYellow"

                    FailedToParse ->
                        "pink"

                    Parsed _ Failed ->
                        "pink"

                    Parsed _ Passed ->
                        "paleGreen"
                )
            ]
            [ text state.input ]
        , case state.feedback of
            [] ->
                text ""

            _ ->
                div []
                    (List.map
                        (\f ->
                            div
                                [ HA.style "margin-top" "5px" ]
                                [ text ("🚫 " ++ f) ]
                        )
                        state.feedback
                    )
        ]


field_string : String -> FieldBuilder String String String (Html msg) msg
field_string id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse = Ok
    , validators = []
    , debounce = 500
    }


field_debounce millis fb =
    { fb | debounce = millis }


field_failIf check feedback fb =
    { fb | validators = { check = check, feedback = feedback, fails = True } :: fb.validators }


field_showIf check feedback fb =
    { fb | validators = { check = check, feedback = feedback, fails = False } :: fb.validators }



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
                , div [] [ text ("Weight: " ++ user.weight) ]
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


type alias Field input delta output =
    { input : input
    , delta : Delta delta
    , output : Output output
    , lastTouched : Maybe Time.Posix
    , feedback : List String
    , multifeedback : List String
    }


type Delta delta
    = StateUpdateRequested delta
    | DebouncingStarted Time.Posix
    | DebouncingChecked Time.Posix
    | Noop


type alias FieldBuilder input delta output element msg =
    { id : String
    , init : input
    , update : delta -> input -> input
    , view :
        { id : String
        , input : input
        , toMsg : delta -> msg
        , feedback : List String
        , output : Output output
        }
        -> element
    , parse : input -> Result String output
    , validators : List { check : output -> Bool, feedback : String, fails : Bool }
    , debounce : Float
    }


type Output output
    = Intact
    | Debouncing
    | FailedToParse
    | Parsed output ValidationOutcome


type ValidationOutcome
    = Passed
    | Failed



-- Field setters
-- get indexer state =
--     (instantiateIndex indexer |> .get |> Tuple.first) state
-- set indexer mapper state =
--     let
--         setFn =
--             instantiateIndex indexer |> .set
--     in
--     setFn (Tuple.mapFirst mapper) state


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


f3 :
    { get : b -> c, set : (( d, ( e, ( a, f ) ) ) -> ( d, ( e, ( a, y ) ) )) -> g }
    -> { get : ( h, ( i, ( j, b ) ) ) -> c, set : (f -> y) -> g }
f3 =
    f2 >> f1


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


updateFieldStates : ((End -> End -> End -> End) -> fields -> deltas -> states -> statesAndCmds) -> fields -> deltas -> states -> statesAndCmds
updateFieldStates updater fields deltas states =
    updater (\End End End -> End) fields deltas states


updater1 :
    (b -> a -> c -> d)
    ->
        ( { e
            | update : delta -> input -> input
            , debounce : Float
            , toDelta : Delta g -> msg
            , parse : input -> Result String output
            , validators : List { check : output -> Bool, feedback : String, fails : Bool }
          }
        , b
        )
    -> ( { h | delta : Delta delta }, a )
    -> ( Field input delta output, c )
    -> ( ( Field input delta output, Cmd msg ), d )
updater1 next ( field_, fields ) ( delta, deltas ) ( state0, states ) =
    let
        state1 =
            -- we're going to run multivalidate on all fields later in the update process, so we want to clear any existing multifeedback first.
            { state0 | multifeedback = [] }

        stateAndCmd =
            case delta.delta of
                StateUpdateRequested d ->
                    let
                        newState =
                            { state1 | input = field_.update d state1.input }
                    in
                    if field_.debounce > 0 then
                        ( newState
                        , Task.perform (field_.toDelta << DebouncingStarted) Time.now
                        )

                    else
                        ( parseAndValidateField field_ newState
                        , Cmd.none
                        )

                DebouncingStarted now ->
                    ( { state1
                        | lastTouched = Just now
                        , output = Debouncing
                      }
                    , Task.perform (\() -> field_.toDelta (DebouncingChecked now)) (Process.sleep field_.debounce)
                    )

                DebouncingChecked now ->
                    ( if state1.lastTouched == Just now then
                        parseAndValidateField field_ state1

                      else
                        state1
                    , Cmd.none
                    )

                Noop ->
                    ( state1
                    , Cmd.none
                    )
    in
    ( stateAndCmd
    , next fields deltas states
    )


validateAll : ((End -> End -> End) -> b -> c -> a) -> b -> c -> a
validateAll validateAller fields states =
    validateAller (\End End -> End) fields states


validateAller1 :
    (b -> a -> c)
    -> ( { d | parse : input -> Result String output, validators : List { check : output -> Bool, feedback : String, fails : Bool } }, b )
    -> ( Field input delta output, a )
    -> ( Field input delta output, c )
validateAller1 next ( field_, fields ) ( state, states ) =
    ( parseAndValidateField field_ state, next fields states )


parseAndValidateField :
    { a | parse : input -> Result String output, validators : List { check : output -> Bool, feedback : String, fails : Bool } }
    -> Field input delta output
    -> Field input delta output
parseAndValidateField field_ state =
    let
        ( output, feedback ) =
            case field_.parse state.input of
                Err f ->
                    ( FailedToParse, [ f ] )

                Ok val ->
                    validateField field_.validators val
    in
    { state
        | output = output
        , feedback = feedback
    }


validateField : List { check : val -> Bool, feedback : String, fails : Bool } -> val -> ( Output val, List String )
validateField validators val =
    List.foldl
        (\{ check, feedback, fails } ( outputVal, feedbacks ) ->
            if check val && fails then
                ( Parsed val Failed, feedback :: feedbacks )

            else if check val then
                ( outputVal, feedback :: feedbacks )

            else
                ( outputVal, feedbacks )
        )
        ( Parsed val Passed, [] )
        validators


cmdStrip cmdStripper statesAndCmds =
    let
        ( ( states, cmdsList ), _ ) =
            cmdStripper ( ( End, [] ), statesAndCmds )
    in
    ( states, Cmd.batch cmdsList )


cmdStripper1 ( ( outState, outCmd ), ( ( state, cmd ), statesAndCmds ) ) =
    ( ( ( state, outState ), cmd :: outCmd ), statesAndCmds )


view_ viewer toMsg fields states =
    viewer (\_ End End -> End) toMsg fields states


viewer1 next toMsg ( field_, fields ) ( state, states ) =
    ( field_.view
        { id = field_.id
        , toMsg = \x -> toMsg (field_.toDelta (StateUpdateRequested x))
        , input = state.input
        , output = state.output
        , feedback = state.feedback ++ state.multifeedback
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
    ( { id = field_.id
      , update = field_.update
      , view = field_.view
      , parse = field_.parse
      , validators = field_.validators
      , debounce = field_.debounce
      , toDelta = \x -> field_.setter (set x) inits
      }
    , next inits fields
    )


toList : (( List a, b ) -> ( List c, d )) -> b -> List c
toList toLister tuple =
    toLister ( [], tuple )
        |> Tuple.first
        |> List.reverse


toLister1 : ( List a, ( a, b ) ) -> ( List a, b )
toLister1 ( list, ( item, items ) ) =
    ( item :: list, items )


reverse : (( End, b ) -> ( a, c )) -> b -> a
reverse reverser tuple =
    reverser ( End, tuple )
        |> Tuple.first


reverser1 : ( a, ( b, c ) ) -> ( ( b, a ), c )
reverser1 ( s, ( fst, rest ) ) =
    ( ( fst, s ), rest )


unfurl : (( Result error b, c ) -> ( a, d )) -> b -> c -> a
unfurl unfurler toOutput states =
    unfurler ( Ok toOutput, states )
        |> Tuple.first


unfurler1 : ( Result error (output -> a), ( { b | output : Output output }, c ) ) -> ( Result () a, c )
unfurler1 ( toOutput, ( state, states ) ) =
    ( case ( toOutput, state.output ) of
        ( Ok fn, Parsed val Passed ) ->
            Ok (fn val)

        _ ->
            Err ()
    , states
    )



-- Form builder functions


form_new output toMsg =
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
    , validators = []
    , toMsg = toMsg
    , output = output
    }


form_field idx fieldBuilder f =
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
        ( { input = fieldBuilder.init
          , delta = Noop
          , output = Intact
          , lastTouched = Nothing
          , feedback = []
          , multifeedback = []
          }
        , f.inits
        )
    , fields =
        ( { id = fieldBuilder.id
          , update = fieldBuilder.update
          , view = fieldBuilder.view
          , parse = fieldBuilder.parse
          , validators = fieldBuilder.validators
          , debounce = fieldBuilder.debounce
          , setter = instantiateIndex idx |> .set
          }
        , f.fields
        )
    , validators = f.validators
    , toMsg = f.toMsg
    , output = f.output
    }


form_end f =
    let
        inits =
            reverse f.stateReverser f.inits

        fields =
            combine f.combiner inits f.fields
                |> reverse f.fieldReverser
    in
    { init = ( inits, Cmd.none )
    , update =
        \deltas states0 ->
            let
                ( reversedStates1, cmd ) =
                    -- in this step we don't just update the field that the
                    -- delta is targetting, we also clear multivalidation for
                    -- all fields
                    updateFieldStates f.updater fields deltas states0
                        |> cmdStrip f.cmdStripper

                states1 =
                    reversedStates1
                        |> reverse f.stateReverser

                states2 =
                    -- and now we update form level multivalidation for all
                    -- fields
                    formValidate f.validators states1
            in
            ( states2
            , Cmd.map f.toMsg cmd
            )
    , view =
        \states ->
            view_ f.viewer f.toMsg fields states
                |> toList f.toLister
    , submit =
        \states ->
            let
                validatedStates =
                    validateAll f.validateAller fields states
                        |> formValidate f.validators
            in
            case unfurl f.unfurler f.output validatedStates of
                Ok output ->
                    Ok output

                Err () ->
                    Err validatedStates
    }



-- FORM-LEVEL VALIDATION (OF MULTIPLE FIELDS)


formValidate : List (states -> states) -> states -> states
formValidate validators states =
    List.foldl (\v previousStates -> v previousStates) states validators


form_failIf2 check feedback indexer1 indexer2 formBuilder =
    let
        idx1 =
            instantiateIndex indexer1

        idx2 =
            instantiateIndex indexer2

        v =
            \state ->
                let
                    field1 =
                        (idx1.get >> Tuple.first) state

                    field2 =
                        (idx2.get >> Tuple.first) state
                in
                case ( field1.output, field2.output ) of
                    ( Parsed output1 _, Parsed output2 _ ) ->
                        if check output1 output2 then
                            state
                                |> idx1.set
                                    (Tuple.mapFirst
                                        (\field1_ ->
                                            { field1_
                                                | output = Parsed output1 Failed
                                                , multifeedback = feedback :: field1_.multifeedback
                                            }
                                        )
                                    )
                                |> idx2.set
                                    (Tuple.mapFirst
                                        (\field2_ ->
                                            { field2_
                                                | output = Parsed output2 Failed
                                                , multifeedback = feedback :: field2_.multifeedback
                                            }
                                        )
                                    )

                        else
                            state

                    _ ->
                        state
    in
    { formBuilder | validators = v :: formBuilder.validators }



-- A SKETCH OF MULTI-FIELD VALIDATION WITH APPLICATIVES
--
-- showIf : checker -> String -> Checker checker
-- showIf checker feedback =
--     { check = checker, feedback = feedback, fails = False }
-- failIf : checker -> String -> Checker checker
-- failIf checker feedback =
--     { check = checker, feedback = feedback, fails = True }
-- test : Result (List String) (List String)
-- test =
--     multivalidate
--         ( Err "f0 did not parse", ( Ok 1, ( Ok 1, End ) ) )
--         [ failIf (\f1_ f2_ -> f1_ == f2_) "failed because f1 and f2 are equal!"
--         , failIf (\f1_ f2_ -> f1_ <= f2_) "failed because f1 <= f2!"
--         , showIf (\_ _ -> True) "this always gets shown if validation passes"
--         , showIf (\f1_ _ -> f1_ == 2) "this gets shown if f1 == 2"
--         ]
--         (fld f1 >> fld f2)
-- type Validator checker formData
--     = Validator (Result (List String) (List (Checker checker))) formData
-- type alias Checker checker =
--     { check : checker
--     , fails : Bool
--     , feedback : String
--     }
-- multivalidate :
--     formData
--     -> List (Checker checker)
--     -> (Validator checker formData -> Validator Bool formData)
--     -> Result (List String) (List String)
-- multivalidate formData checkers fieldGetters =
--     Validator (Ok checkers) formData
--         |> fieldGetters
--         |> done
-- fld :
--     ({ get : a -> a, set : b -> b } -> { c | get : formData -> ( Result String d, e ) })
--     -> Validator (d -> f) formData
--     -> Validator f formData
-- fld idx (Validator resultCheckers formData) =
--     let
--         fieldGetter =
--             instantiateIndex idx |> .get
--         resultFieldData =
--             fieldGetter formData |> Tuple.first
--         appliedCheckers =
--             case ( resultCheckers, resultFieldData ) of
--                 ( Ok checkers, Ok fieldData ) ->
--                     Ok
--                         (List.map
--                             (\checker ->
--                                 { check = checker.check fieldData
--                                 , feedback = checker.feedback
--                                 , fails = checker.fails
--                                 }
--                             )
--                             checkers
--                         )
--                 ( Err previousErrs, Ok _ ) ->
--                     Err previousErrs
--                 ( Ok _, Err parsingErr ) ->
--                     Err [ parsingErr ]
--                 ( Err previousErrs, Err parsingErr ) ->
--                     Err (parsingErr :: previousErrs)
--     in
--     Validator appliedCheckers formData
-- done : Validator Bool formData -> Result (List String) (List String)
-- done (Validator resultCheckers _) =
--     case resultCheckers of
--         Err e ->
--             Err e
--         Ok [] ->
--             Ok []
--         Ok checkers ->
--             List.foldl
--                 (\{ check, feedback, fails } resultFeedbacks ->
--                     case resultFeedbacks of
--                         Err feedbacks ->
--                             if check && fails then
--                                 Err (feedback :: feedbacks)
--                             else
--                                 Err feedbacks
--                         Ok feedbacks ->
--                             if check then
--                                 if fails then
--                                     Err [ feedback ]
--                                 else
--                                     Ok (feedback :: feedbacks)
--                             else
--                                 Ok feedbacks
--                 )
--                 (Ok [])
--                 checkers
