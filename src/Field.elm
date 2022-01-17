module Field exposing
    ( Delta(..)
    , Feedback
    , FeedbackTag(..)
    , Field(..)
    , InternalStatus(..)
    , State
    , Status(..)
    , ValidationStatus(..)
    , ViewConfig
    , custom
    , debounce
    , doNotValidate
    , fail
    , failIf
    , info
    , infoIf
    , init
    , pass
    , submit
    , update
    , validate
    , view
    , warn
    , warnIf
    , withInitialState
    , withValidator
    , withView
    )

import Process
import Task
import Time


type Delta delta
    = UpdateInput delta
    | StartDebouncing (DeltaContext delta) Time.Posix
    | CheckDebouncingTimeout (DeltaContext delta) Time.Posix
    | ExecuteLoading (DeltaContext delta) delta
    | ExecuteParsing
    | Focused


type alias DeltaContext delta =
    { shouldSendCmd : Bool
    , debounce : Float
    , shouldValidate : Bool
    , delta : delta
    }


type InternalStatus
    = Debouncing_ Time.Posix
    | Loading_
    | Parsing_
    | Idle_


type Status
    = Debouncing
    | Loading
    | Parsing
    | Idle


type Opt
    = DontValidate
    | Debounce Float


debounce : Float -> Opt
debounce millis =
    Debounce millis


doNotValidate : Opt
doNotValidate =
    DontValidate


type Field input delta output element msg
    = Field
        { index : Int
        , init : input
        , deltaMsg : Delta delta -> msg
        , updater : delta -> input -> ( input, Cmd delta, List Opt )
        , parser : input -> Result Feedback output
        , validators : List (output -> Maybe Feedback)
        , view : ViewConfig input delta output msg -> element
        , id : String
        , label : String
        }


type alias ViewConfig input delta output msg =
    { input : input
    , status : Status
    , parsed : ValidationStatus output
    , delta : delta -> msg
    , focusMsg : msg
    , focused : Bool
    , label : String
    , id : String
    }


type alias State input output =
    { input : input
    , validated : ValidationStatus output
    , status : InternalStatus
    , focused : Bool
    }


type ValidationStatus output
    = Intact
    | Passed output (List Feedback)
    | Failed (List Feedback)


type alias Feedback =
    { tag : FeedbackTag, text : String }


type FeedbackTag
    = Info
    | Pass
    | Fail
    | Warn


info : String -> Feedback
info str =
    Feedback Info str


pass : String -> Feedback
pass str =
    Feedback Pass str


fail : String -> Feedback
fail str =
    Feedback Fail str


warn : String -> Feedback
warn str =
    Feedback Warn str



-- CREATING FIELDS


custom :
    { init : input
    , deltaMsg : Delta delta -> msg
    , updater : delta -> input -> ( input, Cmd delta, List Opt )
    , parser : input -> Result Feedback output
    , renderer : ViewConfig input delta output msg -> element
    , label : String
    }
    -> Field input delta output element msg
custom args =
    Field
        { index = 0
        , init = args.init
        , deltaMsg = args.deltaMsg
        , updater = args.updater
        , parser = args.parser
        , validators = []
        , view = args.renderer
        , id = ""
        , label = args.label
        }



-- EXTRACTING STATE FROM FIELDS


init : Field input delta output element msg -> State input output
init (Field f) =
    { input = f.init
    , validated = Intact
    , status = Idle_
    , focused = False
    }


update : Field input delta output element msg -> Delta delta -> State input output -> ( State input output, Cmd msg )
update (Field field_) wrappedDelta fieldState =
    case wrappedDelta of
        Focused ->
            ( { fieldState | focused = True }
            , Cmd.none
            )

        UpdateInput delta ->
            let
                ( newInput, cmd, opts ) =
                    field_.updater delta fieldState.input

                ( debounce_, shouldValidate ) =
                    List.foldl
                        (\opt ( d, sv ) ->
                            case opt of
                                Debounce millis ->
                                    ( millis, sv )

                                DontValidate ->
                                    ( d, False )
                        )
                        ( 0, True )
                        opts

                newFieldState =
                    { fieldState | input = newInput }

                shouldSendCmd =
                    cmd /= Cmd.none

                ctx =
                    { debounce = debounce_
                    , shouldSendCmd = shouldSendCmd
                    , shouldValidate = shouldValidate
                    , delta = delta
                    }
            in
            if debounce_ /= 0 then
                --transition to debouncing
                ( newFieldState
                , Task.perform (\time -> field_.deltaMsg (StartDebouncing ctx time)) Time.now
                )

            else if shouldSendCmd then
                -- send the Cmd!
                ( { newFieldState | status = Loading_ }
                , Cmd.map (field_.deltaMsg << ExecuteLoading ctx) cmd
                )

            else if shouldValidate then
                -- transition to parsing
                ( { newFieldState | status = Parsing_ }
                , Task.perform (\() -> field_.deltaMsg ExecuteParsing) (Process.sleep 20)
                )

            else
                -- transition to idle
                ( { newFieldState | status = Idle_ }
                , Cmd.none
                )

        StartDebouncing ctx time ->
            ( { fieldState | status = Debouncing_ time }
            , Task.perform
                (\() -> field_.deltaMsg <| CheckDebouncingTimeout ctx time)
                (Process.sleep ctx.debounce)
            )

        CheckDebouncingTimeout ctx time ->
            case fieldState.status of
                Debouncing_ lastTouched ->
                    if time == lastTouched then
                        if ctx.shouldSendCmd then
                            let
                                ( _, cmd, _ ) =
                                    field_.updater ctx.delta fieldState.input
                            in
                            -- send the Cmd!
                            ( { fieldState | status = Loading_ }
                            , Cmd.map (field_.deltaMsg << ExecuteLoading ctx) cmd
                            )

                        else if ctx.shouldValidate then
                            -- transition to parsing
                            ( { fieldState | status = Parsing_ }
                            , Task.perform (\() -> field_.deltaMsg ExecuteParsing) (Process.sleep 20)
                            )

                        else
                            -- transition to idle
                            ( { fieldState | status = Idle_ }
                            , Cmd.none
                            )

                    else
                        ( fieldState, Cmd.none )

                _ ->
                    ( fieldState, Cmd.none )

        ExecuteLoading ctx newDelta ->
            let
                ( newInput, newCmd, _ ) =
                    field_.updater newDelta fieldState.input

                newFieldState =
                    { fieldState | input = newInput }
            in
            if newCmd /= Cmd.none then
                -- send the next Cmd!
                ( { newFieldState | status = Loading_ }
                , Cmd.map (field_.deltaMsg << ExecuteLoading ctx) newCmd
                )

            else if ctx.shouldValidate then
                -- transition to parsing
                ( { newFieldState | status = Parsing_ }
                , Task.perform (\() -> field_.deltaMsg ExecuteParsing) (Process.sleep 20)
                )

            else
                -- transition to idle
                ( { newFieldState | status = Idle_ }
                , Cmd.none
                )

        ExecuteParsing ->
            ( { fieldState | status = Idle_ }
                |> validate (Field field_)
            , Cmd.none
            )


submit : Field input delta output element msg -> State input output -> Result (State input output) output
submit field fieldState =
    case fieldState.validated of
        Passed output _ ->
            Ok output

        Failed _ ->
            Err fieldState

        Intact ->
            validate field fieldState
                |> submit field


validate : Field input delta output element msg -> State input output -> State input output
validate (Field { parser, validators }) fieldState =
    { fieldState
        | status = Idle_
        , validated =
            case
                fieldState.input
                    |> parser
                    |> Result.mapError List.singleton
                    |> Result.andThen
                        (\parsed ->
                            validators
                                |> List.map (\v -> v parsed)
                                |> accumulateErrors parsed
                        )
            of
                Ok ( output, feedback ) ->
                    Passed output feedback

                Err feedback ->
                    Failed feedback
    }


accumulateErrors : a -> List (Maybe Feedback) -> Result (List Feedback) ( a, List Feedback )
accumulateErrors a list =
    case
        list
            |> List.filterMap identity
            |> List.partition (\{ tag } -> tag == Fail)
    of
        ( [], others ) ->
            Ok ( a, others )

        ( errors, _ ) ->
            Err errors


view : Field input delta output element msg -> State input output -> element
view (Field f) s =
    f.view
        { input = s.input
        , status =
            case s.status of
                Debouncing_ _ ->
                    Debouncing

                Idle_ ->
                    Idle

                Loading_ ->
                    Loading

                Parsing_ ->
                    Parsing
        , parsed = s.validated
        , delta = f.deltaMsg << UpdateInput
        , focusMsg = f.deltaMsg Focused
        , focused = s.focused
        , id = f.id
        , label = f.label
        }



-- BUILDING FIELDS


withInitialState : input -> Field input delta output element msg -> Field input delta output element msg
withInitialState input (Field f) =
    Field { f | init = input }


withValidator : (output -> Maybe Feedback) -> Field input delta output element msg -> Field input delta output element msg
withValidator v (Field f) =
    Field { f | validators = v :: f.validators }


failIf : (output -> Bool) -> String -> Field input delta output element msg -> Field input delta output element msg
failIf test message field =
    withValidator
        (validator test fail message)
        field


warnIf : (output -> Bool) -> String -> Field input delta output element msg -> Field input delta output element msg
warnIf test message field =
    withValidator
        (validator test warn message)
        field


infoIf : (output -> Bool) -> String -> Field input delta output element msg -> Field input delta output element msg
infoIf test message field =
    withValidator
        (validator test info message)
        field


validator : (output -> Bool) -> (String -> Feedback) -> String -> output -> Maybe Feedback
validator test feedback message output =
    if test output then
        Just (feedback message)

    else
        Nothing


withView :
    (ViewConfig input delta output msg -> element2)
    -> Field input delta output element msg
    -> Field input delta output element2 msg
withView r (Field f) =
    Field
        { index = f.index
        , init = f.init
        , deltaMsg = f.deltaMsg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , view = r
        , id = f.id
        , label = f.label
        }
