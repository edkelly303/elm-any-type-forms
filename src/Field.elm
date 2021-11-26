module Field exposing
    ( Delta(..)
    , DeltaContext
    , Feedback
    , FeedbackTag(..)
    , Field(..)
    , InternalStatus(..)
    , Opt(..)
    , RendererConfig
    , State
    , Status(..)
    , custom
    , debounce
    , doNotValidate
    , fail
    , failIf
    , info
    , infoIf
    , initialize
    , pass
    , warn
    , warnIf
    , withInitialState
    , withRenderer
    , withValidator
    )

import Dict
import Time


type Delta delta
    = UpdateInput delta
    | StartDebouncing (DeltaContext delta) Time.Posix
    | CheckDebouncingTimeout (DeltaContext delta) Time.Posix
    | ExecuteLoading (DeltaContext delta) delta
    | ExecuteParsing
    | Focused


type alias DeltaContext delta =
    { cmd : Cmd delta
    , debounce : Float
    , shouldValidate : Bool
    }


type InternalStatus
    = Intact_
    | Debouncing_ Time.Posix
    | Loading_
    | Parsing_
    | Idle_


type Status
    = Intact
    | Debouncing
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
        , renderer : RendererConfig input delta output msg -> element
        , id : String
        , label : String
        , loadCmd : Dict.Dict String (input -> Cmd msg)
        }


type alias RendererConfig input delta output msg =
    { input : input
    , status : Status
    , parsed : Maybe output
    , feedback : List Feedback
    , delta : delta -> msg
    , focusMsg : msg
    , focused : Bool
    , label : String
    , id : String
    }


type alias State input output =
    { input : input
    , validated : Result (List Feedback) ( output, List Feedback )
    , status : InternalStatus
    , focused : Bool
    }


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
    , renderer : RendererConfig input delta output msg -> element
    , label : String
    }
    -> Field input delta output element msg
custom { init, deltaMsg, updater, parser, renderer, label } =
    Field
        { index = 0
        , init = init
        , deltaMsg = deltaMsg
        , updater = updater
        , parser = parser
        , validators = []
        , renderer = renderer
        , id = ""
        , label = label
        , loadCmd = Dict.empty
        }



-- EXTRACTING STATE FROM FIELDS


initialize : Field input delta output element msg -> State input output
initialize (Field { init }) =
    { input = init
    , validated = Err []
    , status = Intact_
    , focused = False
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


withRenderer :
    (RendererConfig input delta output msg -> element2)
    -> Field input delta output element msg
    -> Field input delta output element2 msg
withRenderer r (Field f) =
    Field
        { index = f.index
        , init = f.init
        , deltaMsg = f.deltaMsg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , renderer = r
        , id = f.id
        , label = f.label
        , loadCmd = f.loadCmd
        }
