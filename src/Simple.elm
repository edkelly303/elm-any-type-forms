module Simple exposing (..)

import Field exposing (Delta, Feedback, Field(..), Opt, State, ViewConfig)


new :
    { init : input
    , deltaMsg : Delta delta -> msg
    , updater : delta -> input -> ( input, Cmd delta, List Opt )
    , parser : input -> Result Feedback output
    , renderer : ViewConfig input delta output msg -> element
    , label : String
    }
    -> Field input delta output () element msg
new args =
    Field
        { index = 0
        , init = args.init
        , deltaMsg = args.deltaMsg
        , updater = args.updater
        , parser = \_ -> args.parser
        , validators = []
        , view = args.renderer
        , id = ""
        , label = args.label
        }



-- EXTRACTING STATE FROM FIELDS


init : Field input delta output context element msg -> State input output
init =
    Field.init


update : Field input delta output () element msg -> Delta delta -> State input output -> ( State input output, Cmd msg )
update field delta fieldState =
    Field.update field delta () fieldState


submit : Field input delta output () element msg -> State input output -> Result (State input output) output
submit field fieldState =
    Field.submit field () fieldState


validate : Field input delta output () element msg -> State input output -> State input output
validate field fieldState =
    Field.validate field () fieldState


view : Field input delta output context element msg -> State input output -> element
view =
    Field.view



-- STUFF


withInitialState : input -> Field input delta output () element msg -> Field input delta output () element msg
withInitialState =
    Field.withInitialState


withValidator : (output -> Feedback) -> Field input delta output context element msg -> Field input delta output context element msg
withValidator v (Field f) =
    Field { f | validators = (\_ -> v) :: f.validators }


failIf : (output -> Bool) -> String -> Field input delta output context element msg -> Field input delta output context element msg
failIf test message field =
    field
        |> withValidator
            (\output ->
                if test output then
                    Field.fail message

                else
                    Field.none
            )


warnIf : (output -> Bool) -> String -> Field input delta output context element msg -> Field input delta output context element msg
warnIf test message field =
    field
        |> withValidator
            (\output ->
                if test output then
                    Field.warn message

                else
                    Field.none
            )


infoIf : (output -> Bool) -> String -> Field input delta output context element msg -> Field input delta output context element msg
infoIf test message field =
    field
        |> withValidator
            (\output ->
                if test output then
                    Field.info message

                else
                    Field.none
            )


withView :
    (ViewConfig input delta output msg -> element2)
    -> Field input delta output context element msg
    -> Field input delta output context element2 msg
withView =
    Field.withView
