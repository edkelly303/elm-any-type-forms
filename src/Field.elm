module Field exposing
    ( Builder
    , Delta(..)
    , Interface
    , Output(..)
    , State
    , Status(..)
    , ViewConfig
    , debounce
    , failIf
    , float
    , indexIfParsed
    , infoIf
    , int
    , parse
    , string
    , update
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Process
import Result.Extra
import Task
import Time


type alias Builder input delta output element msg =
    { id : String
    , init : input
    , update : delta -> input -> input
    , view : ViewConfig input delta msg -> element
    , parse : input -> Result String output
    , validators : List { check : output -> Bool, feedback : Result String String }
    , debounce : Float
    }


type alias Interface input delta output formData element msg =
    { view : ViewConfig input delta msg -> element
    , id : String
    , toFormData : Delta delta -> formData
    , debounce : Float
    , index : Int
    , parse : input -> Result String output
    , update : delta -> input -> input
    , validators : formData -> formData
    }


type alias State input delta output =
    { input : input
    , delta : Delta delta
    , output : Output output
    , lastTouched : Maybe Time.Posix
    , feedback : List (Result String String)
    }


type Status
    = Intact
    | Debouncing
    | Idle (List (Result String String))


type Delta delta
    = UpdateRequested delta
    | DebouncingStarted Time.Posix
    | DebouncingChecked Time.Posix
    | Noop


type Output output
    = Intact_
    | Debouncing_
    | FailedToParse String
    | Parsed output


type alias ViewConfig input delta msg =
    { id : String
    , toMsg : delta -> msg
    , input : input
    , status : Status
    }



-- Field widgets and functions


int : String -> Builder String String Int (Html msg) msg
int id =
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


float : String -> Builder String String Float (Html msg) msg
float id =
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


field_string_view : ViewConfig String String msg -> Html msg
field_string_view fieldState =
    H.div [ HA.style "margin-bottom" "30px" ]
        [ H.div
            [ HA.style "margin-bottom" "10px" ]
            [ H.text fieldState.id ]
        , H.input
            [ HE.onInput fieldState.toMsg
            , HA.value fieldState.input
            , HA.style "background-color"
                (case fieldState.status of
                    Intact ->
                        "white"

                    Debouncing ->
                        "lightYellow"

                    Idle feedback ->
                        if List.any Result.Extra.isErr feedback then
                            "pink"

                        else
                            "paleGreen"
                )
            ]
            [ H.text fieldState.input ]
        , case fieldState.status of
            Idle [] ->
                H.div
                    [ HA.style "margin-top" "5px" ]
                    [ H.text "✅ Looking good!" ]

            Idle feedback ->
                H.div []
                    (List.map
                        (\f ->
                            let
                                ( icon, txt ) =
                                    case f of
                                        Ok t ->
                                            ( "💬", t )

                                        Err t ->
                                            ( "❌", t )
                            in
                            H.div
                                [ HA.style "margin-top" "5px" ]
                                [ H.text (icon ++ " " ++ txt) ]
                        )
                        feedback
                    )

            _ ->
                H.text ""
        ]


string : String -> Builder String String String (Html msg) msg
string id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = field_string_view
    , parse = Ok
    , validators = []
    , debounce = 500
    }


debounce :
    Float
    -> Builder input delta output element msg
    -> Builder input delta output element msg
debounce millis fb =
    { fb | debounce = millis }


failIf :
    (output -> Bool)
    -> String
    -> Builder input delta output element msg
    -> Builder input delta output element msg
failIf check feedback fb =
    { fb | validators = { check = check, feedback = Err feedback } :: fb.validators }


infoIf :
    (output -> Bool)
    -> String
    -> Builder input delta output element msg
    -> Builder input delta output element msg
infoIf check feedback fb =
    { fb | validators = { check = check, feedback = Ok feedback } :: fb.validators }


view :
    (formData -> msg)
    -> Interface input delta output formData element msg
    -> State input delta output
    -> element
view toMsg field_ state =
    field_.view
        { id = field_.id
        , toMsg = \x -> toMsg (field_.toFormData (UpdateRequested x))
        , input = state.input
        , status =
            case state.output of
                Intact_ ->
                    Intact

                Debouncing_ ->
                    Debouncing

                FailedToParse feedback ->
                    Idle [ Err feedback ]

                Parsed _ ->
                    Idle state.feedback
        }


update :
    Interface input delta output formData element msg
    -> State input delta output
    -> State input delta output
    -> ( State input delta output, Cmd formData )
update field_ delta state =
    case delta.delta of
        UpdateRequested d ->
            let
                newState =
                    { state | input = field_.update d state.input }
            in
            if field_.debounce > 0 then
                ( newState
                , Task.perform
                    (field_.toFormData << DebouncingStarted)
                    Time.now
                )

            else
                ( parse field_ newState
                , Cmd.none
                )

        DebouncingStarted now ->
            ( { state
                | lastTouched = Just now
                , output = Debouncing_
              }
            , Task.perform
                (\() -> field_.toFormData (DebouncingChecked now))
                (Process.sleep field_.debounce)
            )

        DebouncingChecked now ->
            ( if state.lastTouched == Just now then
                parse field_ state

              else
                state
            , Cmd.none
            )

        Noop ->
            ( state
            , Cmd.none
            )


parse :
    Interface input delta output formData element msg
    -> State input delta output
    -> State input delta output
parse field_ state =
    let
        output =
            case field_.parse state.input of
                Err f ->
                    FailedToParse f

                Ok val ->
                    Parsed val
    in
    { state | output = output }


indexIfParsed :
    Interface input delta output formData element msg
    -> State input delta output
    -> State input delta output
    -> Maybe Int
indexIfParsed field_ delta state =
    case delta.delta of
        UpdateRequested _ ->
            if field_.debounce <= 0 then
                Just field_.index

            else
                Nothing

        DebouncingChecked now ->
            if state.lastTouched == Just now then
                Just field_.index

            else
                Nothing

        _ ->
            Nothing
