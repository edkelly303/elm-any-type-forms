module Field exposing
    ( Delta(..)
    , FieldBuilder
    , Output(..)
    , State
    , Status(..)
    , ViewConfig
    , debounce
    , failIf
    , float
    , infoIf
    , int
    , string
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Result.Extra
import Time


type alias FieldBuilder input delta output element msg =
    { id : String
    , init : input
    , update : delta -> input -> input
    , view : ViewConfig msg -> element
    , parse : input -> Result String output
    , validators : List { check : output -> Bool, feedback : Result String String }
    , debounce : Float
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


type alias ViewConfig msg =
    { id : String
    , toMsg : String -> msg
    , input : String
    , status : Status
    }



-- Field widgets and functions


int : String -> FieldBuilder String String Int (Html msg) msg
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


float : String -> FieldBuilder String String Float (Html msg) msg
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


field_string_view : ViewConfig msg -> Html msg
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


string : String -> FieldBuilder String String String (Html msg) msg
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
    -> FieldBuilder input delta output element msg
    -> FieldBuilder input delta output element msg
debounce millis fb =
    { fb | debounce = millis }


failIf :
    (output -> Bool)
    -> String
    -> FieldBuilder input delta output element msg
    -> FieldBuilder input delta output element msg
failIf check feedback fb =
    { fb | validators = { check = check, feedback = Err feedback } :: fb.validators }


infoIf :
    (output -> Bool)
    -> String
    -> FieldBuilder input delta output element msg
    -> FieldBuilder input delta output element msg
infoIf check feedback fb =
    { fb | validators = { check = check, feedback = Ok feedback } :: fb.validators }
