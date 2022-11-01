module Field exposing
    ( Builder
    , Delta(..)
    , Interface
    , ListField
    , MaybeField
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
    , list
    , maybe
    , parse
    , radio
    , string
    , update
    , validate
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JSD
import List.Extra
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
    , debounce : Float
    , index : Int
    , parse : input -> Result String output
    , update : delta -> input -> input
    , toFormData : Delta delta -> formData
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


type alias MaybeField input delta output =
    State
        (Maybe input)
        (MaybeDelta delta)
        (Maybe output)


type MaybeDelta delta
    = Off
    | On
    | InnerDelta delta


maybe :
    Builder input delta output (Html msg) msg
    -> Builder (Maybe input) (MaybeDelta delta) (Maybe output) (Html msg) msg
maybe innerBuilder =
    { id = innerBuilder.id
    , init = Nothing
    , update =
        \delta input ->
            case ( delta, input ) of
                ( InnerDelta innerDelta, Just innerInput ) ->
                    Just (innerBuilder.update innerDelta innerInput)

                ( InnerDelta _, Nothing ) ->
                    Nothing

                ( Off, _ ) ->
                    Nothing

                ( On, _ ) ->
                    Just innerBuilder.init
    , view =
        \{ input, toMsg, id, status } ->
            H.div []
                [ H.div []
                    [ H.input
                        [ HA.type_ "radio"
                        , HA.id (id ++ "_yes")
                        , HA.name (id ++ "_maybe")
                        , HA.value "yes"
                        , HA.checked (input /= Nothing)
                        , onChecked (toMsg On)
                        ]
                        []
                    , H.label
                        [ HA.for (id ++ "_yes") ]
                        [ H.text "yes" ]
                    , H.input
                        [ HA.type_ "radio"
                        , HA.id (id ++ "_no")
                        , HA.name (id ++ "_maybe")
                        , HA.value "no"
                        , HA.checked (input == Nothing)
                        , onChecked (toMsg Off)
                        ]
                        []
                    , H.label
                        [ HA.for (id ++ "_no") ]
                        [ H.text "no" ]
                    ]
                , case input of
                    Just innerInput ->
                        innerBuilder.view
                            { id = innerBuilder.id
                            , toMsg = InnerDelta >> toMsg
                            , input = innerInput
                            , status = Idle []
                            }

                    Nothing ->
                        H.text ""
                , statusView status
                ]
    , parse =
        \input ->
            case input of
                Just innerInput ->
                    case innerBuilder.parse innerInput of
                        Ok output ->
                            Ok (Just output)

                        Err e ->
                            Err e

                Nothing ->
                    Ok Nothing
    , validators =
        List.map
            (\{ check, feedback } ->
                { check =
                    \maybeOutput ->
                        case maybeOutput of
                            Nothing ->
                                False

                            Just output ->
                                check output
                , feedback = feedback
                }
            )
            innerBuilder.validators
    , debounce = 0
    }


type ListDelta itemDelta
    = ItemDelta itemDelta
    | AddItem
    | DeleteItem Int


type alias ListField input delta output =
    State
        { list : List output
        , item : input
        }
        (ListDelta delta)
        (List output)


list :
    (output -> String)
    -> Builder input delta output (Html msg) msg
    ->
        Builder
            { list : List output
            , item : input
            }
            (ListDelta delta)
            (List output)
            (Html msg)
            msg
list outputToString itemBuilder =
    { id = itemBuilder.id
    , init = { list = [], item = itemBuilder.init }
    , update =
        \delta input ->
            case delta of
                ItemDelta itemDelta ->
                    let
                        item =
                            input.item
                    in
                    { input | item = itemBuilder.update itemDelta item }

                AddItem ->
                    case itemBuilder.parse input.item of
                        Ok output ->
                            { input | list = output :: input.list }

                        Err _ ->
                            input

                DeleteItem idx ->
                    { input | list = List.Extra.removeAt idx input.list }
    , view =
        \{ input, toMsg, status } ->
            H.div
                []
                [ itemBuilder.view
                    { id = itemBuilder.id
                    , toMsg = ItemDelta >> toMsg
                    , input = input.item
                    , status = Idle []
                    }
                , H.button
                    [ HA.style "margin-top" "10px"
                    , HE.onClick (toMsg AddItem)
                    ]
                    [ H.text "add" ]
                , H.div [ HA.style "margin-top" "10px" ]
                    (List.indexedMap
                        (\idx item ->
                            H.button
                                [ HA.style "margin-right" "5px"
                                , HE.onClick (toMsg (DeleteItem idx))
                                ]
                                [ H.text (outputToString item)
                                , H.text " x"
                                ]
                        )
                        input.list
                    )
                , statusView status
                ]
    , parse = \input -> Ok input.list
    , validators =
        List.map
            (\{ check, feedback } ->
                { check = \listOutput -> List.any check listOutput
                , feedback = feedback
                }
            )
            itemBuilder.validators
    , debounce = 0
    }


radio :
    String
    -> List ( String, option )
    -> Builder (Maybe option) option option (Html msg) msg
radio id options =
    { id = id
    , init = Nothing
    , update =
        \delta input ->
            if Just delta == input then
                Nothing

            else
                Just delta
    , view = radioView options
    , parse = Result.fromMaybe "must select something"
    , validators = []
    , debounce = 0
    }


radioView : List ( String, option ) -> ViewConfig (Maybe option) option msg -> Html msg
radioView options { id, toMsg, input } =
    H.div [ HA.style "margin-top" "30px" ]
        [ H.div
            [ HA.style "margin-top" "10px" ]
            [ H.text id ]
        , H.div []
            (List.map
                (\( label, opt ) ->
                    H.div []
                        [ H.input
                            [ HA.type_ "radio"
                            , HA.id (id ++ label)
                            , HA.name id
                            , HA.value label
                            , HA.checked (input == Just opt)
                            , HA.style "margin-top" "10px"
                            , onChecked (toMsg opt)
                            ]
                            []
                        , H.label
                            [ HA.for (id ++ label) ]
                            [ H.text label ]
                        ]
                )
                options
            )
        ]


onChecked : msg -> H.Attribute msg
onChecked msg =
    HE.on "input"
        (HE.targetChecked
            |> JSD.andThen
                (\checked ->
                    if checked then
                        JSD.succeed msg

                    else
                        JSD.fail ""
                )
        )


int : String -> Builder String String Int (Html msg) msg
int id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = stringView
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
    , view = stringView
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


string : String -> Builder String String String (Html msg) msg
string id =
    { id = id
    , init = ""
    , update = \delta _ -> delta
    , view = stringView
    , parse = Ok
    , validators = []
    , debounce = 500
    }


stringView : ViewConfig String String msg -> Html msg
stringView fieldState =
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
        , statusView fieldState.status
        ]


statusView : Status -> Html msg
statusView status =
    case status of
        Idle [] ->
            H.div
                [ HA.style "margin-top" "10px", HA.style "margin-bottom" "10px" ]
                [ H.text "✅ Looking good!" ]

        Idle feedback ->
            H.div [ HA.style "margin-top" "10px" ]
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
                            [ HA.style "margin-bottom" "10px" ]
                            [ H.text (icon ++ " " ++ txt) ]
                    )
                    feedback
                )

        _ ->
            H.text ""


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


validate :
    { a | validators : List { check : output -> Bool, feedback : Result String String } }
    -> State input delta output
    -> State input delta output
validate fieldBuilder fieldState =
    let
        newFeedback =
            case fieldState.output of
                Parsed output ->
                    List.foldl
                        (\{ check, feedback } feedbacks ->
                            if check output then
                                feedback :: feedbacks

                            else
                                feedbacks
                        )
                        []
                        fieldBuilder.validators

                _ ->
                    []
    in
    { fieldState | feedback = newFeedback }
