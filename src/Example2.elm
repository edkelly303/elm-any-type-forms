module Example2 exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Result.Extra


type alias MyOutput =
    { custom : MyCustom
    , record : MyRecord
    }


type MyCustom
    = Red Int
    | Green String
    | Blue


type alias MyRecord =
    { name : String
    , age : Int
    }


type Msg
    = FormMsg


main : Program () MyRecordState MyRecordState
main =
    Browser.element
        { init = \() -> ( example.init, Cmd.none )
        , view = view
        , update = \msg model -> ( example.update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view state =
    H.div []
        (example.view
            { state = state
            , status = Intact
            , id = ""
            , toMsg = identity
            }
        )


example :
    { id : String
    , init : MyRecordState
    , update : MyRecordState -> MyRecordState -> MyRecordState
    , view : { n | state : MyRecordState } -> List (Html MyRecordState)
    , parse : t -> Result String value
    , validators : List u
    , debounce : number
    }
example =
    -- form_new FormMsg
    input_record "my-record" MyRecord
        |> input_field f0 (input_string "name")
        |> input_field f1 (input_int "age")
        -- |> input_field f2
        --    (input_customType
        --        |> input_tag1 f0 Red input_int
        --        |> input_tag1 f1 Green input_string
        --        |> input_tag0 f2 Blue
        --        |> input_endCustomType
        --    )
        |> input_endRecord


input_record id toOutput =
    { id = id
    , toOutput = toOutput
    , fields = identity
    , states = identity
    , updater = identity
    , viewer = identity
    }


type alias InputState state delta =
    { state : state, delta : Maybe delta }


type alias MyRecordState =
    ( InputState String String
    , ( InputState String String
      , End
      )
    )


input_field sel f rec =
    { id = rec.id
    , toOutput = rec.toOutput
    , fields = rec.fields >> Tuple.pair { field = f, selector = sel }
    , states = rec.states >> Tuple.pair { state = f.init, delta = Nothing }
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> recordStateViewer
    }


input_endRecord rec =
    let
        fields =
            rec.fields End

        inits =
            rec.states End
    in
    { id = rec.id
    , init = inits
    , update = \delta state -> updateRecordStates rec.updater fields delta state
    , view = \{ state } -> viewRecordStates rec.viewer inits fields state
    , parse = \state -> Err ""
    , validators = []
    , debounce = 0
    }


type End
    = End


viewRecordStates viewer inits fields states =
    viewer (\list _ End End -> list) [] inits fields states


recordStateViewer next list emptyDeltas ( { field, selector }, fields ) ( { state }, states ) =
    next
        (field.view
            { state = state
            , status = Intact
            , toMsg =
                \delta ->
                    let
                        sel =
                            instantiateSelector selector
                    in
                    set sel.set (\data -> { data | delta = Just delta }) emptyDeltas
            , id = field.id
            }
            :: list
        )
        emptyDeltas
        fields
        states


updateRecordStates updater fields deltas states =
    updater (\End End End -> End) fields deltas states


recordStateUpdater next ( { field }, fields ) ( { delta }, deltas ) ( state, states ) =
    ( case delta of
        Nothing ->
            state

        Just d ->
            { delta = Nothing, state = field.update d state.state }
    , next fields deltas states
    )


type alias Input state delta output element msg =
    { id : String
    , init : state
    , update : delta -> state -> state
    , view : ViewConfig state delta msg -> element
    , parse : state -> Result String output
    , validators : List { check : output -> Bool, feedback : Result String String }
    , debounce : Float
    }


type alias ViewConfig state delta msg =
    { id : String
    , toMsg : delta -> msg
    , state : state
    , status : Status
    }


type Status
    = Intact
    | Debouncing
    | Idle (List (Result String String))


input_int : String -> Input String String Int (Html msg) msg
input_int id =
    { id = id
    , init = "1"
    , update = \delta _ -> delta
    , view = stringView
    , parse =
        \input ->
            case String.toInt input of
                Just i ->
                    Ok i

                Nothing ->
                    Err "must be a whole number"
    , validators = []
    , debounce = 0
    }


input_string : String -> Input String String String (Html msg) msg
input_string id =
    { id = id
    , init = "I'm a string!"
    , update = \delta _ -> delta
    , view = stringView
    , parse = Ok
    , validators = []
    , debounce = 500
    }



-- STUFF


f0 =
    composeSelectors { getFieldState = identity, getField = identity, set = identity }


f1 =
    composeSelectors { getFieldState = Tuple.second, getField = Tuple.second, set = Tuple.mapSecond }


f2 =
    f1 >> f1


f3 =
    f2 >> f1


f4 =
    f3 >> f1


get : (b -> ( a, c )) -> b -> a
get getter data =
    (getter >> Tuple.first) data


set : ((( a, b ) -> ( x, b )) -> c -> d) -> (a -> x) -> c -> d
set setter updater data =
    setter (Tuple.mapFirst updater) data


composeSelectors selector1 selector2 =
    { getFieldState = selector1.getFieldState >> selector2.getFieldState
    , getField = selector1.getField >> selector2.getField
    , set = selector1.set >> selector2.set
    }


instantiateSelector selector =
    -- selector { getFieldState = Tuple.first, getField = Tuple.first, set = Tuple.mapFirst }
    selector { getFieldState = identity, getField = identity, set = identity }


stringView : ViewConfig String String msg -> Html msg
stringView fieldState =
    H.div [ HA.style "margin-bottom" "30px" ]
        [ H.div
            [ HA.style "margin-bottom" "10px" ]
            [ H.text fieldState.id ]
        , H.input
            [ HE.onInput fieldState.toMsg
            , HA.value fieldState.state
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
            [ H.text fieldState.state ]
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
