module Example2 exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Result.Extra


type MyCustom
    = Red Int
    | Green String
    | Blue


type alias PetOwner =
    { name : String
    , age : Int
    , pet : Pet
    }


type alias Pet =
    { name : String
    , age : Int
    }


type alias InputState state delta =
    { state : state, delta : Maybe delta }


type alias PetState =
    ( InputState String String
    , ( InputState String String
      , ( InputState String String
        , End
        )
      )
    )


type alias PetOwnerState =
    ( InputState String String
    , ( InputState String String
      , ( InputState String String
          --   , ( InputState PetState PetState
        , End
        )
      )
    )


type Msg
    = FormMsg PetOwnerState


main : Program () PetOwnerState Msg
main =
    Browser.element
        { init = \() -> ( example.init, Cmd.none )
        , view = example.view
        , update = \msg model -> ( example.update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


view state =
    H.div [] (example.view state)



-- form_new :
--     (PetOwnerState -> Msg)
--     -> Input PetOwnerState PetOwnerState PetOwner (Html PetOwnerState) PetOwnerState
--     -> Input2 PetOwnerState PetOwnerState PetOwner (Html Msg) Msg


form_new toMsg input =
    let
        i =
            input toMsg
    in
    { view =
        \state ->
            i.view
                { state = state
                , status = Intact
                , id = ""
                , toMsg = toMsg
                }
    , update = i.update
    , init = i.init
    }


example =
    form_new FormMsg
        (input_record
            "pet-owner"
            PetOwner
            |> input_field f0 (input_string "name")
            |> input_field f1 (input_int "age")
            |> input_field f2
                (input_int "blah")
            -- (input_record "pet" Pet
            --     |> input_field f0 (input_string "pet-name")
            --     |> input_field f1 (input_int "pet-age")
            --     |> input_field f2 (input_int "pet-bullshit")
            --     |> input_endRecord
            -- )
            -- |> input_field f2
            --    (input_customType
            --        |> input_tag1 f0 Red input_int
            --        |> input_tag1 f1 Green input_string
            --        |> input_tag0 f2 Blue
            --        |> input_endCustomType
            --    )
            |> input_endRecord
        )


input_record id toOutput =
    { id = id
    , toOutput = toOutput
    , fields = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , initializer = identity
    }


input_field sel f rec =
    { id = rec.id
    , toOutput = rec.toOutput
    , fields = rec.fields << Tuple.pair { field = f, selector = instantiateSelector sel }
    , states = rec.states << Tuple.pair { state = \toMsg -> (f toMsg).init, delta = Nothing }
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> recordStateViewer
    , initializer = rec.initializer >> recordInitializer
    }


input_endRecord rec =
    let
        fields =
            rec.fields End

        inits =
            rec.states End
    in
    \toMsg ->
        { id = rec.id
        , init = initializeRecord rec.initializer toMsg inits
        , update = \delta state -> updateRecordStates rec.updater toMsg fields delta state
        , view = \{ state } -> viewRecordStates rec.viewer toMsg inits fields state
        , parse = \state -> Err ""
        , validators = []
        , debounce = 0
        , toMsg = toMsg
        }


type End
    = End


initializeRecord initializer toMsg inits =
    initializer (\_ End -> End) toMsg inits


recordInitializer next toMsg ( init, inits ) =
    ( init toMsg, next toMsg inits )


viewRecordStates viewer toMsg inits fields states =
    viewer (\list _ _ End End -> list) [] toMsg inits fields states
        |> List.reverse


recordStateViewer next list toMsg emptyDeltas ( { field, selector }, fields ) ( { state }, states ) =
    let
        field_ =
            field toMsg
    in
    next
        (field_.view
            { state = state
            , status = Intact
            , toMsg = \delta -> selector.set (\data -> { data | delta = Just delta }) emptyDeltas |> field_.toMsg
            , id = field_.id
            }
            ++ list
        )
        emptyDeltas
        fields
        states


updateRecordStates updater toMsg fields deltas states =
    updater (\_ End End End -> End) toMsg fields deltas states


recordStateUpdater next toMsg ( { field }, fields ) ( { delta }, deltas ) ( state, states ) =
    let
        field_ =
            field toMsg
    in
    ( case delta of
        Nothing ->
            state

        Just d ->
            { delta = Nothing, state = field_.update d state.state }
    , next fields deltas states
    )


type alias Input state delta output element msg =
    (delta -> msg)
    ->
        { id : String
        , init : state
        , update : delta -> state -> state
        , view : ViewConfig state delta msg -> List element
        , parse : state -> Result String output
        , validators : List { check : output -> Bool, feedback : Result String String }
        , debounce : Float
        , toMsg : delta -> msg
        }


type alias Input2 state delta output element msg =
    { id : String
    , init : state
    , update : delta -> state -> state
    , view : ViewConfig state delta msg -> List element
    , parse : state -> Result String output
    , validators : List { check : output -> Bool, feedback : Result String String }
    , debounce : Float
    , toMsg : delta -> msg
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
    \toMsg ->
        { id = id
        , init = "1"
        , update = \delta _ -> delta
        , view = \config -> stringView { toMsg = toMsg, id = id, state = config.state, status = config.status }
        , parse =
            \input ->
                case String.toInt input of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err "must be a whole number"
        , validators = []
        , debounce = 0
        , toMsg = toMsg
        }


input_string : String -> Input String String String (Html msg) msg
input_string id =
    \toMsg ->
        { id = id
        , init = "I'm a string!"
        , update = \delta _ -> delta
        , view = \config -> stringView { toMsg = toMsg, id = id, state = config.state, status = config.status }
        , parse = Ok
        , validators = []
        , debounce = 500
        , toMsg = toMsg
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


composeSelectors selector1 selector2 =
    { getFieldState = selector1.getFieldState >> selector2.getFieldState
    , getField = selector1.getField >> selector2.getField
    , set = selector1.set >> selector2.set
    }


instantiateSelector selector =
    let
        s =
            selector
                { getFieldState = Tuple.first
                , getField = Tuple.first
                , set = identity
                }
    in
    { getFieldState = s.getFieldState
    , getField = s.getField
    , set = \item state -> s.set (Tuple.mapFirst item) state
    }


stringView : ViewConfig String String msg -> List (Html msg)
stringView fieldState =
    [ H.div [ HA.style "margin-bottom" "30px" ]
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
