module Example2 exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Result.Extra



-- Userland types


type
    Msg
    -- = FormMsg StringInputDelta
    -- = FormMsg PetOwnerInputDelta
    = FormMsg (CustomTypeDelta PetOwnerInputDelta)


mainForm =
    -- input_toForm FormMsg simpleInput
    -- input_toForm FormMsg recordInput
    input_toForm FormMsg customInput


type PetOwnerCustom
    = Age Int
    | Name String
    | Pet_ Pet


type alias PetOwnerRecord =
    { name : String
    , age : Int
    , pet : Pet
    }


type alias PetOwnerInputState =
    States3
        IntInputState
        StringInputState
        PetInputState


type alias PetOwnerInputDelta =
    Deltas3
        IntInputDelta
        StringInputDelta
        PetInputDelta


type alias Pet =
    { petName : String
    , petAge : Int
    }


type alias PetInputState =
    States2
        StringInputState
        IntInputState


type alias PetInputDelta =
    Deltas2
        StringInputDelta
        IntInputDelta


simpleInput : Input String String Int
simpleInput =
    input_int "hello"


recordInput : Input PetOwnerInputState PetOwnerInputDelta PetOwnerRecord
recordInput =
    input_record "pet owner"
        (\name age pet ->
            { name = name
            , age = age
            , pet = pet
            }
        )
        |> input_field f0 (input_string "name")
        |> input_field f1 (input_int "age")
        |> input_field f2
            (input_record "pet" Pet
                |> input_field f0 (input_string "pet's name")
                |> input_field f1 (input_int "pet's age")
                |> input_endRecord
            )
        |> input_endRecord


customInput : Input (CustomTypeState PetOwnerInputState) (CustomTypeDelta PetOwnerInputDelta) PetOwnerCustom
customInput =
    input_customType "custom type"
        |> input_variant f0 (input_tag1 Age (input_int "red number"))
        |> input_variant f1 (input_tag1 Name (input_string "green string"))
        |> input_variant f2
            (input_tag1 Pet_
                (input_record "pet" Pet
                    |> input_field f0 (input_string "pet's name")
                    |> input_field f1 (input_int "pet's age")
                    |> input_endRecord
                )
            )
        |> input_endCustomType


main =
    Browser.element
        { init = \() -> ( mainForm.init, Cmd.none )
        , view = mainForm.view
        , update =
            \msg model ->
                case msg of
                    FormMsg m ->
                        let
                            newModel =
                                mainForm.update m model

                            _ =
                                Debug.log "output" (mainForm.submit newModel)
                        in
                        ( newModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }



-- Library types


type End
    = End


type alias Form state delta output msg =
    { init : state
    , update : delta -> state -> state
    , view : state -> Html msg
    , submit : state -> Result (List String) output
    }


type alias Input state delta output =
    { id : String
    , index : Int
    , init : state
    , update : delta -> state -> state
    , view : ViewConfig state -> Html delta
    , parse : state -> Result (List String) output
    , validators : List { check : output -> Bool, feedback : Result String String }
    , debounce : Float
    }


type alias ViewConfig state =
    { id : String
    , state : state
    , status : Status
    }


type Status
    = Intact
    | Debouncing
    | Idle (List (Result String String))


type alias IntInputState =
    String


type alias IntInputDelta =
    String


type alias StringInputState =
    String


type alias StringInputDelta =
    String


type alias States1 a =
    ( a, End )


type alias Deltas1 a =
    ( Maybe a, End )


type alias States2 a b =
    ( a, ( b, End ) )


type alias Deltas2 a b =
    ( Maybe a, ( Maybe b, End ) )


type alias States3 a b c =
    ( a, ( b, ( c, End ) ) )


type alias Deltas3 a b c =
    ( Maybe a, ( Maybe b, ( Maybe c, End ) ) )


type alias States4 a b c d =
    ( a, ( b, ( c, ( d, End ) ) ) )


type alias Deltas4 a b c d =
    ( Maybe a, ( Maybe b, ( Maybe c, ( Maybe d, End ) ) ) )


type alias States5 a b c d e =
    ( a, ( b, ( c, ( d, ( e, End ) ) ) ) )


type alias Deltas5 a b c d e =
    ( Maybe a, ( Maybe b, ( Maybe c, ( Maybe d, ( Maybe e, End ) ) ) ) )


type alias CustomTypeState variants =
    { tagStates : variants
    , selectedTag : Int
    }


type CustomTypeDelta variants
    = TagSelected Int
    | TagDeltaReceived variants


input_toForm : (delta -> msg) -> Input state delta output -> Form state delta output msg
input_toForm toMsg input =
    { init = input.init
    , update = input.update
    , view = \state -> input.view { state = state, status = Intact, id = input.id } |> H.map toMsg
    , submit = input.parse
    }


input_int : String -> Input String String Int
input_int id =
    { id = id
    , index = 0
    , init = "1"
    , update = \delta _ -> delta
    , view = stringView
    , parse =
        \input ->
            case String.toInt input of
                Just i ->
                    Ok i

                Nothing ->
                    Err [ "must be a whole number" ]
    , validators = []
    , debounce = 0
    }


input_string : String -> Input String String String
input_string id =
    { id = id
    , index = 0
    , init = "I'm a string!"
    , update = \delta _ -> delta
    , view = stringView
    , parse = Ok
    , validators = []
    , debounce = 500
    }


input_tag1 : (a -> b) -> Input state delta a -> Input state delta b
input_tag1 tagger input =
    { id = input.id
    , index = 0
    , init = input.init
    , update = input.update
    , view = input.view
    , parse = input.parse >> Result.map tagger
    , validators = []
    , debounce = 0
    }


input_record id toOutput =
    { id = id
    , index = 0
    , toOutput = toOutput
    , fields = identity
    , deltas = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    }


input_field sel input rec =
    { id = rec.id
    , index = rec.index + 1
    , toOutput = rec.toOutput
    , fields = rec.fields << Tuple.pair { field = { input | index = rec.index }, selector = instantiateSelector sel }
    , deltas = rec.deltas << Tuple.pair Nothing
    , states = rec.states << Tuple.pair input.init
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> recordStateViewer
    , parser = rec.parser >> recordStateParser
    }


input_endRecord rec =
    let
        fields =
            rec.fields End

        emptyDeltas =
            rec.deltas End

        inits =
            rec.states End
    in
    { id = rec.id
    , index = 0
    , init = inits
    , update = \delta state -> updateRecordStates rec.updater fields delta state
    , view = \{ state } -> viewRecordStates rec.viewer emptyDeltas fields state
    , parse = \state -> parseRecordStates rec.parser rec.toOutput fields state
    , validators = []
    , debounce = 0
    }


parseRecordStates parser toOutput fields states =
    parser (\output End End -> output) (Ok toOutput) fields states


recordStateParser next toOutputResult ( { field }, fields ) ( state, states ) =
    next
        (case ( toOutputResult, field.parse state ) of
            ( Ok toOutput, Ok parsed ) ->
                Ok (toOutput parsed)

            ( Ok _, Err es ) ->
                Err es

            ( Err es, Ok _ ) ->
                Err es

            ( Err es, Err es2 ) ->
                Err (es ++ es2)
        )
        fields
        states


viewRecordStates viewer emptyDeltas fields states =
    viewer (\list _ End End -> list) [] emptyDeltas fields states
        |> List.reverse
        |> H.div []


recordStateViewer next list emptyDeltas ( { field, selector }, fields ) ( state, states ) =
    next
        ((field.view
            { state = state
            , status = Intact
            , id = field.id
            }
            |> H.map
                (\delta ->
                    selector.set (\_ -> Just delta) emptyDeltas
                )
         )
            :: list
        )
        emptyDeltas
        fields
        states


updateRecordStates updater fields deltas states =
    updater (\End End End -> End) fields deltas states


recordStateUpdater next ( { field }, fields ) ( delta, deltas ) ( state, states ) =
    ( case delta of
        Nothing ->
            state

        Just d ->
            field.update d state
    , next fields deltas states
    )


input_customType id =
    { id = id
    , index = 0
    , names = []
    , fields = identity
    , deltas = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    }


input_variant sel input rec =
    { id = rec.id
    , index = rec.index + 1
    , names = input.id :: rec.names
    , fields =
        rec.fields
            << Tuple.pair
                { field = { input | index = rec.index }
                , selector = instantiateSelector sel
                }
    , deltas = rec.deltas << Tuple.pair Nothing
    , states = rec.states << Tuple.pair input.init
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> selectedTagViewer
    , parser = rec.parser >> selectedTagParser
    }


input_endCustomType rec =
    let
        fields =
            rec.fields End

        emptyDeltas =
            rec.deltas End

        inits =
            rec.states End
    in
    { id = rec.id
    , index = 0
    , init = { tagStates = inits, selectedTag = 0 }
    , update =
        \delta state ->
            case delta of
                TagSelected idx ->
                    { state | selectedTag = idx }

                TagDeltaReceived tagDelta ->
                    { state | tagStates = updateRecordStates rec.updater fields tagDelta state.tagStates }
    , view =
        \{ state } ->
            H.div []
                [ H.text rec.id
                , H.div []
                    (List.indexedMap
                        (\index name -> H.button [ HE.onClick (TagSelected index) ] [ H.text name ])
                        (List.reverse rec.names)
                    )
                , viewSelectedTagState rec.viewer state.selectedTag emptyDeltas fields state.tagStates
                ]
    , parse = \state -> parseSelectedTagState rec.parser state.selectedTag fields state.tagStates
    , validators = []
    , debounce = 0
    }


parseSelectedTagState parser selectedTag fields states =
    parser
        (\result _ End End -> result)
        (Err [ "tag index " ++ String.fromInt selectedTag ++ " not found" ])
        selectedTag
        fields
        states


selectedTagParser next result selectedTag ( { field }, fields ) ( state, states ) =
    next
        (if field.index == selectedTag then
            field.parse state

         else
            result
        )
        selectedTag
        fields
        states


viewSelectedTagState viewer selectedTag emptyDeltas fields states =
    viewer (\maybeView _ _ End End -> maybeView) Nothing selectedTag emptyDeltas fields states
        |> Maybe.map (H.map TagDeltaReceived)
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next maybeView selectedTag emptyDeltas ( { field, selector }, fields ) ( state, states ) =
    next
        (if field.index == selectedTag then
            Just
                (field.view
                    { state = state
                    , status = Intact
                    , id = field.id
                    }
                    |> H.map
                        (\delta ->
                            selector.set (\_ -> Just delta) emptyDeltas
                        )
                )

         else
            maybeView
        )
        selectedTag
        emptyDeltas
        fields
        states



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
            selector { getFieldState = Tuple.first, getField = Tuple.first, set = identity }
    in
    { getFieldState = s.getFieldState
    , getField = s.getField
    , set = \updater state -> s.set (Tuple.mapFirst updater) state
    }


stringView : ViewConfig String -> Html String
stringView fieldState =
    H.div [ HA.style "margin-bottom" "30px" ]
        [ H.div
            [ HA.style "margin-bottom" "10px" ]
            [ H.text fieldState.id ]
        , H.input
            [ HE.onInput identity
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
