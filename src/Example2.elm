module Example2 exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Result.Extra



-- Userland types


type MyCustom
    = Red Int
    | Green String
    | Blue


type alias PetOwner =
    { name : String
    , age : Int
    , pet : Pet

    -- , blah : MyCustom
    }


type alias Pet =
    { petName : String
    , petAge : Int
    }


type Msg
    = FormMsg PetOwnerInput


type alias PetInput =
    Record2
        StringInput
        IntInput


type alias PetOwnerInput =
    Record3
        StringInput
        IntInput
        (NestedRecord PetInput)


toForm : (delta -> msg) -> Input state delta output -> Form state delta output msg
toForm toMsg input =
    { init = input.init
    , update = input.update
    , view = \state -> input.view { state = state, status = Intact, id = input.id } |> H.map toMsg
    , submit = input.parse
    }


petOwnerForm : Form PetOwnerInput PetOwnerInput PetOwner Msg
petOwnerForm =
    input_record "pet owner" PetOwner
        |> input_field f0 (input_string "name")
        |> input_field f1 (input_int "age")
        |> input_field f2
            (input_record "pet" Pet
                |> input_field f0 (input_string "pet's name")
                |> input_field f1 (input_int "pet's age")
                |> input_endRecord
            )
        -- |> input_field f3
        --     (input_customType ""
        --         |> input_tag1 f0 Red (input_int "red number")
        --         |> input_tag1 f1 Green (input_string "green stringr")
        --         |> input_endCustomType
        --     )
        |> input_endRecord
        |> toForm FormMsg


main : Program () PetOwnerInput Msg
main =
    Browser.element
        { init = \() -> ( petOwnerForm.init, Cmd.none )
        , view = petOwnerForm.view
        , update =
            \msg model ->
                case msg of
                    FormMsg m ->
                        let
                            newModel =
                                petOwnerForm.update m model

                            _ =
                                Debug.log "output" (petOwnerForm.submit newModel)
                        in
                        ( newModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }



-- Library types


type alias InputState state delta =
    { state : state, delta : Maybe delta }


type alias NestedRecord a =
    InputState a a


type alias Record1 a =
    ( a, End )


type alias Record2 a b =
    ( a, ( b, End ) )


type alias Record3 a b c =
    ( a, ( b, ( c, End ) ) )


type alias Record4 a b c d =
    ( a, ( b, ( c, ( d, End ) ) ) )


type alias Record5 a b c d e =
    ( a, ( b, ( c, ( d, ( e, End ) ) ) ) )


type alias Form state delta output msg =
    { init : state
    , update : delta -> state -> state
    , view : state -> Html msg
    , submit : state -> Result String output
    }


input_record id toOutput =
    { id = id
    , toOutput = toOutput
    , fields = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    }


input_field sel f rec =
    { id = rec.id
    , toOutput = rec.toOutput
    , fields = rec.fields << Tuple.pair { field = f, selector = instantiateSelector sel }
    , states = rec.states << Tuple.pair { state = f.init, delta = Nothing }
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> recordStateViewer
    , parser = rec.parser >> recordStateParser
    }


input_endRecord rec =
    let
        fields =
            rec.fields End

        inits =
            rec.states End
    in
    { id = rec.id
    , index = 0
    , init = inits
    , update = \delta state -> updateRecordStates rec.updater fields delta state
    , view = \{ state } -> viewRecordStates rec.viewer inits fields state
    , parse = \state -> parseRecordStates rec.parser rec.toOutput fields state
    , validators = []
    , debounce = 0
    }


input_customType id =
    { id = id
    , fields = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    }


input_tag1 sel tag input rec =
    let
        f =
            input_record input.id tag
                |> input_field f0 input
                |> input_endRecord
    in
    { id = rec.id
    , fields =
        rec.fields
            << Tuple.pair
                { field = f
                , selector = instantiateSelector sel
                }
    , states = rec.states << Tuple.pair { state = f.init, delta = Nothing }
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> selectedTagViewer
    , parser = rec.parser >> recordStateParser
    }


input_endCustomType rec =
    let
        fields =
            rec.fields End

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
    , view = Debug.todo "view" --\{ state } -> viewSelectedTagState rec.viewer inits fields state.tagStates
    , parse = \state -> Debug.todo "parseSelectedTagState"
    , validators = []
    , debounce = 0
    }


type CustomTypeDelta delta
    = TagSelected Int
    | TagDeltaReceived delta


type End
    = End


parseRecordStates parser toOutput fields states =
    parser (\output End End -> output) (Ok toOutput) fields states


recordStateParser next toOutputResult ( { field }, fields ) ( { state }, states ) =
    next
        (case ( toOutputResult, field.parse state ) of
            ( Ok toOutput, Ok parsed ) ->
                Ok (toOutput parsed)

            _ ->
                Err ""
        )
        fields
        states


viewSelectedTagState viewer selectedTag inits fields states =
    viewer (\maybeView _ _ End End -> maybeView) Nothing selectedTag inits fields states
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next _ selectedTag emptyDeltas ( { field, selector }, fields ) ( { state }, states ) =
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
                            selector.set (\data -> { data | delta = Just delta }) emptyDeltas
                        )
                )

         else
            Nothing
        )
        selectedTag
        emptyDeltas
        fields
        states


viewRecordStates viewer inits fields states =
    viewer (\list _ End End -> list) [] inits fields states
        |> List.reverse
        |> H.div []


recordStateViewer next list emptyDeltas ( { field, selector }, fields ) ( { state }, states ) =
    next
        ((field.view
            { state = state
            , status = Intact
            , id = field.id
            }
            |> H.map
                (\delta ->
                    selector.set (\data -> { data | delta = Just delta }) emptyDeltas
                )
         )
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


type alias Input state delta output =
    { id : String
    , index : Int
    , init : state
    , update : delta -> state -> state
    , view : ViewConfig state -> Html delta
    , parse : state -> Result String output
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


type alias IntInput =
    InputState String String


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
                    Err "must be a whole number"
    , validators = []
    , debounce = 0
    }


type alias StringInput =
    InputState String String


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
