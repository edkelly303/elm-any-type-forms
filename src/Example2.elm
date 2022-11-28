module Example2 exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Result.Extra



{-
   db    db .d8888. d88888b d8888b. db       .d8b.  d8b   db d8888b.
   88    88 88'  YP 88'     88  `8D 88      d8' `8b 888o  88 88  `8D
   88    88 `8bo.   88ooooo 88oobY' 88      88ooo88 88V8o 88 88   88
   88    88   `Y8b. 88~~~~~ 88`8b   88      88~~~88 88 V8o88 88   88
   88b  d88 db   8D 88.     88 `88. 88booo. 88   88 88  V888 88  .8D
   ~Y8888P' `8888Y' Y88888P 88   YD Y88888P YP   YP VP   V8P Y8888D'
-}


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


type
    Msg
    -- = FormMsg (Delta String)
    -- = FormMsg (Delta SimpleRecordInputDelta)
    -- = FormMsg (Delta NestedRecordInputDelta)
    = FormMsg (Delta SimpleCustomInputDelta)


mainForm =
    -- int
    -- simpleRecordInput
    -- nestedRecordInput
    simpleCustomInput
        |> toForm "form" FormMsg


type alias SimpleRecord =
    { name : String
    , age : Int
    }


type alias Deltas1 a =
    ( Delta a, End )


type alias Deltas2 a b =
    ( Delta a, Deltas1 b )


type alias SimpleRecordInputDelta =
    Deltas2
        String
        String


simpleRecordInput :
    Input
        ( String, ( String, End ) )
        SimpleRecordInputDelta
        SimpleRecord
simpleRecordInput =
    record SimpleRecord
        |> field i0 "name" string
        |> field i1 "age" int
        |> endRecord


type alias NestedRecord =
    { record : SimpleRecord
    }


type alias NestedRecordInputDelta =
    Deltas1 SimpleRecordInputDelta


nestedRecordInput : Input ( ( String, ( String, End ) ), End ) NestedRecordInputDelta NestedRecord
nestedRecordInput =
    record NestedRecord
        |> field i0 "record" simpleRecordInput
        |> endRecord


type SimpleCustom
    = Red
    | Green String


type alias SimpleCustomInputDelta =
    CustomTypeDelta ( Delta Deltas0, ( Delta ( Delta String, End ), End ) )


simpleCustomInput :
    Input
        { tagStates : ( States0, ( ( String, End ), End ) ), selectedTag : Int }
        SimpleCustomInputDelta
        SimpleCustom
simpleCustomInput =
    customType
        |> tag0 i0 "red" Red
        |> tag1 i1 "green" Green string
        |> endCustomType



{-
   d888888b db    db d8888b. d88888b .d8888.
   `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
      88     `8bd8'  88oodD' 88ooooo `8bo.
      88       88    88~~~   88~~~~~   `Y8b.
      88       88    88      88.     db   8D
      YP       YP    88      Y88888P `8888Y'
-}


type End
    = End


type alias Form state delta output msg =
    { init : state
    , update : Delta delta -> state -> state
    , view : state -> Html msg
    , submit : state -> Result (List String) output
    }


type Delta delta
    = Skip
    | Delta delta


type alias InputConfig state delta output =
    { init : state
    , update : delta -> state -> state
    , view : ViewConfig state -> Html delta
    , parse : state -> Result (List String) output
    }


type Input state delta output
    = Input
        (String
         ->
            { id : String
            , index : Int
            , init : state
            , update : Delta delta -> state -> state
            , view : ViewConfig state -> Html (Delta delta)
            , parse : state -> Result (List String) output
            , validators : List { check : output -> Bool, feedback : Result String String }
            , debounce : Float
            }
        )


type alias ViewConfig state =
    { id : String
    , state : state
    , status : Status
    }


type Status
    = Intact
    | Debouncing
    | Idle (List (Result String String))



{-
   d88888b  .d88b.  d8888b. .88b  d88. .d8888.
   88'     .8P  Y8. 88  `8D 88'YbdP`88 88'  YP
   88ooo   88    88 88oobY' 88  88  88 `8bo.
   88~~~   88    88 88`8b   88  88  88   `Y8b.
   88      `8b  d8' 88 `88. 88  88  88 db   8D
   YP       `Y88P'  88   YD YP  YP  YP `8888Y'
-}


toForm : String -> (Delta delta -> msg) -> Input state delta output -> Form state delta output msg
toForm id toMsg (Input input) =
    let
        { init, update, view, parse } =
            input id
    in
    { init = init
    , update = update
    , view = \state -> view { state = state, status = Intact, id = id } |> H.map toMsg
    , submit = parse
    }



{-
   d888888b d8b   db d8888b. db    db d888888b .d8888.
     `88'   888o  88 88  `8D 88    88 `~~88~~' 88'  YP
      88    88V8o 88 88oodD' 88    88    88    `8bo.
      88    88 V8o88 88~~~   88    88    88      `Y8b.
     .88.   88  V888 88      88b  d88    88    db   8D
   Y888888P VP   V8P 88      ~Y8888P'    YP    `8888Y'

-}


fromConfig : InputConfig state delta output -> Input state delta output
fromConfig config =
    Input
        (\id ->
            { id = id
            , index = 0
            , init = config.init
            , update =
                \wrappedDelta state ->
                    case wrappedDelta of
                        Skip ->
                            state

                        Delta delta ->
                            config.update delta state
            , view = config.view >> H.map Delta
            , parse = config.parse
            , validators = []
            , debounce = 0
            }
        )


type alias IntInputState =
    String


type alias IntInputDelta =
    String


int : Input String String Int
int =
    fromConfig
        { init = ""
        , update = \delta _ -> delta
        , view = stringView
        , parse =
            \input ->
                case String.toInt input of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "must be a whole number" ]
        }


type alias StringInputState =
    String


type alias StringInputDelta =
    String


string : Input String String String
string =
    fromConfig
        { init = ""
        , update = \delta _ -> delta
        , view = stringView
        , parse = Ok
        }


type States0
    = States0


type Deltas0
    = Deltas0


always : output -> Input States0 Deltas0 output
always output =
    Input
        (\id ->
            { id = id
            , index = 0
            , init = States0
            , update = \_ _ -> States0
            , view = \_ -> H.text ""
            , parse = \_ -> Ok output
            , validators = []
            , debounce = 0
            }
        )


type alias States1 a =
    ( a, End )


type alias States2 a b =
    ( a, States1 b )


type alias MaybeInputState state =
    CustomTypeState
        (States2
            (States1 state)
            States0
        )


type alias MaybeInputDelta delta =
    CustomTypeDelta
        (Deltas2
            (Deltas1 delta)
            Deltas0
        )


maybe : Input state delta output -> Input (MaybeInputState state) (MaybeInputDelta delta) (Maybe output)
maybe input =
    customType
        |> tag1 i0 "Just" Just input
        |> tag0 i1 "Nothing" Nothing
        |> endCustomType



{-
   d8888b. d88888b  .o88b.  .d88b.  d8888b. d8888b. .d8888.
   88  `8D 88'     d8P  Y8 .8P  Y8. 88  `8D 88  `8D 88'  YP
   88oobY' 88ooooo 8P      88    88 88oobY' 88   88 `8bo.
   88`8b   88~~~~~ 8b      88    88 88`8b   88   88   `Y8b.
   88 `88. 88.     Y8b  d8 `8b  d8' 88 `88. 88  .8D db   8D
   88   YD Y88888P  `Y88P'  `Y88P'  88   YD Y8888D' `8888Y'
-}


record toOutput =
    { index = 0
    , toOutput = toOutput
    , fields = identity
    , deltas = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    }


field sel id (Input input) rec =
    let
        i =
            input id
    in
    { index = rec.index + 1
    , toOutput = rec.toOutput
    , fields = rec.fields << Tuple.pair { field = { i | index = rec.index }, selector = instantiateSelector sel }
    , deltas = rec.deltas << Tuple.pair Skip
    , states = rec.states << Tuple.pair i.init
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> recordStateViewer
    , parser = rec.parser >> recordStateParser
    }


endRecord rec =
    let
        fields =
            rec.fields End

        emptyDeltas =
            rec.deltas End

        inits =
            rec.states End
    in
    Input
        (\id ->
            { id = id
            , index = 0
            , init = inits
            , update =
                \delta state ->
                    case delta of
                        Skip ->
                            state

                        Delta deltas ->
                            updateRecordStates rec.updater fields deltas state
            , view = \{ state } -> viewRecordStates rec.viewer emptyDeltas fields state
            , parse = \state -> parseRecordStates rec.parser rec.toOutput fields state
            , validators = []
            , debounce = 0
            }
        )



{-
   d8888b. d88888b  .o88b.  .d88b.  d8888b. d8888b.      d888888b d8b   db d888888b d88888b d8888b. d8b   db  .d8b.  db      .d8888.
   88  `8D 88'     d8P  Y8 .8P  Y8. 88  `8D 88  `8D        `88'   888o  88 `~~88~~' 88'     88  `8D 888o  88 d8' `8b 88      88'  YP
   88oobY' 88ooooo 8P      88    88 88oobY' 88   88         88    88V8o 88    88    88ooooo 88oobY' 88V8o 88 88ooo88 88      `8bo.
   88`8b   88~~~~~ 8b      88    88 88`8b   88   88         88    88 V8o88    88    88~~~~~ 88`8b   88 V8o88 88~~~88 88        `Y8b.
   88 `88. 88.     Y8b  d8 `8b  d8' 88 `88. 88  .8D        .88.   88  V888    88    88.     88 `88. 88  V888 88   88 88booo. db   8D
   88   YD Y88888P  `Y88P'  `Y88P'  88   YD Y8888D'      Y888888P VP   V8P    YP    Y88888P 88   YD VP   V8P YP   YP Y88888P `8888Y'
-}


parseRecordStates parser toOutput fns states =
    parser (\output End End -> output) (Ok toOutput) fns states


recordStateParser next toOutputResult ( fns, restFns ) ( state, restStates ) =
    next
        (case ( toOutputResult, fns.field.parse state ) of
            ( Ok toOutput, Ok parsed ) ->
                Ok (toOutput parsed)

            ( Ok _, Err es ) ->
                Err es

            ( Err es, Ok _ ) ->
                Err es

            ( Err es, Err es2 ) ->
                Err (es ++ es2)
        )
        restFns
        restStates


viewRecordStates viewer emptyDeltas fns states =
    viewer (\list _ End End -> list) [] emptyDeltas fns states
        |> List.reverse
        |> H.div []


recordStateViewer next list emptyDeltas ( fns, restFns ) ( state, restStates ) =
    next
        ((fns.field.view
            { state = state
            , status = Intact
            , id = fns.field.id
            }
            |> H.map
                (\delta ->
                    Delta (fns.selector.set (\_ -> delta) emptyDeltas)
                )
         )
            :: list
        )
        emptyDeltas
        restFns
        restStates


updateRecordStates updater fields deltas states =
    updater (\End End End -> End) fields deltas states


recordStateUpdater next ( fns, restFns ) ( delta, restDeltas ) ( state, restStates ) =
    ( fns.field.update delta state
    , next restFns restDeltas restStates
    )



{-
    .o88b. db    db .d8888. d888888b  .d88b.  .88b  d88.      d888888b db    db d8888b. d88888b .d8888.
   d8P  Y8 88    88 88'  YP `~~88~~' .8P  Y8. 88'YbdP`88      `~~88~~' `8b  d8' 88  `8D 88'     88'  YP
   8P      88    88 `8bo.      88    88    88 88  88  88         88     `8bd8'  88oodD' 88ooooo `8bo.
   8b      88    88   `Y8b.    88    88    88 88  88  88         88       88    88~~~   88~~~~~   `Y8b.
   Y8b  d8 88b  d88 db   8D    88    `8b  d8' 88  88  88         88       88    88      88.     db   8D
    `Y88P' ~Y8888P' `8888Y'    YP     `Y88P'  YP  YP  YP         YP       YP    88      Y88888P `8888Y'
-}


type alias CustomTypeState variants =
    { tagStates : variants
    , selectedTag : Int
    }


type CustomTypeDelta variants
    = TagSelected Int
    | TagDeltaReceived variants


customType =
    { index = 0
    , names = []
    , fns = identity
    , deltas = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    }


variant sel id (Input input) rec =
    let
        i =
            input id
    in
    { index = rec.index + 1
    , names = id :: rec.names
    , fns =
        rec.fns
            << Tuple.pair
                { field = { i | index = rec.index }
                , selector = instantiateSelector sel
                }
    , deltas = rec.deltas << Tuple.pair Skip
    , states = rec.states << Tuple.pair i.init
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> selectedTagViewer
    , parser = rec.parser >> selectedTagParser
    }


tag0 f id tag =
    variant f id (always tag)


tag1 f id tag input =
    variant f
        id
        (record tag
            |> field i0 "0" input
            |> endRecord
        )


tag2 f id tag input1 input2 =
    variant f
        id
        (record tag
            |> field i0 "0" input1
            |> field i1 "1" input2
            |> endRecord
        )


tag3 f id tag input1 input2 input3 =
    variant f
        id
        (record tag
            |> field i0 "0" input1
            |> field i1 "1" input2
            |> field i2 "2" input3
            |> endRecord
        )


endCustomType rec =
    let
        fns =
            rec.fns End

        emptyDeltas =
            rec.deltas End

        inits =
            rec.states End

        names =
            List.reverse rec.names
    in
    Input
        (\id ->
            { id = id
            , index = 0
            , init = { tagStates = inits, selectedTag = 0 }
            , update =
                \delta state ->
                    case delta of
                        Skip ->
                            state

                        Delta (TagSelected idx) ->
                            { state | selectedTag = idx }

                        Delta (TagDeltaReceived tagDelta) ->
                            { state | tagStates = updateRecordStates rec.updater fns tagDelta state.tagStates }
            , view =
                \{ state } ->
                    let
                        selectedName =
                            List.Extra.getAt state.selectedTag names
                                |> Maybe.withDefault "ERROR"
                    in
                    H.div []
                        [ H.text id
                        , H.div []
                            (List.indexedMap
                                (\index name ->
                                    H.button
                                        [ HE.onClick (Delta (TagSelected index)) ]
                                        [ H.text name ]
                                )
                                names
                            )
                        , H.text selectedName
                        , viewSelectedTagState rec.viewer state.selectedTag emptyDeltas fns state.tagStates
                        ]
            , parse = \state -> parseSelectedTagState rec.parser state.selectedTag fns state.tagStates
            , validators = []
            , debounce = 0
            }
        )



{-
    .o88b. db    db .d8888. d888888b  .d88b.  .88b  d88.      d888888b d8b   db d888888b d88888b d8888b. d8b   db  .d8b.  db      .d8888.
   d8P  Y8 88    88 88'  YP `~~88~~' .8P  Y8. 88'YbdP`88        `88'   888o  88 `~~88~~' 88'     88  `8D 888o  88 d8' `8b 88      88'  YP
   8P      88    88 `8bo.      88    88    88 88  88  88         88    88V8o 88    88    88ooooo 88oobY' 88V8o 88 88ooo88 88      `8bo.
   8b      88    88   `Y8b.    88    88    88 88  88  88         88    88 V8o88    88    88~~~~~ 88`8b   88 V8o88 88~~~88 88        `Y8b.
   Y8b  d8 88b  d88 db   8D    88    `8b  d8' 88  88  88        .88.   88  V888    88    88.     88 `88. 88  V888 88   88 88booo. db   8D
    `Y88P' ~Y8888P' `8888Y'    YP     `Y88P'  YP  YP  YP      Y888888P VP   V8P    YP    Y88888P 88   YD VP   V8P YP   YP Y88888P `8888Y'
-}


parseSelectedTagState parser selectedTag fns states =
    parser
        (\result _ End End -> result)
        (Err [ "tag index " ++ String.fromInt selectedTag ++ " not found" ])
        selectedTag
        fns
        states


selectedTagParser next result selectedTag ( fns, restFns ) ( state, restStates ) =
    next
        (if fns.field.index == selectedTag then
            fns.field.parse state

         else
            result
        )
        selectedTag
        restFns
        restStates


viewSelectedTagState viewer selectedTag emptyDeltas fns states =
    viewer (\maybeView _ _ End End -> maybeView) Nothing selectedTag emptyDeltas fns states
        |> Maybe.map (H.map (Delta << TagDeltaReceived))
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next maybeView selectedTag emptyDeltas ( fns, restFns ) ( state, restStates ) =
    next
        (if fns.field.index == selectedTag then
            Just
                (fns.field.view
                    { state = state
                    , status = Intact
                    , id = fns.field.id
                    }
                    |> H.map
                        (\delta ->
                            fns.selector.set (\_ -> delta) emptyDeltas
                        )
                )

         else
            maybeView
        )
        selectedTag
        emptyDeltas
        restFns
        restStates



{-
   .d8888. d88888b db      d88888b  .o88b. d888888b  .d88b.  d8888b. .d8888.
   88'  YP 88'     88      88'     d8P  Y8 `~~88~~' .8P  Y8. 88  `8D 88'  YP
   `8bo.   88ooooo 88      88ooooo 8P         88    88    88 88oobY' `8bo.
     `Y8b. 88~~~~~ 88      88~~~~~ 8b         88    88    88 88`8b     `Y8b.
   db   8D 88.     88booo. 88.     Y8b  d8    88    `8b  d8' 88 `88. db   8D
   `8888Y' Y88888P Y88888P Y88888P  `Y88P'    YP     `Y88P'  88   YD `8888Y'
-}


i0 =
    composeSelectors { getFieldState = identity, getField = identity, set = identity }


i1 =
    composeSelectors { getFieldState = Tuple.second, getField = Tuple.second, set = Tuple.mapSecond }


i2 =
    i1 >> i1


i3 =
    i2 >> i1


i4 =
    i3 >> i1


i5 =
    i4 >> i1


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



{-
   db    db d888888b d88888b db   d8b   db .d8888.
   88    88   `88'   88'     88   I8I   88 88'  YP
   Y8    8P    88    88ooooo 88   I8I   88 `8bo.
   `8b  d8'    88    88~~~~~ Y8   I8I   88   `Y8b.
    `8bd8'    .88.   88.     `8b d8'8b d8' db   8D
      YP    Y888888P Y88888P  `8b8' `8d8'  `8888Y'
-}


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
