module Form exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import List.Extra
import Process
import Result.Extra
import Task
import Time



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
    { init : State state
    , update : Delta delta -> State state -> ( State state, Cmd msg )
    , view : State state -> Html msg
    , submit : State state -> Result (List ( String, String )) output
    }


type Delta delta
    = Skip
    | ChangeState delta
    | StartDebouncing Time.Posix
    | CheckDebouncer Time.Posix


type State state
    = State InternalState state


type InternalState
    = Intact_
    | DebouncingSince Time.Posix
    | Idle_


type alias InputConfig state delta output =
    { init : state
    , update : delta -> state -> state
    , view : ViewConfig state -> Html delta
    , parse : state -> Result (List String) output
    , validators : List { check : output -> Bool, feedback : String }
    , debounce : Float
    }


type Input state delta output
    = Input
        (String
         ->
            { id : String
            , index : Int
            , init : State state
            , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , view : ViewConfig state -> Html (Delta delta)
            , parse : State state -> Result (List ( String, String )) output
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
    | Idle (List (Result ( String, String ) ( String, String )))


type States0
    = States0


type alias States1 a =
    ( State a, End )


type alias States2 a b =
    ( State a, States1 b )


type alias States3 a b c =
    ( State a, States2 b c )


type Deltas0
    = Deltas0


type alias Deltas1 a =
    ( Delta a, End )


type alias Deltas2 a b =
    ( Delta a, Deltas1 b )


type alias Deltas3 a b c =
    ( Delta a, Deltas2 b c )



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
    , update =
        \msg state ->
            update msg state
                |> Tuple.mapSecond (Cmd.map toMsg)
    , view =
        \(State internalState state) ->
            view
                { state = state
                , status = statusFromInternalState parse (State internalState state)
                , id = id
                }
                |> H.map toMsg
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
            , init = State Intact_ config.init
            , update = wrappedUpdate (\d s -> config.update d s |> (\ns -> ( ns, Cmd.none ))) config.debounce
            , view = config.view >> H.map ChangeState
            , parse =
                \(State _ state) ->
                    case config.parse state of
                        Ok output ->
                            let
                                feedback =
                                    List.filterMap
                                        (\v ->
                                            if v.check output then
                                                Just ( id, v.feedback )

                                            else
                                                Nothing
                                        )
                                        config.validators
                            in
                            if List.isEmpty feedback then
                                Ok output

                            else
                                Err feedback

                        Err errs ->
                            Err (List.map (\err -> ( id, err )) errs)
            }
        )


wrappedUpdate : (delta -> state -> ( state, Cmd delta )) -> Float -> (Delta delta -> State state -> ( State state, Cmd (Delta delta) ))
wrappedUpdate update debounce_ =
    \wrappedDelta (State internalState state) ->
        case wrappedDelta of
            Skip ->
                ( State internalState state
                , Cmd.none
                )

            ChangeState delta ->
                let
                    ( newState, cmd ) =
                        update delta state
                in
                if debounce_ > 0 then
                    ( State internalState newState
                    , Cmd.batch
                        [ Task.perform StartDebouncing Time.now
                        , Cmd.map ChangeState cmd
                        ]
                    )

                else
                    ( State Idle_ newState
                    , Cmd.map ChangeState cmd
                    )

            StartDebouncing now ->
                ( State (DebouncingSince now) state
                , Task.perform (\() -> CheckDebouncer now) (Process.sleep debounce_)
                )

            CheckDebouncer now ->
                case internalState of
                    DebouncingSince startTime ->
                        if now == startTime then
                            ( State Idle_ state
                            , Cmd.none
                            )

                        else
                            ( State internalState state
                            , Cmd.none
                            )

                    _ ->
                        ( State internalState state
                        , Cmd.none
                        )


failIf : (output -> Bool) -> String -> InputConfig state delta output -> InputConfig state delta output
failIf check feedback config =
    { config | validators = { check = check, feedback = feedback } :: config.validators }


debounce : Float -> InputConfig state delta output -> InputConfig state delta output
debounce millis config =
    { config | debounce = millis }


int : Input String String Int
int =
    fromConfig intConfig


intConfig : InputConfig String String Int
intConfig =
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
    , validators = []
    , debounce = 500
    }


string : Input String String String
string =
    fromConfig stringConfig


stringConfig : InputConfig String String String
stringConfig =
    { init = ""
    , update = \delta _ -> delta
    , view = stringView
    , parse = Ok
    , validators = []
    , debounce = 500
    }


always : output -> Input States0 Deltas0 output
always output =
    fromConfig
        { init = States0
        , update = \_ _ -> States0
        , view = \_ -> H.text ""
        , parse = \_ -> Ok output
        , validators = []
        , debounce = 0
        }


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
        |> tag1 i0 "Just" Just "" input
        |> tag0 i1 "Nothing" Nothing
        |> endCustomType


type alias ListState state =
    List (State state)


type ListDelta delta
    = InsertItem Int
    | DeleteItem Int
    | ChangeItem Int (Delta delta)


list : Input state delta output -> Input (List (State state)) (ListDelta delta) (List output)
list (Input toInput) =
    let
        input =
            toInput ""
    in
    Input
        (\id ->
            { id = id
            , index = 0
            , init = State Intact_ []
            , update =
                wrappedUpdate
                    (\delta state ->
                        case delta of
                            InsertItem idx ->
                                let
                                    before =
                                        List.take idx state

                                    after =
                                        List.drop idx state
                                in
                                ( before ++ input.init :: after, Cmd.none )

                            ChangeItem idx itemDelta ->
                                let
                                    ( newState, cmds ) =
                                        List.foldr
                                            (\( i, item ) ( items, cmds_ ) ->
                                                if i == idx then
                                                    let
                                                        ( newItem, newCmd ) =
                                                            input.update itemDelta item
                                                    in
                                                    ( newItem :: items, newCmd :: cmds_ )

                                                else
                                                    ( item :: items, cmds_ )
                                            )
                                            ( [], [] )
                                            (List.indexedMap Tuple.pair state)
                                in
                                ( newState, Cmd.batch cmds |> Cmd.map (ChangeItem idx) )

                            DeleteItem idx ->
                                ( List.Extra.removeAt idx state, Cmd.none )
                    )
                    0
            , view =
                \config ->
                    H.div [ HA.style "margin-bottom" "10px" ]
                        [ H.div [ HA.style "margin-bottom" "10px" ] [ H.text config.id ]
                        , if List.isEmpty config.state then
                            H.div
                                [ HA.style "margin-top" "10px"
                                , HA.style "margin-bottom" "10px"
                                ]
                                [ H.text "[ empty list ]"
                                , H.button
                                    [ HA.style "margin-left" "10px"
                                    , HE.onClick (InsertItem 0)
                                    ]
                                    [ H.text "add an item" ]
                                ]

                          else
                            borderedDiv
                                [ H.div
                                    [ HA.style "margin-top" "10px"
                                    , HA.style "margin-bottom" "10px"
                                    ]
                                    [ H.button [ HE.onClick (InsertItem 0) ]
                                        [ H.text "insert item" ]
                                    ]
                                , H.div []
                                    (List.indexedMap
                                        (\idx (State internalState state) ->
                                            H.div [ HA.style "margin-bottom" "10px" ]
                                                [ input.view
                                                    { state = state
                                                    , status = statusFromInternalState input.parse (State internalState state)
                                                    , id = "list item #" ++ String.fromInt idx
                                                    }
                                                    |> H.map (ChangeItem idx)
                                                , H.div [ HA.style "margin-top" "10px" ] [ H.button [ HE.onClick (DeleteItem idx) ] [ H.text ("delete item #" ++ String.fromInt idx) ] ]
                                                , H.div [ HA.style "margin-top" "10px" ] [ H.button [ HE.onClick (InsertItem (idx + 1)) ] [ H.text "insert item" ] ]
                                                ]
                                        )
                                        config.state
                                    )
                                ]
                        ]
                        |> H.map ChangeState
            , parse =
                \(State _ state) ->
                    List.foldr
                        (\( idx, item ) res ->
                            case res of
                                Ok outputs ->
                                    case input.parse item of
                                        Ok output ->
                                            Ok (output :: outputs)

                                        Err errs ->
                                            Err (List.map (\( _, feedback ) -> "item #" ++ String.fromInt idx ++ ": " ++ feedback) errs)

                                Err errs ->
                                    case input.parse item of
                                        Ok _ ->
                                            Err errs

                                        Err newErrs ->
                                            Err (List.map (\( _, feedback ) -> "item #" ++ String.fromInt idx ++ ": " ++ feedback) newErrs ++ errs)
                        )
                        (Ok [])
                        (List.indexedMap Tuple.pair state)
                        |> Result.mapError (List.map (\feedback -> ( id, feedback )))
            }
        )



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
            , init = State Intact_ inits
            , update =
                \delta (State s state) ->
                    case delta of
                        Skip ->
                            ( State s state, Cmd.none )

                        ChangeState deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater emptyDeltas fields deltas state
                            in
                            ( State s newState
                            , Cmd.map ChangeState cmd
                            )

                        _ ->
                            ( State s state, Cmd.none )
            , view =
                \{ state } ->
                    viewRecordStates rec.viewer emptyDeltas fields state
                        |> recordView id
            , parse = \(State _ state) -> parseRecordStates rec.parser rec.toOutput fields state
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
    viewer (\views _ End End -> views) [] emptyDeltas fns states
        |> List.reverse
        |> H.div []


recordStateViewer next views emptyDeltas ( fns, restFns ) ( State internalState state, restStates ) =
    next
        ((fns.field.view
            { state = state
            , status = statusFromInternalState fns.field.parse (State internalState state)
            , id = fns.field.id
            }
            |> H.map
                (\delta ->
                    ChangeState (fns.selector.set (\_ -> delta) emptyDeltas)
                )
         )
            :: views
        )
        emptyDeltas
        restFns
        restStates


statusFromInternalState parser (State internalState state) =
    case internalState of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                strings =
                    case parser (State internalState state) of
                        Ok _ ->
                            []

                        Err errs ->
                            errs
            in
            Idle (List.map Err strings)


updateRecordStates updater emptyDeltas fields deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output _ End End End -> output)
                { newStates = identity, newCmds = [] }
                emptyDeltas
                fields
                deltas
                states
    in
    ( newStates End, Cmd.batch newCmds )


recordStateUpdater next { newStates, newCmds } emptyDeltas ( fns, restFns ) ( delta, restDeltas ) ( state, restStates ) =
    let
        ( newState, newCmd ) =
            fns.field.update delta state

        cmd2 =
            newCmd
                |> Cmd.map
                    (\d ->
                        fns.selector.set (\_ -> d) emptyDeltas
                    )
    in
    next
        { newStates = newStates << Tuple.pair newState
        , newCmds = cmd2 :: newCmds
        }
        emptyDeltas
        restFns
        restDeltas
        restStates



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


tag1 f id tag inputId input =
    variant f
        id
        (record tag
            |> field i0 inputId input
            |> endRecord
        )


tag2 f id tag id1 input1 id2 input2 =
    variant f
        id
        (record tag
            |> field i0 id1 input1
            |> field i1 id2 input2
            |> endRecord
        )


tag3 f id tag id1 input1 id2 input2 id3 input3 =
    variant f
        id
        (record tag
            |> field i0 id1 input1
            |> field i1 id2 input2
            |> field i2 id3 input3
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
            , init = State Intact_ { tagStates = inits, selectedTag = 0 }
            , update =
                \delta (State s state) ->
                    case delta of
                        Skip ->
                            ( State s state, Cmd.none )

                        ChangeState (TagSelected idx) ->
                            ( State s { state | selectedTag = idx }, Cmd.none )

                        ChangeState (TagDeltaReceived tagDelta) ->
                            let
                                ( newTagStates, cmd ) =
                                    updateRecordStates rec.updater emptyDeltas fns tagDelta state.tagStates
                            in
                            ( State s { state | tagStates = newTagStates }
                            , Cmd.map (ChangeState << TagDeltaReceived) cmd
                            )

                        _ ->
                            ( State s state, Cmd.none )
            , view =
                \config ->
                    let
                        options =
                            List.indexedMap Tuple.pair names

                        selectedTag =
                            config.state.selectedTag

                        tagStates =
                            config.state.tagStates
                    in
                    viewSelectedTagState rec.viewer selectedTag emptyDeltas fns tagStates
                        |> customTypeView config.id options selectedTag
            , parse = \(State _ state) -> parseSelectedTagState rec.parser state.selectedTag fns state.tagStates
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
        (Err [ ( "FATAL ERROR", "tag index " ++ String.fromInt selectedTag ++ " not found" ) ])
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
        |> Maybe.map (H.map (ChangeState << TagDeltaReceived))
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next maybeView selectedTag emptyDeltas ( fns, restFns ) ( State s state, restStates ) =
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


recordView : String -> Html (Delta msg) -> Html (Delta msg)
recordView id recordStatesView =
    H.div
        []
        [ H.div [ HA.style "margin-bottom" "10px" ] [ H.text id ]
        , borderedDiv [ recordStatesView ]
        ]


customTypeView id options selectedTag selectedTagView =
    H.div [ HA.style "margin-top" "10px" ]
        [ H.div [ HA.style "margin-bottom" "10px" ] [ H.text id ]
        , borderedDiv
            [ radioView options id selectedTag (ChangeState << TagSelected)
            , borderedDiv [ selectedTagView ]
            ]
        ]


stringView : ViewConfig String -> Html String
stringView fieldState =
    H.div [ HA.style "margin-bottom" "10px" ]
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
                                    Ok ( _, t ) ->
                                        ( "💬", t )

                                    Err ( _, t ) ->
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


borderedDiv : List (Html msg) -> Html msg
borderedDiv content =
    H.div
        [ HA.style "border-width" "1px"
        , HA.style "border-color" "lightGray"
        , HA.style "border-style" "solid"
        ]
        [ H.div
            [ HA.style "margin" "10px" ]
            content
        ]


radioView : List ( state, String ) -> String -> state -> (state -> msg) -> Html msg
radioView options id selectedOption toMsg =
    H.div [ HA.style "margin-bottom" "10px" ]
        (List.map
            (\( option, label ) ->
                H.span [ HA.style "margin-right" "10px" ]
                    [ H.input
                        [ HA.type_ "radio"
                        , HA.id (id ++ label)
                        , HA.name id
                        , HA.value label
                        , HA.checked (selectedOption == option)
                        , onChecked (toMsg option)
                        ]
                        []
                    , H.label
                        [ HA.for (id ++ label) ]
                        [ H.text label ]
                    ]
            )
            options
        )


onChecked : msg -> H.Attribute msg
onChecked msg =
    HE.on "input"
        (HE.targetChecked
            |> Json.Decode.andThen
                (\checked ->
                    if checked then
                        Json.Decode.succeed msg

                    else
                        Json.Decode.fail ""
                )
        )
