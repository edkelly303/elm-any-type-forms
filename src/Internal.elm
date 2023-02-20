module Internal exposing
    ( Control
    , ControlConfig
    , Delta
    , End
    , Flag
    , Form
    , ListDelta
    , State
    , Status
    , ViewConfig
    , bool
    , checkMsgType
    , customType
    , debounce
    , end
    , enum
    , failIf
    , field
    , flagIf
    , float
    , fromControl
    , hiddenField
    , initWith
    , int
    , layout
    , list
    , makeControl
    , maybe
    , onFlag
    , readOnlyField
    , record
    , string
    , tag0
    , tag1
    , tag2
    , tuple
    , wrapper
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode
import List.Extra
import Path
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
    , initWith : output -> State state
    , update : Delta delta -> State state -> ( State state, Cmd msg )
    , view : State state -> Html msg
    , submit : State state -> Result { errors : List ( String, String ), state : State state } output
    }


type alias ControlConfig state delta output =
    { empty : state
    , initialise : output -> state
    , update : delta -> state -> state
    , view : ViewConfig state -> Html delta
    , parse : state -> Result (List String) output
    }


type alias Control state delta output =
    AdvancedControl output state delta output


type AdvancedControl input state delta output
    = Control (Path.Path -> ControlRecord input state delta output)


type alias ControlRecord input state delta output =
    { delta : Delta delta
    , index : Int
    , init : State state
    , initialise : input -> State state
    , baseUpdate :
        Float
        -> Delta delta
        -> State state
        -> ( State state, Cmd (Delta delta) )
    , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
    , view : ViewConfig state -> Html (Delta delta)
    , childViews : ViewConfig state -> List (Html (Delta delta))
    , parse : State state -> Result (List ( String, String )) output
    , path : Path.Path
    , emitFlags : State state -> List Flag
    , collectDebouncingReceivers : State state -> List Flag
    , receiveFlags : List Flag -> List ( String, String )
    , receiverCount : List Flag
    , setAllIdle : State state -> State state
    }


type Flag
    = FlagLabel String
    | FlagPath Path.Path Int


type alias ViewConfig state =
    { label : String
    , state : state
    , status : Status
    , flags : List Flag
    , selected : Int
    }


type Status
    = Intact
    | Debouncing
    | Idle (List (Result ( String, String ) ( String, String )))



{-
   .d8888. d888888b  .d8b.  d888888b d88888b .d8888.
   88'  YP `~~88~~' d8' `8b `~~88~~' 88'     88'  YP
   `8bo.      88    88ooo88    88    88ooooo `8bo.
     `Y8b.    88    88~~~88    88    88~~~~~   `Y8b.
   db   8D    88    88   88    88    88.     db   8D
   `8888Y'    YP    YP   YP    YP    Y88888P `8888Y'
-}


type State state
    = State InternalState state


type alias InternalState =
    { status : InternalStatus
    , selected : Int
    }


type InternalStatus
    = Intact_
    | DebouncingSince Time.Posix
    | Idle_



{-
   d8888b. d88888b db      d888888b  .d8b.  .d8888.
   88  `8D 88'     88      `~~88~~' d8' `8b 88'  YP
   88   88 88ooooo 88         88    88ooo88 `8bo.
   88   88 88~~~~~ 88         88    88~~~88   `Y8b.
   88  .8D 88.     88booo.    88    88   88 db   8D
   Y8888D' Y88888P Y88888P    YP    YP   YP `8888Y'
-}


type Delta delta
    = Skip
    | ChangeState delta
    | StartDebouncing Time.Posix
    | CheckDebouncer Time.Posix
    | TagSelected Int



{-
   d88888b  .d88b.  d8888b. .88b  d88. .d8888.
   88'     .8P  Y8. 88  `8D 88'YbdP`88 88'  YP
   88ooo   88    88 88oobY' 88  88  88 `8bo.
   88~~~   88    88 88`8b   88  88  88   `Y8b.
   88      `8b  d8' 88 `88. 88  88  88 db   8D
   YP       `Y88P'  88   YD YP  YP  YP `8888Y'
-}


fromControl : String -> (Delta delta -> msg) -> Control state delta output -> Form state delta output msg
fromControl label toMsg (Control control) =
    let
        { init, update, view, parse, setAllIdle, emitFlags, receiveFlags, collectDebouncingReceivers } =
            control Path.root
    in
    { init = init
    , initWith =
        \output ->
            let
                (Control initialisedControl) =
                    Control control
                        |> initWith output
            in
            initialisedControl Path.root
                |> .init
    , update =
        \msg state ->
            update msg state
                |> Tuple.mapSecond (Cmd.map toMsg)
    , view =
        \((State internalState state) as s) ->
            let
                emittedFlags =
                    emitFlags s
                        |> Debug.log "emitted flags"

                debouncingReceivers =
                    collectDebouncingReceivers s
                        |> Debug.log "debouncing receivers"

                flags =
                    List.filter (\f -> not <| List.member f debouncingReceivers) emittedFlags
                        |> Debug.log "flags"
            in
            H.div []
                [ H.h1 [] [ H.text label ]
                , H.div []
                    [ view
                        { state = state
                        , status = getStatus parse receiveFlags flags (State internalState state)
                        , label = label
                        , flags = flags
                        , selected = 0
                        }
                        |> H.map toMsg
                    ]
                ]
    , submit =
        \state ->
            let
                parsingResult =
                    parse state

                validationErrors =
                    state
                        |> emitFlags
                        |> receiveFlags
            in
            case ( parsingResult, validationErrors ) of
                ( Ok output, [] ) ->
                    Ok output

                ( Ok _, vErrs ) ->
                    Err { state = setAllIdle state, errors = vErrs }

                ( Err pErrs, vErrs ) ->
                    Err { state = setAllIdle state, errors = pErrs ++ vErrs }
    }



{-
    .o88b. db   db d88888b  .o88b. db   dD      .88b  d88. .d8888.  d888b       d888888b db    db d8888b. d88888b
   d8P  Y8 88   88 88'     d8P  Y8 88 ,8P'      88'YbdP`88 88'  YP 88' Y8b      `~~88~~' `8b  d8' 88  `8D 88'
   8P      88ooo88 88ooooo 8P      88,8P        88  88  88 `8bo.   88              88     `8bd8'  88oodD' 88ooooo
   8b      88~~~88 88~~~~~ 8b      88`8b        88  88  88   `Y8b. 88  ooo         88       88    88~~~   88~~~~~
   Y8b  d8 88   88 88.     Y8b  d8 88 `88.      88  88  88 db   8D 88. ~8~         88       88    88      88.
    `Y88P' YP   YP Y88888P  `Y88P' YP   YD      YP  YP  YP `8888Y'  Y888P          YP       YP    88      Y88888P


-}


checkMsgType : Control state delta output -> Delta delta
checkMsgType _ =
    Skip



{-
    .o88b.  .d88b.  d8b   db d888888b d8888b.  .d88b.  db      .d8888.
   d8P  Y8 .8P  Y8. 888o  88 `~~88~~' 88  `8D .8P  Y8. 88      88'  YP
   8P      88    88 88V8o 88    88    88oobY' 88    88 88      `8bo.
   8b      88    88 88 V8o88    88    88`8b   88    88 88        `Y8b.
   Y8b  d8 `8b  d8' 88  V888    88    88 `88. `8b  d8' 88booo. db   8D
    `Y88P'  `Y88P'  VP   V8P    YP    88   YD  `Y88P'  Y88888P `8888Y'
-}


makeControl : ControlConfig state delta output -> Control state delta output
makeControl config =
    Control
        (\path ->
            let
                preUpdate =
                    wrapUpdate
                        (\delta state ->
                            ( config.update delta state
                            , Cmd.none
                            )
                        )

                parse =
                    \(State _ state) ->
                        config.parse state
                            |> Result.mapError (List.map (Tuple.pair (Path.toString path)))
            in
            { path = path
            , index = 0
            , init = State { status = Intact_, selected = 0 } config.empty
            , delta = Skip
            , initialise = \input -> State { status = Intact_, selected = 0 } (config.initialise input)
            , baseUpdate = preUpdate
            , update = preUpdate 0
            , childViews = \_ -> []
            , view = config.view >> H.map ChangeState
            , parse = parse
            , setAllIdle = \(State i s) -> State { i | status = Idle_ } s
            , emitFlags = \_ -> []
            , collectDebouncingReceivers = \_ -> []
            , receiveFlags = \_ -> []
            , receiverCount = []
            }
        )


wrapUpdate : (delta -> state -> ( state, Cmd delta )) -> Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
wrapUpdate innerUpdate debounce_ wrappedDelta (State internalState state) =
    case wrappedDelta of
        Skip ->
            ( State internalState state
            , Cmd.none
            )

        ChangeState delta ->
            let
                ( newState, cmd ) =
                    innerUpdate delta state
            in
            if debounce_ > 0 then
                ( State internalState newState
                , Cmd.batch
                    [ Task.perform StartDebouncing Time.now
                    , Cmd.map ChangeState cmd
                    ]
                )

            else
                ( State { internalState | status = Idle_ } newState
                , Cmd.map ChangeState cmd
                )

        StartDebouncing now ->
            ( State { internalState | status = DebouncingSince now } state
            , Task.perform (\() -> CheckDebouncer now) (Process.sleep debounce_)
            )

        CheckDebouncer now ->
            case internalState.status of
                DebouncingSince startTime ->
                    if now == startTime then
                        ( State { internalState | status = Idle_ } state
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

        TagSelected idx ->
            ( State { internalState | selected = idx } state
            , Cmd.none
            )



{-
   db    db  .d8b.  db      d888888b d8888b.  .d8b.  d888888b d888888b  .d88b.  d8b   db
   88    88 d8' `8b 88        `88'   88  `8D d8' `8b `~~88~~'   `88'   .8P  Y8. 888o  88
   Y8    8P 88ooo88 88         88    88   88 88ooo88    88       88    88    88 88V8o 88
   `8b  d8' 88~~~88 88         88    88   88 88~~~88    88       88    88    88 88 V8o88
    `8bd8'  88   88 88booo.   .88.   88  .8D 88   88    88      .88.   `8b  d8' 88  V888
      YP    YP   YP Y88888P Y888888P Y8888D' YP   YP    YP    Y888888P  `Y88P'  VP   V8P
-}


flagIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
flagIf check flag (Control control) =
    Control (control >> flagEmitter check (FlagLabel flag))


flagEmitter : (output -> Bool) -> Flag -> ControlRecord input state delta output -> ControlRecord input state delta output
flagEmitter check flag ctrl =
    { ctrl
        | emitFlags =
            \state ->
                let
                    oldFlags =
                        ctrl.emitFlags state

                    newFlags =
                        case ctrl.parse state of
                            Ok output ->
                                if check output then
                                    [ flag ]

                                else
                                    []

                            Err _ ->
                                []
                in
                List.Extra.unique (oldFlags ++ newFlags)
    }


onFlag : String -> String -> Control state delta output -> Control state delta output
onFlag flag message (Control control) =
    Control (control >> flagReceiver (FlagLabel flag) message)


flagReceiver : Flag -> String -> ControlRecord input state delta output -> ControlRecord input state delta output
flagReceiver flag message ctrl =
    { ctrl
        | receiveFlags =
            \flags ->
                let
                    oldReceiver =
                        ctrl.receiveFlags flags

                    newReceiver =
                        if List.member flag flags then
                            [ ( Path.toString ctrl.path, message ) ]

                        else
                            []
                in
                List.Extra.unique (oldReceiver ++ newReceiver)
        , receiverCount = flag :: ctrl.receiverCount
        , collectDebouncingReceivers =
            \((State internalState _) as state) ->
                case internalState.status of
                    DebouncingSince _ ->
                        flag :: ctrl.collectDebouncingReceivers state

                    _ ->
                        ctrl.collectDebouncingReceivers state
    }


failIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
failIf check message (Control c) =
    Control
        (\path ->
            let
                control =
                    c path

                flag =
                    FlagPath path (List.length control.receiverCount)
            in
            control
                |> flagEmitter check flag
                |> flagReceiver flag message
        )



{-
   d8888b. d88888b d8888b.  .d88b.  db    db d8b   db  .o88b. d88888b
   88  `8D 88'     88  `8D .8P  Y8. 88    88 888o  88 d8P  Y8 88'
   88   88 88ooooo 88oooY' 88    88 88    88 88V8o 88 8P      88ooooo
   88   88 88~~~~~ 88~~~b. 88    88 88    88 88 V8o88 8b      88~~~~~
   88  .8D 88.     88   8D `8b  d8' 88b  d88 88  V888 Y8b  d8 88.
   Y8888D' Y88888P Y8888P'  `Y88P'  ~Y8888P' VP   V8P  `Y88P' Y88888P
-}


debounce : Float -> Control state delta output -> Control state delta output
debounce millis (Control control) =
    let
        debouncer i =
            { i | update = i.baseUpdate millis }
    in
    Control (control >> debouncer)



{-
   db       .d8b.  db    db  .d88b.  db    db d888888b
   88      d8' `8b `8b  d8' .8P  Y8. 88    88 `~~88~~'
   88      88ooo88  `8bd8'  88    88 88    88    88
   88      88~~~88    88    88    88 88    88    88
   88booo. 88   88    88    `8b  d8' 88b  d88    88
   Y88888P YP   YP    YP     `Y88P'  ~Y8888P'    YP
-}


layout :
    (List (Html (Delta delta)) -> ViewConfig state -> Html (Delta delta))
    -> Control state delta output
    -> Control state delta output
layout v (Control control) =
    let
        viewer i =
            { i | view = \config -> v (i.childViews config) config }
    in
    Control (control >> viewer)



{-
   d888888b d8b   db d888888b d888888b      d88888b d8888b.  .d88b.  .88b  d88.
     `88'   888o  88   `88'   `~~88~~'      88'     88  `8D .8P  Y8. 88'YbdP`88
      88    88V8o 88    88       88         88ooo   88oobY' 88    88 88  88  88
      88    88 V8o88    88       88         88~~~   88`8b   88    88 88  88  88
     .88.   88  V888   .88.      88         88      88 `88. `8b  d8' 88  88  88
   Y888888P VP   V8P Y888888P    YP         YP      88   YD  `Y88P'  YP  YP  YP
-}


initWith : input -> AdvancedControl input state delta output -> AdvancedControl input state delta output
initWith input (Control control) =
    let
        initialiser i =
            { i | init = i.initialise input }
    in
    Control (control >> initialiser)



{-
   d888888b d8b   db d888888b
     `88'   888o  88 `~~88~~'
      88    88V8o 88    88
      88    88 V8o88    88
     .88.   88  V888    88
   Y888888P VP   V8P    YP
-}


int : Control String String Int
int =
    makeControl
        { empty = ""
        , initialise = String.fromInt
        , update = \delta _ -> delta
        , view = textControlView "number"
        , parse =
            \state ->
                case String.toInt state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "must be a whole number" ]
        }
        |> debounce 500



{-
   d88888b db       .d88b.   .d8b.  d888888b
   88'     88      .8P  Y8. d8' `8b `~~88~~'
   88ooo   88      88    88 88ooo88    88
   88~~~   88      88    88 88~~~88    88
   88      88booo. `8b  d8' 88   88    88
   YP      Y88888P  `Y88P'  YP   YP    YP

-}


float : Control String String Float
float =
    makeControl
        { empty = ""
        , initialise = String.fromFloat
        , update = \delta _ -> delta
        , view = textControlView "number"
        , parse =
            \state ->
                case String.toFloat state of
                    Just i ->
                        Ok i

                    Nothing ->
                        Err [ "must be a whole number" ]
        }
        |> debounce 500



{-
   .d8888. d888888b d8888b. d888888b d8b   db  d888b
   88'  YP `~~88~~' 88  `8D   `88'   888o  88 88' Y8b
   `8bo.      88    88oobY'    88    88V8o 88 88
     `Y8b.    88    88`8b      88    88 V8o88 88  ooo
   db   8D    88    88 `88.   .88.   88  V888 88. ~8~
   `8888Y'    YP    88   YD Y888888P VP   V8P  Y888P
-}


string : Control String String String
string =
    makeControl
        { empty = ""
        , initialise = identity
        , update = \delta _ -> delta
        , view = textControlView "text"
        , parse = Ok
        }
        |> debounce 500



{-
   d88888b d8b   db db    db .88b  d88.
   88'     888o  88 88    88 88'YbdP`88
   88ooooo 88V8o 88 88    88 88  88  88
   88~~~~~ 88 V8o88 88    88 88  88  88
   88.     88  V888 88b  d88 88  88  88
   Y88888P VP   V8P ~Y8888P' YP  YP  YP
-}


enum :
    ( String, enum )
    -> ( String, enum )
    -> List ( String, enum )
    -> Control enum enum enum
enum first second rest =
    makeControl
        { empty = Tuple.second first
        , initialise = identity
        , update = \delta _ -> delta
        , view = enumView (first :: second :: rest)
        , parse = Ok
        }



{-
   d8888b.  .d88b.   .d88b.  db
   88  `8D .8P  Y8. .8P  Y8. 88
   88oooY' 88    88 88    88 88
   88~~~b. 88    88 88    88 88
   88   8D `8b  d8' `8b  d8' 88booo.
   Y8888P'  `Y88P'   `Y88P'  Y88888P
-}


bool : String -> String -> Control Bool Bool Bool
bool trueId falseId =
    enum ( trueId, True ) ( falseId, False ) []



{-
   db   d8b   db d8888b.  .d8b.  d8888b. d8888b. d88888b d8888b.
   88   I8I   88 88  `8D d8' `8b 88  `8D 88  `8D 88'     88  `8D
   88   I8I   88 88oobY' 88ooo88 88oodD' 88oodD' 88ooooo 88oobY'
   Y8   I8I   88 88`8b   88~~~88 88~~~   88~~~   88~~~~~ 88`8b
   `8b d8'8b d8' 88 `88. 88   88 88      88      88.     88 `88.
    `8b8' `8d8'  88   YD YP   YP 88      88      Y88888P 88   YD
-}


wrapper :
    (output -> wrapped)
    -> (wrapped -> output)
    -> Control state delta output
    ->
        Control
            ( State state, End )
            ( Delta delta, End )
            wrapped
wrapper wrap unwrap control =
    record wrap
        |> field "Wrapper" unwrap control
        |> end



{-
   .88b  d88.  .d8b.  db    db d8888b. d88888b
   88'YbdP`88 d8' `8b `8b  d8' 88  `8D 88'
   88  88  88 88ooo88  `8bd8'  88oooY' 88ooooo
   88  88  88 88~~~88    88    88~~~b. 88~~~~~
   88  88  88 88   88    88    88   8D 88.
   YP  YP  YP YP   YP    YP    Y8888P' Y88888P
-}


maybe :
    Control state delta output
    ->
        Control
            ( State (), ( State ( State state, End ), End ) )
            ( Delta (), ( Delta ( Delta delta, End ), End ) )
            (Maybe output)
maybe control =
    customType
        (\nothing just tag ->
            case tag of
                Nothing ->
                    nothing

                Just a ->
                    just a
        )
        |> tag0 "Nothing" Nothing
        |> tag1 "Just" Just control
        |> end


{-
   d888888b db    db d8888b. db      d88888b
   `~~88~~' 88    88 88  `8D 88      88'
      88    88    88 88oodD' 88      88ooooo
      88    88    88 88~~~   88      88~~~~~
      88    88b  d88 88      88booo. 88.
      YP    ~Y8888P' 88      Y88888P Y88888P
-}


tuple :
    String
    -> Control state1 delta1 output1
    -> String
    -> Control state2 delta2 output2
    -> Control ( State state1, ( State state2, End ) ) ( Delta delta1, ( Delta delta2, End ) ) ( output1, output2 )
tuple fstLabel fst sndLabel snd =
    record Tuple.pair
        |> field fstLabel Tuple.first fst
        |> field sndLabel Tuple.second snd
        |> end



{-
   db      d888888b .d8888. d888888b
   88        `88'   88'  YP `~~88~~'
   88         88    `8bo.      88
   88         88      `Y8b.    88
   88booo.   .88.   db   8D    88
   Y88888P Y888888P `8888Y'    YP
-}


type ListDelta delta
    = InsertItem Int
    | DeleteItem Int
    | ChangeItem Int (Delta delta)


list : Control state delta output -> Control (List (State state)) (ListDelta delta) (List output)
list (Control ctrl) =
    Control
        (\path ->
            let
                update =
                    wrapUpdate listUpdate

                listUpdate delta state =
                    case delta of
                        InsertItem idx ->
                            let
                                itemControl =
                                    ctrl path

                                before =
                                    List.take idx state

                                after =
                                    List.drop idx state
                            in
                            ( before ++ itemControl.init :: after, Cmd.none )

                        ChangeItem idx itemDelta ->
                            let
                                ( newState, cmds ) =
                                    List.foldr
                                        (\( i, item ) ( items, cmds_ ) ->
                                            if i == idx then
                                                let
                                                    itemControl =
                                                        ctrl (Path.add (String.fromInt i) path)

                                                    ( newItem, newCmd ) =
                                                        itemControl.update itemDelta item
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

                parse =
                    \(State _ state) ->
                        List.foldr
                            (\( idx, item ) res ->
                                let
                                    identifyErrors e =
                                        List.map (\( _, feedback ) -> "item #" ++ String.fromInt idx ++ ": " ++ feedback) e

                                    itemControl =
                                        ctrl (Path.add (String.fromInt idx) path)
                                in
                                case res of
                                    Ok outputs ->
                                        case itemControl.parse item of
                                            Ok output ->
                                                Ok (output :: outputs)

                                            Err errs ->
                                                Err (identifyErrors errs)

                                    Err errs ->
                                        case itemControl.parse item of
                                            Ok _ ->
                                                Err errs

                                            Err newErrs ->
                                                Err (identifyErrors newErrs ++ errs)
                            )
                            (Ok [])
                            (List.indexedMap Tuple.pair state)
                            |> Result.mapError (List.map (\feedback -> ( Path.toString path, feedback )))
            in
            { path = path
            , index = 0
            , initialise =
                \input ->
                    List.indexedMap
                        (\idx item ->
                            let
                                itemControl =
                                    ctrl (Path.add (String.fromInt idx) path)
                            in
                            itemControl.initialise item
                        )
                        input
                        |> State { status = Intact_, selected = 0 }
            , init = State { status = Intact_, selected = 0 } []
            , delta = Skip
            , baseUpdate = update
            , update = update 0
            , childViews = \_ -> []
            , view = listView path ctrl
            , parse = parse
            , setAllIdle =
                \(State i s) ->
                    State { i | status = Idle_ }
                        (List.indexedMap
                            (\idx item ->
                                let
                                    itemControl =
                                        ctrl (Path.add (String.fromInt idx) path)
                                in
                                itemControl.setAllIdle item
                            )
                            s
                        )
            , emitFlags = \_ -> []
            , receiveFlags = \_ -> []
            , receiverCount = []
            , collectDebouncingReceivers = \_ -> []
            }
        )



{-
   d8888b. d88888b  .o88b.  .d88b.  d8888b. d8888b.
   88  `8D 88'     d8P  Y8 .8P  Y8. 88  `8D 88  `8D
   88oobY' 88ooooo 8P      88    88 88oobY' 88   88
   88`8b   88~~~~~ 8b      88    88 88`8b   88   88
   88 `88. 88.     Y8b  d8 `8b  d8' 88 `88. 88  .8D
   88   YD Y88888P  `Y88P'  `Y88P'  88   YD Y8888D'

-}


end builder =
    case builder of
        Rec r ->
            endRecord r

        Cus c ->
            endCustomType c


type Builder r c
    = Rec r
    | Cus c


record toOutput =
    Rec
        { index = 0
        , labels = []
        , toOutput = toOutput
        , fields = \_ x -> x
        , states = \_ x -> x
        , updater = identity
        , viewer = identity
        , parser = identity
        , idleSetter = identity
        , initialiser = identity
        , before = identity
        , befores = identity
        , after = End
        , afters = End
        , makeSetters = identity
        , flagEmitter = identity
        , flagReceiver = identity
        , receiverCollector = identity
        }


field =
    internalField Open


hiddenField =
    internalField Hidden


readOnlyField =
    internalField ReadOnly


type Access
    = Open
    | ReadOnly
    | Hidden


internalField access label fromOutput (Control control) builder =
    case builder of
        Cus c ->
            Cus c

        Rec rec ->
            Rec
                { index = rec.index + 1
                , labels = rec.labels ++ [ label ]
                , toOutput = rec.toOutput
                , fields =
                    \path ->
                        rec.fields path
                            << Tuple.pair
                                { field = control (Path.add label path)
                                , fromOutput = fromOutput
                                , access = access
                                }
                , states =
                    \path ->
                        rec.states path
                            << Tuple.pair (control (Path.add label path) |> .init)
                , updater = rec.updater >> recordStateUpdater
                , viewer = rec.viewer >> recordStateViewer
                , parser = rec.parser >> recordStateValidator
                , idleSetter = rec.idleSetter >> recordStateIdleSetter
                , initialiser = rec.initialiser >> recordStateInitialiser
                , before = rec.before << Tuple.pair Skip
                , befores = rec.befores << Tuple.pair rec.before
                , after = ( Skip, rec.after )
                , afters = ( rec.after, rec.afters )
                , makeSetters = rec.makeSetters >> deltaSetterMaker
                , flagEmitter = rec.flagEmitter >> recordFlagEmitter
                , flagReceiver = rec.flagReceiver >> recordFlagReceiver
                , receiverCollector = rec.receiverCollector >> recordDebouncingReceiverCollector
                }


endRecord rec =
    Control
        (\path ->
            let
                fns =
                    rec.fields path End

                inits =
                    rec.states path End

                deltaSetters =
                    makeDeltaSetters rec.makeSetters rec.befores rec.afters

                update delta (State s state) =
                    case delta of
                        Skip ->
                            ( State s state, Cmd.none )

                        ChangeState deltas ->
                            let
                                ( newState, cmd ) =
                                    updateRecordStates rec.updater fns deltaSetters deltas state
                            in
                            ( State s newState
                            , Cmd.map ChangeState cmd
                            )

                        _ ->
                            ( State s state, Cmd.none )

                childViews config =
                    viewRecordStates rec.viewer config.flags fns deltaSetters config.state

                view childViews_ config =
                    let
                        fieldViews =
                            childViews_ config

                        idViews =
                            List.map (\i -> H.h4 [ HA.style "color" "maroon" ] [ H.text i ]) rec.labels

                        combinedViews =
                            List.map2
                                (\idView fieldView ->
                                    if fieldView == H.text "" then
                                        H.text ""
                                        --this is a bit of a hack!

                                    else
                                        H.div []
                                            [ idView
                                            , fieldView
                                            ]
                                )
                                idViews
                                fieldViews
                    in
                    H.div []
                        [ if List.length combinedViews > 1 then
                            borderedDiv combinedViews

                          else
                            H.div [] fieldViews
                        ]

                parse (State _ state) =
                    validateRecordStates rec.parser rec.toOutput fns state

                setAllIdle (State i state) =
                    State { i | status = Idle_ } (setAllRecordStatesToIdle rec.idleSetter fns state)

                emitFlags (State _ state) =
                    emitFlagsForRecord rec.flagEmitter fns state
            in
            { path = path
            , index = 0
            , init = State { status = Intact_, selected = 0 } inits
            , delta = Skip
            , initialise = \output -> initialiseRecordStates rec.initialiser output fns
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view childViews config
            , parse = parse
            , setAllIdle = setAllIdle
            , emitFlags = emitFlags
            , receiveFlags = \flags -> receiveFlagsForRecord rec.flagReceiver flags fns
            , receiverCount = []
            , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForRecord rec.receiverCollector fns states
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


collectDebouncingReceiversForRecord receiverCollector_ fns states =
    receiverCollector_ (\receivers End End -> receivers) [] fns states


recordDebouncingReceiverCollector next receivers ( fns, restFns ) ( state, restStates ) =
    next (receivers ++ fns.field.collectDebouncingReceivers state) restFns restStates


receiveFlagsForRecord flagReceiver_ flags fns =
    flagReceiver_ (\_ errors End -> errors) flags [] fns


recordFlagReceiver next flags errors ( fns, restFns ) =
    next flags (errors ++ fns.field.receiveFlags flags) restFns


emitFlagsForRecord flagEmitter_ fns states =
    flagEmitter_ (\flags End End -> flags) [] fns states


recordFlagEmitter next flags ( fns, restFns ) ( state, restStates ) =
    let
        newFlags =
            fns.field.emitFlags state
    in
    next (flags ++ newFlags) restFns restStates


makeDeltaSetters makeSetters_ befores afters =
    makeSetters_ (\End End -> End) (befores End) afters


deltaSetterMaker next ( before, befores ) ( after, afters ) =
    ( \value -> before ( value, after )
    , next befores afters
    )


initialiseRecordStates initialiser output fns =
    initialiser (\_ End -> End) output fns
        |> State { status = Intact_, selected = 0 }


recordStateInitialiser next output ( fns, restFns ) =
    ( fns.field.initialise (fns.fromOutput output)
    , next output restFns
    )


validateRecordStates parser toOutput fns states =
    parser (\output End End -> output) (Ok toOutput) fns states


recordStateValidator next toOutputResult ( fns, restFns ) ( state, restStates ) =
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


setAllRecordStatesToIdle idleSetter_ fns states =
    idleSetter_ (\End End -> End) fns states


recordStateIdleSetter next ( fns, restFns ) ( state, restStates ) =
    ( fns.field.setAllIdle state
    , next restFns restStates
    )


viewRecordStates viewer flags fns setters states =
    viewer (\views _ End End End -> views) [] flags fns setters states
        |> List.reverse


recordStateViewer next views flags ( fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    let
        view =
            fns.field.view
                { state = state
                , status = getStatus fns.field.parse fns.field.receiveFlags flags (State internalState state)
                , label = Path.last fns.field.path
                , flags = flags
                , selected = internalState.selected
                }
                |> H.map (\delta -> ChangeState (setter delta))
    in
    next
        ((case fns.access of
            Open ->
                view

            Hidden ->
                H.text ""

            ReadOnly ->
                H.fieldset
                    [ HA.disabled True
                    , HA.style "border" "none"
                    , HA.style "padding" "0px"
                    , HA.style "margin" "0px"
                    ]
                    [ view ]
         )
            :: views
        )
        flags
        restFns
        restSetters
        restStates


getStatus parse receiveFlags flags ((State internalState _) as state) =
    case internalState.status of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                parsedErrors =
                    case parse state of
                        Ok _ ->
                            []

                        Err errs ->
                            errs

                flaggedErrors =
                    flags
                        |> receiveFlags
            in
            Idle (List.map Err (parsedErrors ++ flaggedErrors))


updateRecordStates updater fields setters deltas states =
    let
        { newStates, newCmds } =
            updater
                (\output End End End End -> output)
                { newStates = identity, newCmds = [] }
                fields
                setters
                deltas
                states
    in
    ( newStates End, Cmd.batch newCmds )


recordStateUpdater next { newStates, newCmds } ( fns, restFns ) ( setter, restSetters ) ( delta, restDeltas ) ( state, restStates ) =
    let
        ( newState, newCmd ) =
            fns.field.update delta state

        cmd2 =
            newCmd
                |> Cmd.map setter
    in
    next
        { newStates = newStates << Tuple.pair newState
        , newCmds = cmd2 :: newCmds
        }
        restFns
        restSetters
        restDeltas
        restStates



{-
    .o88b. db    db .d8888. d888888b  .d88b.  .88b  d88.      d888888b db    db d8888b. d88888b
   d8P  Y8 88    88 88'  YP `~~88~~' .8P  Y8. 88'YbdP`88      `~~88~~' `8b  d8' 88  `8D 88'
   8P      88    88 `8bo.      88    88    88 88  88  88         88     `8bd8'  88oodD' 88ooooo
   8b      88    88   `Y8b.    88    88    88 88  88  88         88       88    88~~~   88~~~~~
   Y8b  d8 88b  d88 db   8D    88    `8b  d8' 88  88  88         88       88    88      88.
    `Y88P' ~Y8888P' `8888Y'    YP     `Y88P'  YP  YP  YP         YP       YP    88      Y88888P
-}


customType destructor =
    Cus
        { index = 0
        , labels = []
        , fns = \_ x -> x
        , states = \_ x -> x
        , updater = identity
        , viewer = identity
        , parser = identity
        , idleSetter = identity
        , deltaBefore = identity
        , deltaBefores = identity
        , deltaAfter = End
        , deltaAfters = End
        , makeDeltaSetters = identity
        , stateBefore = identity
        , stateBefores = identity
        , toArgStates = identity
        , stateAfter = End
        , stateAfters = End
        , makeStateSetters = identity
        , stateInserter = identity
        , applyInputs = identity
        , flagEmitter = identity
        , flagReceiver = identity
        , receiverCollector = identity
        , destructor = destructor
        }


tagHelper label (Control control) toArgState builder =
    case builder of
        Rec r ->
            Rec r

        Cus rec ->
            Cus
                { index = rec.index + 1
                , labels = label :: rec.labels
                , fns =
                    \path ->
                        let
                            control_ =
                                control (Path.add label path)
                        in
                        rec.fns path
                            << Tuple.pair
                                { field = { control_ | index = rec.index } }
                , states =
                    \path ->
                        rec.states path
                            << Tuple.pair (control (Path.add label path) |> .init)
                , updater = rec.updater >> recordStateUpdater
                , viewer = rec.viewer >> selectedTagViewer
                , parser = rec.parser >> selectedTagParser
                , idleSetter = rec.idleSetter >> selectedTagIdleSetter
                , deltaBefore = rec.deltaBefore << Tuple.pair Skip
                , deltaBefores = rec.deltaBefores << Tuple.pair rec.deltaBefore
                , deltaAfter = ( Skip, rec.deltaAfter )
                , deltaAfters = ( rec.deltaAfter, rec.deltaAfters )
                , makeDeltaSetters = rec.makeDeltaSetters >> deltaSetterMaker
                , stateBefore = rec.stateBefore << Tuple.pair Nothing
                , stateBefores = rec.stateBefores << Tuple.pair rec.stateBefore
                , toArgStates = rec.toArgStates << Tuple.pair toArgState
                , stateAfter = ( Nothing, rec.stateAfter )
                , stateAfters = ( rec.stateAfter, rec.stateAfters )
                , makeStateSetters = rec.makeStateSetters >> stateSetterMaker
                , stateInserter = rec.stateInserter >> argStateIntoTagStateInserter
                , applyInputs = rec.applyInputs >> stateSetterToInitialiserApplier
                , flagEmitter = rec.flagEmitter >> customTypeFlagEmitter
                , flagReceiver = rec.flagReceiver >> customTypeFlagReceiver
                , receiverCollector = rec.receiverCollector >> recordDebouncingReceiverCollector
                , destructor = rec.destructor
                }


tag0 label tag =
    tagHelper
        label
        (null tag)
        (\insertArgStateIntoTagStates ->
            insertArgStateIntoTagStates tag
        )


null : tag -> Control () () tag
null tag =
    makeControl
        { empty = ()
        , initialise = \_ -> ()
        , update = \() () -> ()
        , view = \_ -> H.text ""
        , parse = \() -> Ok tag
        }


tag1 label tag control =
    tagHelper
        label
        (record tag
            |> field "Tag" Tuple.first control
            |> end
        )
        (\insertArgStateIntoTagStates arg1 ->
            insertArgStateIntoTagStates ( arg1, End )
        )


tag2 label tag ( label1, control1 ) ( label2, control2 ) =
    tagHelper
        label
        (record tag
            |> field label1 Tuple.first control1
            |> field label2 (Tuple.second >> Tuple.first) control2
            |> end
        )
        (\insertArgStateIntoTagStates arg1 arg2 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, End ) )
        )


endCustomType rec =
    Control
        (\path ->
            let
                fns =
                    rec.fns path End

                inits =
                    rec.states path End

                deltaSetters =
                    makeDeltaSetters rec.makeDeltaSetters rec.deltaBefores rec.deltaAfters

                stateSetters =
                    makeStateSetters rec.makeStateSetters rec.stateInserter inits fns rec.toArgStates rec.stateBefores rec.stateAfters

                labels =
                    List.reverse rec.labels

                update =
                    \delta (State i state) ->
                        case delta of
                            Skip ->
                                ( State i state, Cmd.none )

                            TagSelected idx ->
                                ( State { i | selected = idx } state, Cmd.none )

                            ChangeState tagDelta ->
                                let
                                    ( newTagStates, cmd ) =
                                        updateRecordStates rec.updater fns deltaSetters tagDelta state
                                in
                                ( State i newTagStates
                                , Cmd.map ChangeState cmd
                                )

                            _ ->
                                ( State i state, Cmd.none )

                childViews config =
                    [ viewSelectedTagState rec.viewer config.flags config.selected fns deltaSetters config.state ]

                view config =
                    let
                        options =
                            List.indexedMap Tuple.pair labels

                        childViews_ =
                            childViews config
                    in
                    customTypeView config.label options config.selected childViews_

                parse =
                    \(State i state) -> validateSelectedTagState rec.parser i.selected fns state

                setAllIdle =
                    \(State i state) -> State { i | status = Idle_ } (setSelectedTagStateIdle rec.idleSetter i.selected fns state)

                emitFlags (State i state) =
                    emitFlagsForCustomType rec.flagEmitter i.selected fns state
            in
            { path = path
            , index = 0
            , init = State { status = Intact_, selected = 0 } inits
            , delta = Skip
            , initialise = applyStateSettersToInitialiser rec.applyInputs rec.destructor stateSetters
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view config
            , parse = parse
            , setAllIdle = setAllIdle
            , emitFlags = emitFlags
            , receiveFlags = \flags -> receiveFlagsForCustomType rec.flagReceiver flags fns
            , receiverCount = []
            , collectDebouncingReceivers = \(State _ states) -> collectDebouncingReceiversForRecord rec.receiverCollector fns states
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


receiveFlagsForCustomType =
    receiveFlagsForRecord


customTypeFlagReceiver =
    recordFlagReceiver


emitFlagsForCustomType flagEmitter_ selectedTag fns tagStates =
    flagEmitter_ (\flags _ End End -> flags) [] selectedTag fns tagStates


customTypeFlagEmitter next flags selectedTag ( fns, restFns ) ( tagState, restTagStates ) =
    let
        newFlags =
            if fns.field.index == selectedTag then
                fns.field.emitFlags tagState

            else
                []
    in
    next (flags ++ newFlags) selectedTag restFns restTagStates


applyStateSettersToInitialiser stateSetterToInitialiserApplier_ initialiser stateSetters tag =
    stateSetterToInitialiserApplier_
        (\appliedInitialiser End -> appliedInitialiser)
        initialiser
        stateSetters
        tag


stateSetterToInitialiserApplier next initialiser ( stateSetter, stateSetters ) =
    next (initialiser stateSetter) stateSetters


makeStateSetters makeSetters_ argStateIntoTagStateInserter_ inits fns toArgStates befores afters =
    makeSetters_ (\_ _ End End End End -> End) argStateIntoTagStateInserter_ inits fns (toArgStates End) (befores End) afters


stateSetterMaker next argStateIntoTagStateInserter_ inits ( fns, restFns ) ( toArgState, toArgStates ) ( before, befores ) ( after, afters ) =
    ( toArgState
        (\argState ->
            let
                maybes =
                    before ( Just (fns.field.initialise argState), after )
            in
            insertArgStateIntoTagState argStateIntoTagStateInserter_ maybes inits
        )
    , next argStateIntoTagStateInserter_ inits restFns toArgStates befores afters
    )


insertArgStateIntoTagState stateInserter_ maybeArgStates inits =
    stateInserter_
        (\_ selectedTag tagStates End End -> State { status = Intact_, selected = selectedTag } (tagStates End))
        0
        0
        identity
        maybeArgStates
        inits


argStateIntoTagStateInserter next thisTagIndex currentlySelectedTagIndex tagStates ( maybeArgState, maybeArgStates ) ( init, inits ) =
    let
        ( selectedTag, tagArgState ) =
            case maybeArgState of
                Just state ->
                    ( thisTagIndex, state )

                Nothing ->
                    ( currentlySelectedTagIndex, init )
    in
    next (thisTagIndex + 1) selectedTag (tagStates << Tuple.pair tagArgState) maybeArgStates inits


setSelectedTagStateIdle idleSetter_ selectedTag fns tagStates =
    idleSetter_
        (\_ End End -> End)
        selectedTag
        fns
        tagStates


selectedTagIdleSetter next selectedTag ( fns, restFns ) ( state, restStates ) =
    ( if fns.field.index == selectedTag then
        fns.field.setAllIdle state

      else
        state
    , next selectedTag restFns restStates
    )


validateSelectedTagState parser selectedTag fns states =
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


viewSelectedTagState viewer flags selectedTag fns setters states =
    viewer (\maybeView _ _ End End End -> maybeView) Nothing flags selectedTag fns setters states
        |> Maybe.map (H.map ChangeState)
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next maybeView flags selectedTag ( fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    next
        (if fns.field.index == selectedTag then
            Just
                (fns.field.view
                    { state = state
                    , status = Intact
                    , label = Path.toString fns.field.path
                    , flags = flags
                    , selected = internalState.selected
                    }
                    |> H.map setter
                )

         else
            maybeView
        )
        flags
        selectedTag
        restFns
        restSetters
        restStates



{-
   db    db d888888b d88888b db   d8b   db .d8888.
   88    88   `88'   88'     88   I8I   88 88'  YP
   Y8    8P    88    88ooooo 88   I8I   88 `8bo.
   `8b  d8'    88    88~~~~~ Y8   I8I   88   `Y8b.
    `8bd8'    .88.   88.     `8b d8'8b d8' db   8D
      YP    Y888888P Y88888P  `8b8' `8d8'  `8888Y'
-}


customTypeView :
    String
    -> List ( Int, String )
    -> Int
    -> List (Html (Delta variants))
    -> Html (Delta variants)
customTypeView label options selectedTag selectedTagView =
    H.div []
        (if List.length options > 1 then
            [ H.div []
                [ radioView
                    { options = options
                    , label = label
                    , selectedOption = selectedTag
                    , toMsg = TagSelected
                    , columns = 3
                    }
                , H.div [] selectedTagView
                ]
            ]

         else
            selectedTagView
        )


listView path ctrl config =
    H.div []
        [ if List.isEmpty config.state then
            H.button [ HE.onClick (InsertItem 0) ] [ H.text "➕ Add an item" ]

          else
            H.div []
                (List.indexedMap
                    (\idx (State internalState state) ->
                        let
                            control =
                                ctrl (Path.add (String.fromInt idx) path)
                        in
                        H.div [ HA.style "margin-top" "10px" ]
                            [ H.summary
                                [ HA.style "margin-bottom" "10px" ]
                                [ H.text ("#" ++ String.fromInt (idx + 1))
                                , H.button
                                    [ HA.style "margin-left" "10px"
                                    , HE.onClick (DeleteItem idx)
                                    ]
                                    [ H.text "❌" ]
                                , H.button
                                    [ HA.style "margin-left" "10px"
                                    , HE.onClick (InsertItem (idx + 1))
                                    ]
                                    [ H.text "➕" ]
                                ]
                            , control.view
                                { state = state
                                , status = getStatus control.parse control.receiveFlags config.flags (State internalState state)
                                , label = config.label ++ "-item#" ++ String.fromInt (idx + 1)
                                , flags = config.flags
                                , selected = config.selected
                                }
                                |> H.map (ChangeItem idx)
                            ]
                    )
                    config.state
                )
        ]
        |> H.map ChangeState


textControlView : String -> ViewConfig String -> Html String
textControlView type_ config =
    H.div
        []
        [ H.input
            [ HE.onInput identity
            , HA.type_ type_
            , HA.id config.label
            , HA.value config.state
            , HA.style "background-color"
                (case config.status of
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
            [ H.text config.state ]
        , statusView config.status
        ]


statusView : Status -> Html msg
statusView status =
    case status of
        Idle [] ->
            H.div
                [ HA.style "margin-top" "10px"
                , HA.style "margin-bottom" "10px"
                ]
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


enumView : List ( String, enum ) -> ViewConfig enum -> Html enum
enumView tags config =
    radioView
        { options = List.map (\( a, b ) -> ( b, a )) tags
        , label = config.label
        , selectedOption = config.state
        , toMsg = identity
        , columns = 3
        }


radioView :
    { options : List ( state, String )
    , label : String
    , selectedOption : state
    , toMsg : state -> msg
    , columns : Int
    }
    -> Html msg
radioView { options, label, selectedOption, toMsg, columns } =
    H.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" (String.repeat columns "1fr ")
        ]
        (List.map
            (\( option, optionLabel ) ->
                H.div
                    [ HA.style "margin-right" "15px"
                    , HA.style "margin-bottom" "10px"
                    ]
                    [ H.input
                        [ HA.type_ "radio"
                        , HA.id (label ++ "-" ++ optionLabel)
                        , HA.name label
                        , HA.value optionLabel
                        , HA.checked (selectedOption == option)
                        , onChecked (toMsg option)
                        ]
                        []
                    , H.label
                        [ HA.for (label ++ "-" ++ optionLabel)
                        , HA.style "margin-left" "4px"
                        ]
                        [ H.text optionLabel ]
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
