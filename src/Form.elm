module Form exposing
    ( Control
    , CustomTypeDelta
    , CustomTypeState
    , Delta
    , End
    , Form
    , ListDelta
    , ListState
    , MaybeDelta
    , MaybeState
    , State
    , TupleDelta
    , TupleState
    , TypeCheck
    , WrapperDelta
    , WrapperState
    , bool
    , checkDeltaType
    , checkStateType
    , customType
    , datetime
    , debounce
    , endCustomType
    , endRecord
    , enum
    , failIf
    , failOnFlag
    , field
    , flagIf
    , hiddenField
    , initFrom
    , int
    , layout
    , list
    , makeControl
    , maybe
    , noteIf
    , readOnlyField
    , record
    , string
    , tag0
    , tag1
    , tag2
    , toForm
    , tuple
    , wrapper
    )

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Iso8601
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
    InternalControl output state delta output


type InternalControl input state delta output
    = Control
        (String
         ->
            { label : String
            , index : Int
            , init : State state
            , delta : Delta delta
            , initialise : input -> state
            , baseUpdate : Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , childViews : ViewConfig state -> List (Html (Delta delta))
            , view : ViewConfig state -> Html (Delta delta)
            , baseValidate : State state -> Result (List ( String, String )) output
            , validate : State state -> Result (List ( String, String )) output
            , notify : State state -> List ( String, String )
            , setAllIdle : State state -> State state
            , emitFlags : State state -> List String
            , receiveFlags : List String -> List String
            }
        )


type alias ViewConfig state =
    { label : String
    , state : state
    , status : Status
    , flags : List String
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


type InternalState
    = Intact_
    | DebouncingSince Time.Posix
    | Idle_


type alias States1 a =
    ( State a, End )


type alias States2 a b =
    ( State a, States1 b )



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


type alias Deltas1 a =
    ( Delta a, End )


type alias Deltas2 a b =
    ( Delta a, Deltas1 b )



{-
   d88888b  .d88b.  d8888b. .88b  d88. .d8888.
   88'     .8P  Y8. 88  `8D 88'YbdP`88 88'  YP
   88ooo   88    88 88oobY' 88  88  88 `8bo.
   88~~~   88    88 88`8b   88  88  88   `Y8b.
   88      `8b  d8' 88 `88. 88  88  88 db   8D
   YP       `Y88P'  88   YD YP  YP  YP `8888Y'
-}


toForm : String -> (Delta delta -> msg) -> Control state delta output -> Form state delta output msg
toForm label toMsg (Control control) =
    let
        { init, update, view, validate, notify, setAllIdle, emitFlags, receiveFlags } =
            control label
    in
    { init = init
    , initWith =
        \output ->
            let
                (Control initialisedControl) =
                    Control control
                        |> initFrom output
            in
            initialisedControl label
                |> .init
    , update =
        \msg state ->
            update msg state
                |> Tuple.mapSecond (Cmd.map toMsg)
    , view =
        \((State internalState state) as s) ->
            let
                flags =
                    emitFlags s |> Debug.log "flags"
            in
            H.div []
                [ H.h1 [] [ H.text label ]
                , H.div []
                    [ view
                        { state = state
                        , status =
                            -- getStatus validate notify (State internalState state)
                            getStatus2 flags receiveFlags (State internalState state)
                        , label = label
                        , flags = flags
                        }
                        |> H.map toMsg
                    ]
                ]
    , submit =
        \state ->
            validate state
                |> Result.mapError (\errors -> { errors = errors, state = setAllIdle state })
    }


type TypeCheck type_
    = TypeCheck


checkDeltaType : Control state delta output -> TypeCheck delta
checkDeltaType _ =
    TypeCheck


checkStateType : Control state delta output -> TypeCheck state
checkStateType _ =
    TypeCheck



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
        (\label ->
            let
                preUpdate =
                    wrapUpdate (\d s -> config.update d s |> (\ns -> ( ns, Cmd.none )))

                validate =
                    \(State _ state) ->
                        config.parse state
                            |> Result.mapError (List.map (Tuple.pair label))
            in
            { label = label
            , index = 0
            , init = State Intact_ config.empty
            , delta = Skip
            , initialise = config.initialise
            , baseUpdate = preUpdate
            , update = preUpdate 0
            , childViews = \_ -> []
            , view = config.view >> H.map ChangeState
            , baseValidate = validate
            , validate = validate
            , setAllIdle = \(State _ s) -> State Idle_ s
            , notify = \_ -> []
            , emitFlags = \_ -> []
            , receiveFlags = \_ -> []
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


flagIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
flagIf check flag (Control control) =
    let
        emitFlags_ ctrl =
            { ctrl
                | emitFlags =
                    \state ->
                        let
                            oldFlags =
                                ctrl.emitFlags state

                            newFlags =
                                case ctrl.baseValidate state of
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
    in
    Control (control >> emitFlags_)


failOnFlag : String -> String -> Control state delta output -> Control state delta output
failOnFlag flag message (Control control) =
    let
        flagReceiver ctrl =
            { ctrl
                | receiveFlags =
                    \flags ->
                        let
                            oldReceiver =
                                ctrl.receiveFlags flags

                            newReceiver =
                                if List.member flag flags then
                                    [ message ]

                                else
                                    []
                        in
                        List.Extra.unique (oldReceiver ++ newReceiver)
            }
    in
    Control (control >> flagReceiver)



{-
   d88888b  .d8b.  d888888b db           d888888b d88888b
   88'     d8' `8b   `88'   88             `88'   88'
   88ooo   88ooo88    88    88              88    88ooo
   88~~~   88~~~88    88    88              88    88~~~
   88      88   88   .88.   88booo.        .88.   88
   YP      YP   YP Y888888P Y88888P      Y888888P YP
-}


failIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
failIf check feedback (Control control) =
    let
        validate ctrl =
            let
                newValidator =
                    \state ->
                        case ctrl.baseValidate state of
                            Ok output ->
                                if check output then
                                    Err [ ( ctrl.label, feedback ) ]

                                else
                                    Ok output

                            Err errs ->
                                Err errs

                existingValidator =
                    ctrl.validate
            in
            { ctrl
                | validate =
                    \state ->
                        case ( newValidator state, existingValidator state ) of
                            ( Ok _, Ok output ) ->
                                Ok output

                            ( Ok _, Err errs ) ->
                                Err errs

                            ( Err errs, Ok _ ) ->
                                Err errs

                            ( Err errs1, Err errs2 ) ->
                                Err (List.Extra.unique (errs1 ++ errs2))
            }
    in
    Control (control >> validate)



{-
   d8b   db  .d88b.  d888888b d88888b      d888888b d88888b
   888o  88 .8P  Y8. `~~88~~' 88'            `88'   88'
   88V8o 88 88    88    88    88ooooo         88    88ooo
   88 V8o88 88    88    88    88~~~~~         88    88~~~
   88  V888 `8b  d8'    88    88.            .88.   88
   VP   V8P  `Y88P'     YP    Y88888P      Y888888P YP
-}


noteIf : (output -> Bool) -> String -> Control state delta output -> Control state delta output
noteIf check feedback (Control control) =
    let
        notify ctrl =
            { ctrl
                | notify =
                    \state ->
                        let
                            existingNotes =
                                ctrl.notify state

                            newNotes =
                                case ctrl.baseValidate state of
                                    Ok output ->
                                        if check output then
                                            [ ( ctrl.label, feedback ) ]

                                        else
                                            []

                                    Err _ ->
                                        []
                        in
                        List.Extra.unique (existingNotes ++ newNotes)
            }
    in
    Control (control >> notify)



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


initFrom : input -> InternalControl input state delta output -> InternalControl input state delta output
initFrom input (Control control) =
    let
        initialiser i =
            { i | init = State Intact_ (i.initialise input) }
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
   d8888b.  .d8b.  d888888b d88888b d888888b d888888b .88b  d88. d88888b
   88  `8D d8' `8b `~~88~~' 88'     `~~88~~'   `88'   88'YbdP`88 88'
   88   88 88ooo88    88    88ooooo    88       88    88  88  88 88ooooo
   88   88 88~~~88    88    88~~~~~    88       88    88  88  88 88~~~~~
   88  .8D 88   88    88    88.        88      .88.   88  88  88 88.
   Y8888D' YP   YP    YP    Y88888P    YP    Y888888P YP  YP  YP Y88888P
-}


datetime : Control String String Time.Posix
datetime =
    makeControl
        { empty = ""
        , initialise = Iso8601.fromTime >> String.replace "Z" ""
        , update = \delta _ -> delta
        , view = textControlView "datetime-local"
        , parse = Iso8601.toTime >> Result.mapError (\_ -> [ "Not a valid datetime" ])
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


type alias WrapperState state =
    States1 state


type alias WrapperDelta delta =
    Deltas1 delta


wrapper :
    (output -> wrapped)
    -> (wrapped -> output)
    -> Control state delta output
    ->
        Control
            (WrapperState state)
            (WrapperDelta delta)
            wrapped
wrapper wrap unwrap control =
    Control
        (\label ->
            let
                (Control toWrapped) =
                    record wrap
                        |> field unwrap label control
                        |> endRecord
            in
            toWrapped label
        )



{-
   .88b  d88.  .d8b.  db    db d8888b. d88888b
   88'YbdP`88 d8' `8b `8b  d8' 88  `8D 88'
   88  88  88 88ooo88  `8bd8'  88oooY' 88ooooo
   88  88  88 88~~~88    88    88~~~b. 88~~~~~
   88  88  88 88   88    88    88   8D 88.
   YP  YP  YP YP   YP    YP    Y8888P' Y88888P
-}


type alias MaybeState state =
    CustomTypeState
        (States2
            ()
            (States1 state)
        )


type alias MaybeDelta delta =
    CustomTypeDelta
        (Deltas2
            ()
            (Deltas1 delta)
        )


maybe : Control state delta output -> Control (MaybeState state) (MaybeDelta delta) (Maybe output)
maybe control =
    Control
        (\label ->
            let
                (Control toWrapped) =
                    customType
                        |> tag0 "Nothing" Nothing
                        |> tag1 "Just" Just control
                        |> endCustomType
                            (\nothing just tag ->
                                case tag of
                                    Nothing ->
                                        nothing

                                    Just a ->
                                        just a
                            )
            in
            toWrapped label
        )



{-
   d888888b db    db d8888b. db      d88888b
   `~~88~~' 88    88 88  `8D 88      88'
      88    88    88 88oodD' 88      88ooooo
      88    88    88 88~~~   88      88~~~~~
      88    88b  d88 88      88booo. 88.
      YP    ~Y8888P' 88      Y88888P Y88888P
-}


type alias TupleState s1 s2 =
    States2 s1 s2


type alias TupleDelta d1 d2 =
    Deltas2 d1 d2


tuple :
    String
    -> Control state1 delta1 output1
    -> String
    -> Control state2 delta2 output2
    -> Control (TupleState state1 state2) (TupleDelta delta1 delta2) ( output1, output2 )
tuple fstLabel fst sndLabel snd =
    record Tuple.pair
        |> field Tuple.first fstLabel fst
        |> field Tuple.second sndLabel snd
        |> endRecord



{-
   db      d888888b .d8888. d888888b
   88        `88'   88'  YP `~~88~~'
   88         88    `8bo.      88
   88         88      `Y8b.    88
   88booo.   .88.   db   8D    88
   Y88888P Y888888P `8888Y'    YP
-}


type alias ListState state =
    List (State state)


type ListDelta delta
    = InsertItem Int
    | DeleteItem Int
    | ChangeItem Int (Delta delta)


list : Control state delta output -> Control (List (State state)) (ListDelta delta) (List output)
list (Control toControl) =
    let
        control =
            toControl ""
    in
    Control
        (\label ->
            let
                update =
                    wrapUpdate listUpdate

                listUpdate delta state =
                    case delta of
                        InsertItem idx ->
                            let
                                before =
                                    List.take idx state

                                after =
                                    List.drop idx state
                            in
                            ( before ++ control.init :: after, Cmd.none )

                        ChangeItem idx itemDelta ->
                            let
                                ( newState, cmds ) =
                                    List.foldr
                                        (\( i, item ) ( items, cmds_ ) ->
                                            if i == idx then
                                                let
                                                    ( newItem, newCmd ) =
                                                        control.update itemDelta item
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

                validate =
                    \(State _ state) ->
                        List.foldr
                            (\( idx, item ) res ->
                                let
                                    identifyErrors e =
                                        List.map (\( _, feedback ) -> "item #" ++ String.fromInt idx ++ ": " ++ feedback) e
                                in
                                case res of
                                    Ok outputs ->
                                        case control.validate item of
                                            Ok output ->
                                                Ok (output :: outputs)

                                            Err errs ->
                                                Err (identifyErrors errs)

                                    Err errs ->
                                        case control.validate item of
                                            Ok _ ->
                                                Err errs

                                            Err newErrs ->
                                                Err (identifyErrors newErrs ++ errs)
                            )
                            (Ok [])
                            (List.indexedMap Tuple.pair state)
                            |> Result.mapError (List.map (\feedback -> ( label, feedback )))
            in
            { label = label
            , index = 0
            , initialise = List.map (\item -> State Intact_ (control.initialise item))
            , init = State Intact_ []
            , delta = Skip
            , baseUpdate = update
            , update = update 0
            , childViews = \_ -> []
            , view = listView control.view control.validate control.notify
            , baseValidate = validate
            , validate = validate
            , setAllIdle = \(State _ s) -> State Idle_ (List.map control.setAllIdle s)
            , notify = \_ -> []
            , emitFlags = \_ -> []
            , receiveFlags = \_ -> []
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


record toOutput =
    { index = 0
    , labels = []
    , toOutput = toOutput
    , fields = identity
    , states = identity
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


internalField access fromOutput label (Control control) rec =
    let
        i =
            control label
    in
    { index = rec.index + 1
    , labels = rec.labels ++ [ label ]
    , toOutput = rec.toOutput
    , fields =
        rec.fields
            << Tuple.pair
                { field = { i | index = rec.index }
                , fromOutput = fromOutput
                , access = access
                }
    , states = rec.states << Tuple.pair i.init
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
    }


endRecord rec =
    let
        fns =
            rec.fields End

        inits =
            rec.states End

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
                , statusView config.status
                ]

        validate (State _ state) =
            validateRecordStates rec.parser rec.toOutput fns state

        setAllIdle (State _ state) =
            State Idle_ (setAllRecordStatesToIdle rec.idleSetter fns state)

        emitFlags (State _ state) =
            emitFlagsForRecord rec.flagEmitter fns state
    in
    Control
        (\label ->
            { label = label
            , index = 0
            , init = State Intact_ inits
            , delta = Skip
            , initialise = \output -> initialiseRecordStates rec.initialiser output fns inits
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view childViews config
            , baseValidate = validate
            , validate = validate
            , setAllIdle = setAllIdle
            , notify = \_ -> []
            , emitFlags = emitFlags
            , receiveFlags = \_ -> []
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


initialiseRecordStates initialiser output fns states =
    initialiser (\_ End End -> End) output fns states


recordStateInitialiser next output ( fns, restFns ) ( states, restStates ) =
    ( State Intact_ (fns.field.initialise (fns.fromOutput output))
    , next output restFns restStates
    )


validateRecordStates parser toOutput fns states =
    parser (\output End End -> output) (Ok toOutput) fns states


recordStateValidator next toOutputResult ( fns, restFns ) ( state, restStates ) =
    next
        (case ( toOutputResult, fns.field.validate state ) of
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
                , status =
                    -- getStatus fns.field.validate fns.field.notify (State internalState state)
                    getStatus2 flags fns.field.receiveFlags (State internalState state)
                , label = fns.field.label
                , flags = flags
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


getStatus :
    (State state -> Result (List ( String, String )) output)
    -> (State state -> List ( String, String ))
    -> State state
    -> Status
getStatus validator notifier ((State internalState _) as state) =
    case internalState of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                errors =
                    case validator state of
                        Ok _ ->
                            []

                        Err errs ->
                            errs

                notes =
                    notifier state
            in
            Idle (List.map Err errors ++ List.map Ok notes)


getStatus2 flags flagReceiver_ (State internalState _) =
    case internalState of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                errors =
                    flags
                        |> flagReceiver_
                        |> List.map (Tuple.pair "")
            in
            Idle (List.map Err errors)


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


type alias CustomTypeState variants =
    { tagStates : variants
    , selectedTag : Int
    }


type CustomTypeDelta variants
    = TagSelected Int
    | TagDeltaReceived variants


customType =
    { index = 0
    , labels = []
    , fns = identity
    , states = identity
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
    }


variant label (Control control) toArgState rec =
    let
        i =
            control label
    in
    { index = rec.index + 1
    , labels = label :: rec.labels
    , fns = rec.fns << Tuple.pair { field = { i | index = rec.index } }
    , states = rec.states << Tuple.pair i.init
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
    , flagEmitter = rec.flagEmitter >> recordFlagEmitter
    }


tag0 label tag =
    variant
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
    variant
        label
        (record tag
            |> field Tuple.first "" control
            |> endRecord
        )
        (\insertArgStateIntoTagStates arg1 ->
            insertArgStateIntoTagStates ( arg1, End )
        )


tag2 label tag ( label1, control1 ) ( label2, control2 ) =
    variant
        label
        (record tag
            |> field Tuple.first label1 control1
            |> field (Tuple.second >> Tuple.first) label2 control2
            |> endRecord
        )
        (\insertArgStateIntoTagStates arg1 arg2 ->
            insertArgStateIntoTagStates ( arg1, ( arg2, End ) )
        )


endCustomType initialiser rec =
    let
        fns =
            rec.fns End

        inits =
            rec.states End

        deltaSetters =
            makeDeltaSetters rec.makeDeltaSetters rec.deltaBefores rec.deltaAfters

        stateSetters =
            makeStateSetters rec.makeStateSetters rec.stateInserter inits fns rec.toArgStates rec.stateBefores rec.stateAfters

        labels =
            List.reverse rec.labels

        update =
            \delta (State s state) ->
                case delta of
                    Skip ->
                        ( State s state, Cmd.none )

                    ChangeState (TagSelected idx) ->
                        ( State s { state | selectedTag = idx }, Cmd.none )

                    ChangeState (TagDeltaReceived tagDelta) ->
                        let
                            ( newTagStates, cmd ) =
                                updateRecordStates rec.updater fns deltaSetters tagDelta state.tagStates
                        in
                        ( State s { state | tagStates = newTagStates }
                        , Cmd.map (ChangeState << TagDeltaReceived) cmd
                        )

                    _ ->
                        ( State s state, Cmd.none )

        childViews config =
            [ viewSelectedTagState rec.viewer config.flags config.state.selectedTag fns deltaSetters config.state.tagStates ]

        view config =
            let
                options =
                    List.indexedMap Tuple.pair labels

                childViews_ =
                    childViews config
            in
            customTypeView config.label options config.state.selectedTag childViews_

        validate =
            \(State _ state) -> validateSelectedTagState rec.parser state.selectedTag fns state.tagStates

        setAllIdle =
            \(State _ state) -> State Idle_ (setSelectedTagStateIdle rec.idleSetter state.selectedTag fns state.tagStates)

        emitFlags (State _ state) =
            emitFlagsForRecord rec.flagEmitter fns state.tagStates
    in
    Control
        (\label ->
            { label = label
            , index = 0
            , init = State Intact_ { tagStates = inits, selectedTag = 0 }
            , delta = Skip
            , initialise = applyStateSettersToInitialiser rec.applyInputs initialiser stateSetters
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view config
            , baseValidate = validate
            , validate = validate
            , setAllIdle = setAllIdle
            , notify = \_ -> []
            , emitFlags = emitFlags
            , receiveFlags = \_ -> []
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
        (\_ selectedTag tagStates End End -> { selectedTag = selectedTag, tagStates = tagStates End })
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
                    ( thisTagIndex, State Intact_ state )

                Nothing ->
                    ( currentlySelectedTagIndex, init )
    in
    next (thisTagIndex + 1) selectedTag (tagStates << Tuple.pair tagArgState) maybeArgStates inits


setSelectedTagStateIdle idleSetter_ selectedTag fns tagStates =
    { selectedTag = selectedTag
    , tagStates =
        idleSetter_
            (\_ End End -> End)
            selectedTag
            fns
            tagStates
    }


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
            fns.field.validate state

         else
            result
        )
        selectedTag
        restFns
        restStates


viewSelectedTagState viewer flags selectedTag fns setters states =
    viewer (\maybeView _ _ End End End -> maybeView) Nothing flags selectedTag fns setters states
        |> Maybe.map (H.map (ChangeState << TagDeltaReceived))
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next maybeView flags selectedTag ( fns, restFns ) ( setter, restSetters ) ( State s state, restStates ) =
    next
        (if fns.field.index == selectedTag then
            Just
                (fns.field.view
                    { state = state
                    , status = Intact
                    , label = fns.field.label
                    , flags = flags
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
    -> List (Html (Delta (CustomTypeDelta variants)))
    -> Html (Delta (CustomTypeDelta variants))
customTypeView label options selectedTag selectedTagView =
    H.div []
        (if List.length options > 1 then
            [ H.div []
                [ radioView
                    { options = options
                    , label = label
                    , selectedOption = selectedTag
                    , toMsg = ChangeState << TagSelected
                    , columns = 3
                    }
                , H.div [] selectedTagView
                ]
            ]

         else
            selectedTagView
        )


listView :
    (ViewConfig state -> Html (Delta delta))
    -> (State state -> Result (List ( String, String )) output)
    -> (State state -> List ( String, String ))
    -> ViewConfig (List (State state))
    -> Html (Delta (ListDelta delta))
listView controlView controlParse controlFeedback config =
    H.div []
        [ if List.isEmpty config.state then
            H.button [ HE.onClick (InsertItem 0) ] [ H.text "➕ Add an item" ]

          else
            H.div []
                (List.indexedMap
                    (\idx (State internalState state) ->
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
                            , controlView
                                { state = state
                                , status = getStatus controlParse controlFeedback (State internalState state)
                                , label = config.label ++ "-item#" ++ String.fromInt (idx + 1)
                                , flags = config.flags
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
