module Form exposing
    ( CustomTypeDelta
    , CustomTypeState
    , Delta
    , Deltas0
    , Deltas1
    , Deltas10
    , Deltas11
    , Deltas12
    , Deltas13
    , Deltas14
    , Deltas15
    , Deltas2
    , Deltas3
    , Deltas4
    , Deltas5
    , Deltas6
    , Deltas7
    , Deltas8
    , Deltas9
    , End
    , EnumDelta
    , EnumState
    , Form
    , Input
    , ListDelta
    , ListState
    , MaybeDelta
    , MaybeState
    , State
    , States0
    , States1
    , States10
    , States11
    , States12
    , States13
    , States14
    , States15
    , States2
    , States3
    , States4
    , States5
    , States6
    , States7
    , States8
    , States9
    , TupleDelta
    , TupleState
    , WrapperDelta
    , WrapperState
    , always
    , customType
    , debounce
    , endCustomType
    , endRecord
    , enum
    , failIf
    , field
    , i0
    , i1
    , i10
    , i11
    , i12
    , i13
    , i14
    , i15
    , i2
    , i3
    , i4
    , i5
    , i6
    , i7
    , i8
    , i9
    , int
    , list
    , makeInput
    , maybe
    , noteIf
    , record
    , string
    , tag0
    , tag1
    , tag2
    , tag3
    , tag4
    , tag5
    , toForm
    , tuple
    , wrapper
    )

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
            , init : State state
            , baseUpdate : Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , view : ViewConfig state -> Html (Delta delta)
            , baseValidate : State state -> Result (List ( String, String )) output
            , validate : State state -> Result (List ( String, String )) output
            , notify : State state -> List ( String, String )
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


type States0
    = States0


type alias States1 a =
    ( State a, End )


type alias States2 a b =
    ( State a, States1 b )


type alias States3 a b c =
    ( State a, States2 b c )


type alias States4 a b c d =
    ( State a, States3 b c d )


type alias States5 a b c d e =
    ( State a, States4 b c d e )


type alias States6 a b c d e f =
    ( State a, States5 b c d e f )


type alias States7 a b c d e f g =
    ( State a, States6 b c d e f g )


type alias States8 a b c d e f g h =
    ( State a, States7 b c d e f g h )


type alias States9 a b c d e f g h i =
    ( State a, States8 b c d e f g h i )


type alias States10 a b c d e f g h i j =
    ( State a, States9 b c d e f g h i j )


type alias States11 a b c d e f g h i j k =
    ( State a, States10 b c d e f g h i j k )


type alias States12 a b c d e f g h i j k l =
    ( State a, States11 b c d e f g h i j k l )


type alias States13 a b c d e f g h i j k l m =
    ( State a, States12 b c d e f g h i j k l m )


type alias States14 a b c d e f g h i j k l m n =
    ( State a, States13 b c d e f g h i j k l m n )


type alias States15 a b c d e f g h i j k l m n o =
    ( State a, States14 b c d e f g h i j k l m n o )



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


type Deltas0
    = Deltas0


type alias Deltas1 a =
    ( Delta a, End )


type alias Deltas2 a b =
    ( Delta a, Deltas1 b )


type alias Deltas3 a b c =
    ( Delta a, Deltas2 b c )


type alias Deltas4 a b c d =
    ( Delta a, Deltas3 b c d )


type alias Deltas5 a b c d e =
    ( Delta a, Deltas4 b c d e )


type alias Deltas6 a b c d e f =
    ( Delta a, Deltas5 b c d e f )


type alias Deltas7 a b c d e f g =
    ( Delta a, Deltas6 b c d e f g )


type alias Deltas8 a b c d e f g h =
    ( Delta a, Deltas7 b c d e f g h )


type alias Deltas9 a b c d e f g h i =
    ( Delta a, Deltas8 b c d e f g h i )


type alias Deltas10 a b c d e f g h i j =
    ( Delta a, Deltas9 b c d e f g h i j )


type alias Deltas11 a b c d e f g h i j k =
    ( Delta a, Deltas10 b c d e f g h i j k )


type alias Deltas12 a b c d e f g h i j k l =
    ( Delta a, Deltas11 b c d e f g h i j k l )


type alias Deltas13 a b c d e f g h i j k l m =
    ( Delta a, Deltas12 b c d e f g h i j k l m )


type alias Deltas14 a b c d e f g h i j k l m n =
    ( Delta a, Deltas13 b c d e f g h i j k l m n )


type alias Deltas15 a b c d e f g h i j k l m n o =
    ( Delta a, Deltas14 b c d e f g h i j k l m n o )



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
        { init, update, view, validate, notify } =
            input id
    in
    { init = init
    , update =
        \msg state ->
            update msg state
                |> Tuple.mapSecond (Cmd.map toMsg)
    , view =
        \(State internalState state) ->
            H.div []
                [ H.h1 [] [ H.text id ]
                , H.div []
                    [ view
                        { state = state
                        , status = statusFromInternalState validate notify (State internalState state)
                        , id = id
                        }
                        |> H.map toMsg
                    ]
                ]
    , submit = validate
    }



{-
   d888888b d8b   db d8888b. db    db d888888b .d8888.
     `88'   888o  88 88  `8D 88    88 `~~88~~' 88'  YP
      88    88V8o 88 88oodD' 88    88    88    `8bo.
      88    88 V8o88 88~~~   88    88    88      `Y8b.
     .88.   88  V888 88      88b  d88    88    db   8D
   Y888888P VP   V8P 88      ~Y8888P'    YP    `8888Y'
-}


makeInput : InputConfig state delta output -> Input state delta output
makeInput config =
    Input
        (\id ->
            let
                preUpdate =
                    wrappedUpdate (\d s -> config.update d s |> (\ns -> ( ns, Cmd.none )))

                validate =
                    \(State _ state) -> config.parse state |> Result.mapError (List.map (Tuple.pair id))
            in
            { id = id
            , index = 0
            , init = State Intact_ config.init
            , baseUpdate = preUpdate
            , update = preUpdate 0
            , view = config.view >> H.map ChangeState
            , baseValidate = validate
            , validate = validate
            , notify = \_ -> []
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



{-
   db    db  .d8b.  db      d888888b d8888b.  .d8b.  d888888b d888888b  .d88b.  d8b   db
   88    88 d8' `8b 88        `88'   88  `8D d8' `8b `~~88~~'   `88'   .8P  Y8. 888o  88
   Y8    8P 88ooo88 88         88    88   88 88ooo88    88       88    88    88 88V8o 88
   `8b  d8' 88~~~88 88         88    88   88 88~~~88    88       88    88    88 88 V8o88
    `8bd8'  88   88 88booo.   .88.   88  .8D 88   88    88      .88.   `8b  d8' 88  V888
      YP    YP   YP Y88888P Y888888P Y8888D' YP   YP    YP    Y888888P  `Y88P'  VP   V8P
-}


failIf : (output -> Bool) -> String -> Input state delta output -> Input state delta output
failIf check feedback (Input input) =
    let
        validate i =
            let
                newValidator =
                    \state ->
                        case i.baseValidate state of
                            Ok output ->
                                if check output then
                                    Err [ ( i.id, feedback ) ]

                                else
                                    Ok output

                            Err errs ->
                                Err errs

                existingValidator =
                    i.validate
            in
            { i
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
    Input (input >> validate)


noteIf : (output -> Bool) -> String -> Input state delta output -> Input state delta output
noteIf check feedback (Input input) =
    let
        show i =
            { i
                | notify =
                    \state ->
                        let
                            existingNotes =
                                i.notify state

                            newNotes =
                                case i.baseValidate state of
                                    Ok output ->
                                        if check output then
                                            [ ( i.id, feedback ) ]

                                        else
                                            []

                                    Err _ ->
                                        []
                        in
                        List.Extra.unique (existingNotes ++ newNotes)
            }
    in
    Input (input >> show)



{-
   d8888b. d88888b d8888b.  .d88b.  db    db d8b   db  .o88b. d888888b d8b   db  d888b
   88  `8D 88'     88  `8D .8P  Y8. 88    88 888o  88 d8P  Y8   `88'   888o  88 88' Y8b
   88   88 88ooooo 88oooY' 88    88 88    88 88V8o 88 8P         88    88V8o 88 88
   88   88 88~~~~~ 88~~~b. 88    88 88    88 88 V8o88 8b         88    88 V8o88 88  ooo
   88  .8D 88.     88   8D `8b  d8' 88b  d88 88  V888 Y8b  d8   .88.   88  V888 88. ~8~
   Y8888D' Y88888P Y8888P'  `Y88P'  ~Y8888P' VP   V8P  `Y88P' Y888888P VP   V8P  Y888P
-}


debounce : Float -> Input state delta output -> Input state delta output
debounce millis (Input input) =
    let
        debouncer i =
            { i | update = i.baseUpdate millis }
    in
    Input (input >> debouncer)



{-
   d888888b d8b   db d888888b
     `88'   888o  88 `~~88~~'
      88    88V8o 88    88
      88    88 V8o88    88
     .88.   88  V888    88
   Y888888P VP   V8P    YP
-}


int : Input String String Int
int =
    makeInput
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
        |> debounce 500



{-
   .d8888. d888888b d8888b. d888888b d8b   db  d888b
   88'  YP `~~88~~' 88  `8D   `88'   888o  88 88' Y8b
   `8bo.      88    88oobY'    88    88V8o 88 88
     `Y8b.    88    88`8b      88    88 V8o88 88  ooo
   db   8D    88    88 `88.   .88.   88  V888 88. ~8~
   `8888Y'    YP    88   YD Y888888P VP   V8P  Y888P
-}


string : Input String String String
string =
    makeInput
        { init = ""
        , update = \delta _ -> delta
        , view = stringView
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


type alias EnumState enum =
    enum


type alias EnumDelta enum =
    enum


enum :
    ( String, EnumState enum )
    -> List ( String, EnumState enum )
    -> Input (EnumState enum) (EnumDelta enum) enum
enum tag tags =
    makeInput
        { init = Tuple.second tag
        , update = \delta _ -> delta
        , view = enumView (tag :: tags)
        , parse = Ok
        }



{-
    .d8b.  db      db   d8b   db  .d8b.  db    db .d8888.
   d8' `8b 88      88   I8I   88 d8' `8b `8b  d8' 88'  YP
   88ooo88 88      88   I8I   88 88ooo88  `8bd8'  `8bo.
   88~~~88 88      Y8   I8I   88 88~~~88    88      `Y8b.
   88   88 88booo. `8b d8'8b d8' 88   88    88    db   8D
   YP   YP Y88888P  `8b8' `8d8'  YP   YP    YP    `8888Y'
-}


always : output -> Input States0 Deltas0 output
always output =
    makeInput
        { init = States0
        , update = \_ _ -> States0
        , view = \_ -> H.text ""
        , parse = \_ -> Ok output
        }



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


wrapper : (output -> wrapped) -> Input state delta output -> Input (WrapperState state) (WrapperDelta delta) wrapped
wrapper wrapping input =
    record wrapping
        |> field i0 "" input
        |> endRecord



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
            States0
            (States1 state)
        )


type alias MaybeDelta delta =
    CustomTypeDelta
        (Deltas2
            Deltas0
            (Deltas1 delta)
        )


maybe : Input state delta output -> Input (MaybeState state) (MaybeDelta delta) (Maybe output)
maybe input =
    customType
        |> tag0 i0 "Nothing" Nothing
        |> tag1 i1 "Just" Just "" input
        |> endCustomType



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
    -> Input state1 delta1 output1
    -> String
    -> Input state2 delta2 output2
    -> Input (TupleState state1 state2) (TupleDelta delta1 delta2) ( output1, output2 )
tuple fstId fst sndId snd =
    record Tuple.pair
        |> field i0 fstId fst
        |> field i1 sndId snd
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


list : Input state delta output -> Input (List (State state)) (ListDelta delta) (List output)
list (Input toInput) =
    let
        input =
            toInput ""
    in
    Input
        (\id ->
            let
                preUpdate =
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
                                        case input.validate item of
                                            Ok output ->
                                                Ok (output :: outputs)

                                            Err errs ->
                                                Err (identifyErrors errs)

                                    Err errs ->
                                        case input.validate item of
                                            Ok _ ->
                                                Err errs

                                            Err newErrs ->
                                                Err (identifyErrors newErrs ++ errs)
                            )
                            (Ok [])
                            (List.indexedMap Tuple.pair state)
                            |> Result.mapError (List.map (\feedback -> ( id, feedback )))
            in
            { id = id
            , index = 0
            , init = State Intact_ []
            , baseUpdate = preUpdate
            , update = preUpdate 0
            , view = listView input.view input.validate input.notify
            , baseValidate = validate
            , validate = validate
            , notify = \_ -> []
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
    , ids = []
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
    , ids = rec.ids ++ [ id ]
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

        update =
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
    in
    Input
        (\id ->
            let
                validate =
                    \(State _ state) -> parseRecordStates rec.parser rec.toOutput fields state
            in
            { id = id
            , index = 0
            , init = State Intact_ inits
            , baseUpdate = \_ -> update
            , update = update
            , view =
                \{ state } ->
                    let
                        fieldViews =
                            viewRecordStates rec.viewer emptyDeltas fields state

                        idViews =
                            List.map (\i -> H.h4 [ HA.style "color" "maroon" ] [ H.text i ]) rec.ids
                    in
                    H.div []
                        (List.map2
                            (\idView fieldView ->
                                H.div []
                                    [ idView
                                    , fieldView
                                    ]
                            )
                            idViews
                            fieldViews
                        )
            , baseValidate = validate
            , validate = validate
            , notify = \_ -> []
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


viewRecordStates viewer emptyDeltas fns states =
    viewer (\views _ End End -> views) [] emptyDeltas fns states
        |> List.reverse


recordStateViewer next views emptyDeltas ( fns, restFns ) ( State internalState state, restStates ) =
    next
        ((fns.field.view
            { state = state
            , status = statusFromInternalState fns.field.validate fns.field.notify (State internalState state)
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


statusFromInternalState :
    (State state -> Result (List ( String, String )) output)
    -> (State state -> List ( String, String ))
    -> State state
    -> Status
statusFromInternalState validator notifier (State internalState state) =
    case internalState of
        Intact_ ->
            Intact

        DebouncingSince _ ->
            Debouncing

        Idle_ ->
            let
                errors =
                    case validator (State internalState state) of
                        Ok _ ->
                            []

                        Err errs ->
                            errs

                notes =
                    notifier (State internalState state)
            in
            Idle (List.map Err errors ++ List.map Ok notes)


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


tag0 sel id tag =
    variant sel id (always tag)


tag1 sel id tag inputId input =
    variant sel
        id
        (record tag
            |> field i0 inputId input
            |> endRecord
        )


tag2 sel id tag id1 input1 id2 input2 =
    variant sel
        id
        (record tag
            |> field i0 id1 input1
            |> field i1 id2 input2
            |> endRecord
        )


tag3 sel id tag id1 input1 id2 input2 id3 input3 =
    variant sel
        id
        (record tag
            |> field i0 id1 input1
            |> field i1 id2 input2
            |> field i2 id3 input3
            |> endRecord
        )


tag4 sel id tag id1 input1 id2 input2 id3 input3 id4 input4 =
    variant sel
        id
        (record tag
            |> field i0 id1 input1
            |> field i1 id2 input2
            |> field i2 id3 input3
            |> field i3 id4 input4
            |> endRecord
        )


tag5 sel id tag id1 input1 id2 input2 id3 input3 id4 input4 id5 input5 =
    variant sel
        id
        (record tag
            |> field i0 id1 input1
            |> field i1 id2 input2
            |> field i2 id3 input3
            |> field i3 id4 input4
            |> field i4 id5 input5
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
                                updateRecordStates rec.updater emptyDeltas fns tagDelta state.tagStates
                        in
                        ( State s { state | tagStates = newTagStates }
                        , Cmd.map (ChangeState << TagDeltaReceived) cmd
                        )

                    _ ->
                        ( State s state, Cmd.none )
    in
    Input
        (\id ->
            let
                validate =
                    \(State _ state) -> parseSelectedTagState rec.parser state.selectedTag fns state.tagStates
            in
            { id = id
            , index = 0
            , init = State Intact_ { tagStates = inits, selectedTag = 0 }
            , baseUpdate = \_ -> update
            , update = update
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
            , baseValidate = validate
            , validate = validate
            , notify = \_ -> []
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
            fns.field.validate state

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


i6 =
    i5 >> i1


i7 =
    i6 >> i1


i8 =
    i7 >> i1


i9 =
    i8 >> i1


i10 =
    i9 >> i1


i11 =
    i10 >> i1


i12 =
    i11 >> i1


i13 =
    i12 >> i1


i14 =
    i13 >> i1


i15 =
    i14 >> i1


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


customTypeView :
    String
    -> List ( Int, String )
    -> Int
    -> Html (Delta (CustomTypeDelta variants))
    -> Html (Delta (CustomTypeDelta variants))
customTypeView id options selectedTag selectedTagView =
    H.div
        [ HA.style "margin-top" "10px"
        , HA.style "margin-bottom" "30px"
        ]
        (if List.length options > 1 then
            [ H.div []
                [ radioView options id selectedTag (ChangeState << TagSelected)
                , selectedTagView
                ]
            ]

         else
            [ selectedTagView ]
        )


listView :
    (ViewConfig state -> Html (Delta delta))
    -> (State state -> Result (List ( String, String )) output)
    -> (State state -> List ( String, String ))
    -> ViewConfig (List (State state))
    -> Html (Delta (ListDelta delta))
listView inputView inputParse inputFeedback config =
    H.div [ HA.style "margin-bottom" "30px" ]
        [ if List.isEmpty config.state then
            H.div
                [ HA.style "margin-top" "10px"
                , HA.style "margin-bottom" "10px"
                ]
                [ H.text "[ empty list ]"
                , H.button
                    [ HA.style "margin-left" "10px"
                    , HE.onClick (InsertItem 0)
                    ]
                    [ H.text "Add an item" ]
                ]

          else
            H.div []
                [ H.div
                    [ HA.style "margin-top" "10px"
                    , HA.style "margin-bottom" "10px"
                    ]
                    [ H.button [ HE.onClick (InsertItem 0) ]
                        [ H.text "Insert item" ]
                    ]
                , H.div []
                    (List.indexedMap
                        (\idx (State internalState state) ->
                            H.div [ HA.style "margin-bottom" "10px" ]
                                [ borderedDiv
                                    [ H.div [ HA.style "margin-bottom" "10px", HA.style "color" "gray" ]
                                        [ H.text ("Item #" ++ String.fromInt idx) ]
                                    , inputView
                                        { state = state
                                        , status = statusFromInternalState inputParse inputFeedback (State internalState state)
                                        , id = ""
                                        }
                                        |> H.map (ChangeItem idx)
                                    ]
                                , H.div [ HA.style "margin-top" "10px" ] [ H.button [ HE.onClick (DeleteItem idx) ] [ H.text ("Delete item #" ++ String.fromInt idx) ] ]
                                , H.div [ HA.style "margin-top" "10px" ] [ H.button [ HE.onClick (InsertItem (idx + 1)) ] [ H.text "Insert item" ] ]
                                ]
                        )
                        config.state
                    )
                ]
        ]
        |> H.map ChangeState


stringView : ViewConfig String -> Html String
stringView fieldState =
    H.div
        [ HA.style "margin-top" "10px"
        , HA.style "margin-bottom" "30px"
        , HA.id fieldState.id
        ]
        [ H.input
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


enumView : List ( String, enum ) -> ViewConfig enum -> Html enum
enumView tags config =
    H.div
        [ HA.style "margin-bottom" "30px", HA.style "margin-top" "10px" ]
        [ radioView (List.map (\( a, b ) -> ( b, a )) tags) config.id config.state identity ]


radioView : List ( state, String ) -> String -> state -> (state -> msg) -> Html msg
radioView options id selectedOption toMsg =
    H.div
        [ HA.style "margin-bottom" "10px"
        , HA.style "display" "flex"
        , HA.style "flex-wrap" "wrap"
        ]
        (List.map
            (\( option, label ) ->
                H.div
                    [ HA.style "margin-right" "5px"
                    , HA.style "margin-bottom" "5px"
                    ]
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
