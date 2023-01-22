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
    , atField0
    , atField1
    , atField11
    , atField12
    , atField13
    , atField14
    , atField15
    , atField2
    , atField3
    , atField4
    , atField5
    , atField6
    , atField7
    , atField8
    , atField9
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
    , field
    , hiddenField
    , initWith0Args
    , initWith1Arg
    , initWith2Args
    , initialise
    , int
    , layout
    , list
    , makeInput
    , maybe
    , noteIf
    , readOnlyField
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
    , wrapper, TypeCheck
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
    , update : Delta delta -> State state -> ( State state, Cmd msg )
    , view : State state -> Html msg
    , submit : State state -> Result (State state) output
    }


type alias InputConfig state delta output =
    { empty : state
    , initialise : output -> state
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
            , delta : Delta delta
            , initialise : Maybe (output -> state)
            , baseUpdate : Float -> Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , update : Delta delta -> State state -> ( State state, Cmd (Delta delta) )
            , childViews : ViewConfig state -> List (Html (Delta delta))
            , view : ViewConfig state -> Html (Delta delta)
            , baseValidate : State state -> Result (List ( String, String )) output
            , validate : State state -> Result (List ( String, String )) output
            , submit : State state -> State state
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
        { init, update, view, validate, notify, submit } =
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
    , submit =
        \state ->
            validate state
                |> Result.mapError (\_ -> submit state)
    }


type TypeCheck type_
    = TypeCheck


checkDeltaType : Input state delta output -> TypeCheck delta
checkDeltaType _ =
    TypeCheck


checkStateType : Input state delta output -> TypeCheck state
checkStateType _ =
    TypeCheck



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
                    wrapUpdate (\d s -> config.update d s |> (\ns -> ( ns, Cmd.none )))

                validate =
                    \(State _ state) ->
                        config.parse state
                            |> Result.mapError (List.map (Tuple.pair id))
            in
            { id = id
            , index = 0
            , init = State Intact_ config.empty
            , delta = Skip
            , initialise = Just config.initialise
            , baseUpdate = preUpdate
            , update = preUpdate 0
            , childViews = \_ -> []
            , view = config.view >> H.map ChangeState
            , baseValidate = validate
            , validate = validate
            , submit = \(State _ s) -> State Idle_ s
            , notify = \_ -> []
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



{-
   d88888b  .d8b.  d888888b db           d888888b d88888b
   88'     d8' `8b   `88'   88             `88'   88'
   88ooo   88ooo88    88    88              88    88ooo
   88~~~   88~~~88    88    88              88    88~~~
   88      88   88   .88.   88booo.        .88.   88
   YP      YP   YP Y888888P Y88888P      Y888888P YP
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



{-
   d8b   db  .d88b.  d888888b d88888b      d888888b d88888b
   888o  88 .8P  Y8. `~~88~~' 88'            `88'   88'
   88V8o 88 88    88    88    88ooooo         88    88ooo
   88 V8o88 88    88    88    88~~~~~         88    88~~~
   88  V888 `8b  d8'    88    88.            .88.   88
   VP   V8P  `Y88P'     YP    Y88888P      Y888888P YP
-}


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
   d8888b. d88888b d8888b.  .d88b.  db    db d8b   db  .o88b. d88888b
   88  `8D 88'     88  `8D .8P  Y8. 88    88 888o  88 d8P  Y8 88'
   88   88 88ooooo 88oooY' 88    88 88    88 88V8o 88 8P      88ooooo
   88   88 88~~~~~ 88~~~b. 88    88 88    88 88 V8o88 8b      88~~~~~
   88  .8D 88.     88   8D `8b  d8' 88b  d88 88  V888 Y8b  d8 88.
   Y8888D' Y88888P Y8888P'  `Y88P'  ~Y8888P' VP   V8P  `Y88P' Y88888P
-}


debounce : Float -> Input state delta output -> Input state delta output
debounce millis (Input input) =
    let
        debouncer i =
            { i | update = i.baseUpdate millis }
    in
    Input (input >> debouncer)



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
    -> Input state delta output
    -> Input state delta output
layout v (Input input) =
    let
        viewer i =
            { i | view = \config -> v (i.childViews config) config }
    in
    Input (input >> viewer)



{-
   d888888b d8b   db d888888b d888888b d888888b  .d8b.  db      d888888b .d8888. d88888b
     `88'   888o  88   `88'   `~~88~~'   `88'   d8' `8b 88        `88'   88'  YP 88'
      88    88V8o 88    88       88       88    88ooo88 88         88    `8bo.   88ooooo
      88    88 V8o88    88       88       88    88~~~88 88         88      `Y8b. 88~~~~~
     .88.   88  V888   .88.      88      .88.   88   88 88booo.   .88.   db   8D 88.
   Y888888P VP   V8P Y888888P    YP    Y888888P YP   YP Y88888P Y888888P `8888Y' Y88888P

-}


initialise : output -> Input state delta output -> Input state delta output
initialise output (Input input) =
    let
        initialiser i =
            case i.initialise of
                Nothing ->
                    i

                Just initialise_ ->
                    { i | init = State Intact_ (initialise_ output) }
    in
    Input (input >> initialiser)



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
        { empty = ""
        , initialise = String.fromInt
        , update = \delta _ -> delta
        , view = textInputView "number"
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
        { empty = ""
        , initialise = identity
        , update = \delta _ -> delta
        , view = textInputView "text"
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


datetime : Input String String Time.Posix
datetime =
    makeInput
        { empty = ""
        , initialise = Iso8601.fromTime >> String.replace "Z" ""
        , update = \delta _ -> delta
        , view = textInputView "datetime-local"
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
    -> Input enum enum enum
enum first second rest =
    makeInput
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


bool : String -> String -> Input Bool Bool Bool
bool true false =
    enum ( true, True ) ( false, False ) []



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


wrapper : (output -> wrapped) -> (wrapped -> output) -> Input state delta output -> Input (WrapperState state) (WrapperDelta delta) wrapped
wrapper wrap unwrap input =
    Input
        (\id ->
            let
                (Input toWrapped) =
                    record wrap
                        |> field unwrap id input
                        |> endRecord
            in
            toWrapped id
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
    Input
        (\id ->
            let
                (Input toWrapped) =
                    customType
                        |> tag0 "Nothing" Nothing
                        |> tag1 "Just" Just input
                        |> endCustomType
                            (\output ->
                                case output of
                                    Nothing ->
                                        initWith0Args atField0

                                    Just a ->
                                        initWith1Arg atField1 ( input, a )
                            )
            in
            toWrapped id
        )


initWith0Args sel fns init =
    let
        selector =
            instantiateSelector sel

        idx =
            selector.getField fns
                |> .field
                |> .index
    in
    { selectedTag = idx
    , tagStates = init
    }


initWith1Arg sel ( input, output ) fns init =
    let
        (Input toInnerInput) =
            input

        innerInput =
            toInnerInput ""

        tagSelector =
            instantiateSelector sel

        selectedTag =
            tagSelector.getField fns
                |> .field
                |> .index

        setTag =
            tagSelector.set

        setArg0 =
            instantiateSelector atField0
                |> .set
    in
    case innerInput.initialise of
        Nothing ->
            { selectedTag = 0
            , tagStates = init
            }

        Just initialise_ ->
            { selectedTag = selectedTag
            , tagStates =
                setTag
                    (\(State _ inner) ->
                        State Intact_
                            (setArg0
                                (\_ -> State Intact_ (initialise_ output))
                                inner
                            )
                    )
                    init
            }


initWith2Args sel ( input1, output1 ) ( input2, output2 ) fns init =
    let
        (Input toInnerInput1) =
            input1

        innerInput1 =
            toInnerInput1 ""

        (Input toInnerInput2) =
            input2

        innerInput2 =
            toInnerInput2 ""

        tagSelector =
            instantiateSelector sel

        selectedTag =
            tagSelector.getField fns
                |> .field
                |> .index

        setTag =
            tagSelector.set

        setArg0 =
            instantiateSelector atField0
                |> .set

        setArg1 =
            instantiateSelector atField1
                |> .set
    in
    case ( innerInput1.initialise, innerInput2.initialise ) of
        ( Just init1, Just init2 ) ->
            { selectedTag = selectedTag
            , tagStates =
                init
                    |> setTag
                        (\(State _ inner) ->
                            State Intact_
                                (setArg0
                                    (\_ -> State Intact_ (init1 output1))
                                    inner
                                )
                        )
                    |> setTag
                        (\(State _ inner) ->
                            State Intact_
                                (setArg1
                                    (\_ -> State Intact_ (init2 output2))
                                    inner
                                )
                        )
            }

        _ ->
            { selectedTag = 0
            , tagStates = init
            }



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
        |> field Tuple.first fstId fst
        |> field Tuple.second sndId snd
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
            , initialise =
                input.initialise |> Maybe.map (\i -> List.map (\item -> State Intact_ (i item)))
            , init = State Intact_ []
            , delta = Skip
            , baseUpdate = update
            , update = update 0
            , childViews = \_ -> []
            , view = listView input.view input.validate input.notify
            , baseValidate = validate
            , validate = validate
            , submit = \(State _ s) -> State Idle_ (List.map input.submit s)
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


oldRecord toOutput =
    { index = 0
    , ids = []
    , toOutput = toOutput
    , fields = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    , submitter = identity
    , before = identity
    , befores = identity
    , after = End
    , afters = End
    , makeSetters = identity
    }


oldField id (Input input) rec =
    let
        i =
            input id
    in
    { index = rec.index + 1
    , ids = rec.ids ++ [ id ]
    , toOutput = rec.toOutput
    , fields = rec.fields << Tuple.pair { field = { i | index = rec.index }, access = Open }
    , states = rec.states << Tuple.pair i.init
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> recordStateViewer
    , parser = rec.parser >> recordStateValidator
    , submitter = rec.submitter >> recordStateSubmitter
    , before = rec.before << Tuple.pair Skip
    , befores = rec.befores << Tuple.pair rec.before
    , after = ( Skip, rec.after )
    , afters = ( rec.after, rec.afters )
    , makeSetters = rec.makeSetters >> setterMaker
    }


makeSetters makeSetters_ befores afters =
    makeSetters_ (\End End -> End) (befores End) afters


setterMaker next ( before, befores ) ( after, afters ) =
    ( \value -> before ( value, after )
    , next befores afters
    )


oldEndRecord :
    { index : Int
    , fields : End -> fns
    , states : End -> state
    , makeSetters :
        (End -> End -> End)
        -> befores
        -> afters
        -> setters
    , before : before
    , befores : End -> befores
    , after : after
    , afters : afters
    , updater :
        ({ newStates : End -> state, newCmds : List (Cmd delta) }
         -> End
         -> End
         -> End
         -> End
         -> { newStates : End -> state, newCmds : List (Cmd delta) }
        )
        -> { newStates : state -> state, newCmds : List (Cmd delta) }
        -> fns
        -> setters
        -> delta
        -> state
        -> { newStates : End -> state, newCmds : List (Cmd delta) }
    , viewer :
        (List (Html (Delta delta))
         -> End
         -> End
         -> End
         -> List (Html (Delta delta))
        )
        -> List (Html (Delta delta))
        -> fns
        -> setters
        -> state
        -> List (Html (Delta delta))
    , ids : List String
    , parser :
        (Result (List ( String, String )) output -> End -> End -> Result (List ( String, String )) output)
        -> Result (List ( String, String )) toOutput
        -> fns
        -> state
        -> Result (List ( String, String )) output
    , submitter :
        (End -> End -> End)
        -> fns
        -> state
        -> state
    , toOutput : toOutput
    }
    -> Input state delta output
oldEndRecord rec =
    let
        fns =
            rec.fields End

        inits =
            rec.states End

        setters =
            makeSetters rec.makeSetters rec.befores rec.afters

        update delta (State s state) =
            case delta of
                Skip ->
                    ( State s state, Cmd.none )

                ChangeState deltas ->
                    let
                        ( newState, cmd ) =
                            updateRecordStates rec.updater fns setters deltas state
                    in
                    ( State s newState
                    , Cmd.map ChangeState cmd
                    )

                _ ->
                    ( State s state, Cmd.none )

        childViews config =
            viewRecordStates rec.viewer fns setters config.state

        view childViews_ config =
            let
                fieldViews =
                    childViews_ config

                idViews =
                    List.map (\i -> H.h4 [ HA.style "color" "maroon" ] [ H.text i ]) rec.ids

                combinedViews =
                    List.map2
                        (\idView fieldView ->
                            H.div []
                                [ idView
                                , fieldView
                                ]
                        )
                        idViews
                        fieldViews
            in
            if List.length combinedViews > 1 then
                borderedDiv combinedViews

            else
                H.div [] fieldViews

        validate (State _ state) =
            validateRecordStates rec.parser rec.toOutput fns state

        submit (State _ state) =
            State Idle_ (submitRecordStates rec.submitter fns state)
    in
    Input
        (\id ->
            { id = id
            , index = 0
            , init = State Intact_ inits
            , delta = Skip
            , initialise = Nothing
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view childViews config
            , baseValidate = validate
            , validate = validate
            , submit = submit
            , notify = \_ -> []
            }
        )


record toOutput =
    { index = 0
    , ids = []
    , toOutput = toOutput
    , fields = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    , submitter = identity
    , initialiser = identity
    , before = identity
    , befores = identity
    , after = End
    , afters = End
    , makeSetters = identity
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


internalField access fromOutput id (Input input) rec =
    let
        i =
            input id
    in
    { index = rec.index + 1
    , ids = rec.ids ++ [ id ]
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
    , submitter = rec.submitter >> recordStateSubmitter
    , initialiser = rec.initialiser >> recordStateInitialiser
    , before = rec.before << Tuple.pair Skip
    , befores = rec.befores << Tuple.pair rec.before
    , after = ( Skip, rec.after )
    , afters = ( rec.after, rec.afters )
    , makeSetters = rec.makeSetters >> setterMaker
    }


endRecord rec =
    let
        fns =
            rec.fields End

        inits =
            rec.states End

        setters =
            makeSetters rec.makeSetters rec.befores rec.afters

        update delta (State s state) =
            case delta of
                Skip ->
                    ( State s state, Cmd.none )

                ChangeState deltas ->
                    let
                        ( newState, cmd ) =
                            updateRecordStates rec.updater fns setters deltas state
                    in
                    ( State s newState
                    , Cmd.map ChangeState cmd
                    )

                _ ->
                    ( State s state, Cmd.none )

        childViews config =
            viewRecordStates rec.viewer fns setters config.state

        view childViews_ config =
            let
                fieldViews =
                    childViews_ config

                idViews =
                    List.map (\i -> H.h4 [ HA.style "color" "maroon" ] [ H.text i ]) rec.ids

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
            if List.length combinedViews > 1 then
                borderedDiv combinedViews

            else
                H.div [] fieldViews

        validate (State _ state) =
            validateRecordStates rec.parser rec.toOutput fns state

        submit (State _ state) =
            State Idle_ (submitRecordStates rec.submitter fns state)
    in
    Input
        (\id ->
            { id = id
            , index = 0
            , init = State Intact_ inits
            , delta = Skip
            , initialise = Just (\output -> initialiseRecordStates rec.initialiser output fns inits)
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view childViews config
            , baseValidate = validate
            , validate = validate
            , submit = submit
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


initialiseRecordStates initialiser output fns states =
    initialiser (\_ End End -> End) output fns states


recordStateInitialiser next output ( fns, restFns ) ( states, restStates ) =
    case fns.field.initialise of
        Just initialise_ ->
            ( State Intact_ (initialise_ (fns.fromOutput output))
            , next output restFns restStates
            )

        Nothing ->
            ( fns.field.init
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


submitRecordStates submitter fns states =
    submitter (\End End -> End) fns states


recordStateSubmitter next ( fns, restFns ) ( state, restStates ) =
    ( fns.field.submit state
    , next restFns restStates
    )


viewRecordStates viewer fns setters states =
    viewer (\views End End End -> views) [] fns setters states
        |> List.reverse


recordStateViewer next views ( fns, restFns ) ( setter, restSetters ) ( State internalState state, restStates ) =
    let
        view =
            fns.field.view
                { state = state
                , status = statusFromInternalState fns.field.validate fns.field.notify (State internalState state)
                , id = fns.field.id
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
        restFns
        restSetters
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
    , names = []
    , fns = identity
    , states = identity
    , updater = identity
    , viewer = identity
    , parser = identity
    , submitter = identity
    , before = identity
    , befores = identity
    , after = End
    , afters = End
    , makeSetters = identity
    }


variant id (Input input) rec =
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
                }
    , states = rec.states << Tuple.pair i.init
    , updater = rec.updater >> recordStateUpdater
    , viewer = rec.viewer >> selectedTagViewer
    , parser = rec.parser >> selectedTagParser
    , submitter = rec.submitter >> selectedTagSubmitter
    , before = rec.before << Tuple.pair Skip
    , befores = rec.befores << Tuple.pair rec.before
    , after = ( Skip, rec.after )
    , afters = ( rec.after, rec.afters )
    , makeSetters = rec.makeSetters >> setterMaker
    }


tag0 id tag =
    let
        null =
            makeInput
                { empty = States0
                , initialise = \_ -> States0
                , update = \_ _ -> States0
                , view = \_ -> H.text ""
                , parse = \_ -> Ok tag
                }
    in
    variant id null


tag1 id tag input =
    variant
        id
        (oldRecord tag
            |> oldField "" input
            |> oldEndRecord
        )


tag2 id tag id1 input1 id2 input2 =
    variant
        id
        (oldRecord tag
            |> oldField id1 input1
            |> oldField id2 input2
            |> oldEndRecord
        )


tag3 id tag id1 input1 id2 input2 id3 input3 =
    variant
        id
        (oldRecord tag
            |> oldField id1 input1
            |> oldField id2 input2
            |> oldField id3 input3
            |> oldEndRecord
        )


tag4 id tag id1 input1 id2 input2 id3 input3 id4 input4 =
    variant
        id
        (oldRecord tag
            |> oldField id1 input1
            |> oldField id2 input2
            |> oldField id3 input3
            |> oldField id4 input4
            |> oldEndRecord
        )


tag5 id tag id1 input1 id2 input2 id3 input3 id4 input4 id5 input5 =
    variant
        id
        (oldRecord tag
            |> oldField id1 input1
            |> oldField id2 input2
            |> oldField id3 input3
            |> oldField id4 input4
            |> oldField id5 input5
            |> oldEndRecord
        )


endCustomType initialiser rec =
    let
        fns =
            rec.fns End

        inits =
            rec.states End

        setters =
            makeSetters rec.makeSetters rec.befores rec.afters

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
                                updateRecordStates rec.updater fns setters tagDelta state.tagStates
                        in
                        ( State s { state | tagStates = newTagStates }
                        , Cmd.map (ChangeState << TagDeltaReceived) cmd
                        )

                    _ ->
                        ( State s state, Cmd.none )

        childViews config =
            [ viewSelectedTagState rec.viewer config.state.selectedTag fns setters config.state.tagStates ]

        view config =
            let
                options =
                    List.indexedMap Tuple.pair names

                childViews_ =
                    childViews config
            in
            customTypeView config.id options config.state.selectedTag childViews_
    in
    Input
        (\id ->
            let
                validate =
                    \(State _ state) -> validateSelectedTagState rec.parser state.selectedTag fns state.tagStates

                submit =
                    \(State _ state) -> State Idle_ (submitSelectedTagState rec.submitter state.selectedTag fns state.tagStates)
            in
            { id = id
            , index = 0
            , init = State Intact_ { tagStates = inits, selectedTag = 0 }
            , delta = Skip
            , initialise = Just (\output -> initialiser output fns inits)
            , baseUpdate = \_ -> update
            , update = update
            , childViews = \config -> childViews config
            , view = \config -> view config
            , baseValidate = validate
            , validate = validate
            , submit = submit
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


submitSelectedTagState submitter selectedTag fns tagStates =
    { selectedTag = selectedTag
    , tagStates =
        submitter
            (\_ End End -> End)
            selectedTag
            fns
            tagStates
    }


selectedTagSubmitter next selectedTag ( fns, restFns ) ( state, restStates ) =
    ( if fns.field.index == selectedTag then
        fns.field.submit state

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


viewSelectedTagState viewer selectedTag fns setters states =
    viewer (\maybeView _ End End End -> maybeView) Nothing selectedTag fns setters states
        |> Maybe.map (H.map (ChangeState << TagDeltaReceived))
        |> Maybe.withDefault (H.text "ERROR!")


selectedTagViewer next maybeView selectedTag ( fns, restFns ) ( setter, restSetters ) ( State s state, restStates ) =
    next
        (if fns.field.index == selectedTag then
            Just
                (fns.field.view
                    { state = state
                    , status = Intact
                    , id = fns.field.id
                    }
                    |> H.map setter
                )

         else
            maybeView
        )
        selectedTag
        restFns
        restSetters
        restStates



{-
   .d8888. d88888b db      d88888b  .o88b. d888888b  .d88b.  d8888b. .d8888.
   88'  YP 88'     88      88'     d8P  Y8 `~~88~~' .8P  Y8. 88  `8D 88'  YP
   `8bo.   88ooooo 88      88ooooo 8P         88    88    88 88oobY' `8bo.
     `Y8b. 88~~~~~ 88      88~~~~~ 8b         88    88    88 88`8b     `Y8b.
   db   8D 88.     88booo. 88.     Y8b  d8    88    `8b  d8' 88 `88. db   8D
   `8888Y' Y88888P Y88888P Y88888P  `Y88P'    YP     `Y88P'  88   YD `8888Y'
-}


atField0 =
    composeSelectors { getFieldState = identity, getField = identity, set = identity }


atField1 =
    composeSelectors { getFieldState = Tuple.second, getField = Tuple.second, set = Tuple.mapSecond }


atField2 =
    atField1 >> atField1


atField3 =
    atField2 >> atField1


atField4 =
    atField3 >> atField1


atField5 =
    atField4 >> atField1


atField6 =
    atField5 >> atField1


atField7 =
    atField6 >> atField1


atField8 =
    atField7 >> atField1


atField9 =
    atField8 >> atField1


atField10 =
    atField9 >> atField1


atField11 =
    atField10 >> atField1


atField12 =
    atField11 >> atField1


atField13 =
    atField12 >> atField1


atField14 =
    atField13 >> atField1


atField15 =
    atField14 >> atField1


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
    -> List (Html (Delta (CustomTypeDelta variants)))
    -> Html (Delta (CustomTypeDelta variants))
customTypeView id options selectedTag selectedTagView =
    H.div []
        (if List.length options > 1 then
            [ H.div []
                [ radioView
                    { options = options
                    , id = id
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
listView inputView inputParse inputFeedback config =
    H.div []
        [ if List.isEmpty config.state then
            H.button [ HE.onClick (InsertItem 0) ] [ H.text "➕ Add an item" ]

          else
            H.div []
                (List.indexedMap
                    (\idx (State internalState state) ->
                        H.div []
                            [ H.details []
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
                                , inputView
                                    { state = state
                                    , status = statusFromInternalState inputParse inputFeedback (State internalState state)
                                    , id = config.id ++ "-item#" ++ String.fromInt (idx + 1)
                                    }
                                    |> H.map (ChangeItem idx)
                                ]
                            ]
                    )
                    config.state
                )
        ]
        |> H.map ChangeState


textInputView : String -> ViewConfig String -> Html String
textInputView type_ config =
    H.div
        []
        [ H.input
            [ HE.onInput identity
            , HA.type_ type_
            , HA.id config.id
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
        , id = config.id
        , selectedOption = config.state
        , toMsg = identity
        , columns = 3
        }


radioView :
    { options : List ( state, String )
    , id : String
    , selectedOption : state
    , toMsg : state -> msg
    , columns : Int
    }
    -> Html msg
radioView { options, id, selectedOption, toMsg, columns } =
    H.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" (String.repeat columns "1fr ")
        ]
        (List.map
            (\( option, label ) ->
                H.div
                    [ HA.style "margin-right" "15px"
                    , HA.style "margin-bottom" "10px"
                    ]
                    [ H.input
                        [ HA.type_ "radio"
                        , HA.id (id ++ "-" ++ label)
                        , HA.name id
                        , HA.value label
                        , HA.checked (selectedOption == option)
                        , onChecked (toMsg option)
                        ]
                        []
                    , H.label
                        [ HA.for (id ++ "-" ++ label)
                        , HA.style "margin-left" "4px"
                        ]
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
