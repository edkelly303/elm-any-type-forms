module Control exposing (..)

import Dict
import Html
import Internal
import Path


type alias Control state delta output =
    Internal.Control state delta output


type alias ControlConfig state delta output =
    Internal.ControlConfig state delta output


type alias Delta delta =
    Internal.Delta delta


type alias End =
    Internal.End


type alias Flag =
    Internal.Flag


type alias Form state delta output msg =
    Internal.Form state delta output msg


type alias ListDelta delta =
    Internal.ListDelta delta


type alias State state =
    Internal.State state


type alias Status =
    Internal.Status


type alias ViewConfig state =
    Internal.ViewConfig state


toForm :
    String
    -> (Delta delta -> msg)
    -> Control state delta output
    -> Form state delta output msg
toForm =
    Internal.fromControl


bool : String -> String -> Control Bool Bool Bool
bool =
    Internal.bool


enum :
    ( String, enum )
    -> ( String, enum )
    -> List ( String, enum )
    -> Control enum enum enum
enum =
    Internal.enum


float : Control String String Float
float =
    Internal.float


int : Control String String Int
int =
    Internal.int


list :
    Control state delta output
    ->
        Control
            (List (State state))
            (ListDelta delta)
            (List output)
list =
    Internal.list


maybe :
    Control state delta output
    ->
        Control
            ( State (), ( State ( State state, End ), End ) )
            ( Delta (), ( Delta ( Delta delta, End ), End ) )
            (Maybe output)
maybe =
    Internal.maybe


tuple :
    String
    -> Control state1 delta1 output1
    -> String
    -> Control state2 delta2 output2
    ->
        Control
            ( State state1, ( State state2, End ) )
            ( Delta delta1, ( Delta delta2, End ) )
            ( output1, output2 )
tuple =
    Internal.tuple


string : Control String String String
string =
    Internal.string


wrapper :
    (output -> wrapped)
    -> (wrapped -> output)
    -> Control state delta output
    -> Control ( State state, End ) ( Delta delta, End ) wrapped
wrapper =
    Internal.wrapper


customType :
    b
    ->
        Internal.Builder
            r
            { applyInputs : a15 -> a15
            , deltaAfter : End
            , deltaAfters : End
            , deltaBefore : a14 -> a14
            , deltaBefores : a13 -> a13
            , destructor : b
            , flagEmitter : a12 -> a12
            , flagReceiver : a11 -> a11
            , fns : c -> d -> d
            , idleSetter : a10 -> a10
            , index : number
            , labels : List e
            , makeDeltaSetters : a9 -> a9
            , makeStateSetters : a8 -> a8
            , parser : a7 -> a7
            , receiverCollector : a6 -> a6
            , stateAfter : End
            , stateAfters : End
            , stateBefore : a5 -> a5
            , stateBefores : a4 -> a4
            , stateInserter : a3 -> a3
            , states : f -> g -> g
            , toArgStates : a2 -> a2
            , updater : a1 -> a1
            , viewer : a -> a
            }
customType =
    Internal.customType


tag0 =
    Internal.tag0


tag1 =
    Internal.tag1


tag2 =
    Internal.tag2


record =
    Internal.record


field :
    String
    -> b
    -> Control state8 delta10 output7
    ->
        Internal.Builder
            { g
                | after : d
                , afters : e
                , before : ( Internal.Delta delta11, a12 ) -> c4
                , befores : ( ( Internal.Delta delta11, a12 ) -> c4, a11 ) -> c3
                , fields :
                    Path.Path
                    ->
                        ( { access : Internal.Access
                          , field :
                                Internal.ControlFns output7 state8 delta10 output7
                          , fromInput : b
                          }
                        , a10
                        )
                    -> c2
                , flagEmitter :
                    a9
                    -> List Internal.Flag
                    -> restFns7
                    -> restStates7
                    -> List Internal.Flag
                , flagReceiver :
                    a8
                    -> List Internal.Flag
                    -> List ( String, String )
                    -> restFns6
                    -> restStates6
                    -> List ( String, String )
                , idleSetter : a7 -> restFns5 -> restStates5 -> restStates5
                , index : number
                , initialiser : a6 -> recordInput -> restFns4 -> restStates4
                , labels : List String
                , makeSetters : a5 -> befores -> afters -> next
                , parser :
                    a4
                    -> Result (List ( String, String )) output1_1
                    -> restFns3
                    -> restStates3
                    -> Result (List ( String, String )) output2_1
                , receiverCollector :
                    a3
                    -> List Internal.Flag
                    -> restFns2
                    -> restStates2
                    -> List Internal.Flag
                , states : Path.Path -> ( Internal.State state8, a2 ) -> c1
                , toOutput : f
                , updater :
                    a1
                    ->
                        { newCmds : List (Cmd recordDelta1)
                        , newStates : restStates1 -> recordState0
                        }
                    -> restFns1
                    -> restDeltaSetters1
                    -> restDeltas
                    -> restStates1
                    ->
                        { newCmds : List (Cmd recordDelta1)
                        , newStates : recordState1
                        }
                , viewer :
                    a
                    -> List (Html.Html (Internal.Delta recordDelta))
                    -> List Internal.Flag
                    -> restFns
                    -> restDeltaSetters
                    -> restStates
                    -> List (Html.Html (Internal.Delta recordDelta))
            }
            c
    ->
        Internal.Builder
            { after : ( Internal.Delta delta9, d )
            , afters : ( d, e )
            , before : a12 -> c4
            , befores : a11 -> c3
            , fields : Path.Path -> a10 -> c2
            , flagEmitter :
                a9
                -> List Internal.Flag
                ->
                    ( { fns7
                        | field : Internal.ControlFns input6 state7 delta8 output6
                      }
                    , restFns7
                    )
                -> ( Internal.State state7, restStates7 )
                -> List Internal.Flag
            , flagReceiver :
                a8
                -> List Internal.Flag
                -> List ( String, String )
                ->
                    ( { fns6
                        | field : Internal.ControlFns input5 state6 delta7 output5
                      }
                    , restFns6
                    )
                -> ( Internal.State state6, restStates6 )
                -> List ( String, String )
            , idleSetter :
                a7
                ->
                    ( { fns5
                        | field : Internal.ControlFns input4 state5 delta6 output4
                      }
                    , restFns5
                    )
                -> ( Internal.State state5, restStates5 )
                -> ( Internal.State state5, restStates5 )
            , index : number
            , initialiser :
                a6
                -> recordInput
                ->
                    ( { fns4
                        | field :
                            Internal.ControlFns fieldInput state4 delta5 output3
                        , fromInput : recordInput -> fieldInput
                      }
                    , restFns4
                    )
                -> ( Internal.State state4, restStates4 )
            , labels : List String
            , makeSetters :
                a5
                -> ( ( value, after ) -> delta4, befores )
                -> ( after, afters )
                -> ( value -> delta4, next )
            , parser :
                a4
                -> Result (List ( String, String )) (output0 -> output1_1)
                ->
                    ( { fns3
                        | field : Internal.ControlFns input3 state3 delta3 output0
                      }
                    , restFns3
                    )
                -> ( Internal.State state3, restStates3 )
                -> Result (List ( String, String )) output2_1
            , receiverCollector :
                a3
                -> List Internal.Flag
                ->
                    ( { fns2
                        | field : Internal.ControlFns input2 state2 delta2 output2
                      }
                    , restFns2
                    )
                -> ( Internal.State state2, restStates2 )
                -> List Internal.Flag
            , states : Path.Path -> a2 -> c1
            , toOutput : f
            , updater :
                a1
                ->
                    { newCmds : List (Cmd recordDelta1)
                    , newStates :
                        ( Internal.State state1, restStates1 ) -> recordState0
                    }
                ->
                    ( { fns1
                        | field : Internal.ControlFns input1 state1 delta1 output1
                      }
                    , restFns1
                    )
                -> ( Internal.Delta delta1 -> recordDelta1, restDeltaSetters1 )
                -> ( Internal.Delta delta1, restDeltas )
                -> ( Internal.State state1, restStates1 )
                -> { newCmds : List (Cmd recordDelta1), newStates : recordState1 }
            , viewer :
                a
                -> List (Html.Html (Internal.Delta recordDelta))
                -> List Internal.Flag
                ->
                    ( { fns
                        | access : Internal.Access
                        , field : Internal.ControlFns input state delta output
                      }
                    , restFns
                    )
                -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
                -> ( Internal.State state, restStates )
                -> List (Html.Html (Internal.Delta recordDelta))
            }
            c
field =
    Internal.field


readOnlyField =
    Internal.readOnlyField


hiddenField =
    Internal.hiddenField


end =
    Internal.end


create : ControlConfig state delta output -> Control state delta output
create =
    Internal.makeControl


checkMsgType : Control state delta output -> Delta delta
checkMsgType =
    Internal.checkMsgType


initWith :
    output
    -> Control state delta output
    -> Control state delta output
initWith =
    Internal.initWith


debounce :
    Float
    -> Control state delta output
    -> Control state delta output
debounce =
    Internal.debounce


failIf :
    (output -> Bool)
    -> String
    -> Control state delta output
    -> Control state delta output
failIf =
    Internal.failIf


flagIf :
    (output -> Bool)
    -> String
    -> Control state delta output
    -> Control state delta output
flagIf =
    Internal.flagIf


flagListAt :
    (output -> List Int)
    -> String
    -> Control (List state) (ListDelta delta) output
    -> Control (List state) (ListDelta delta) output
flagListAt =
    Internal.flagListAt


onFlag :
    String
    -> String
    -> Control state delta output
    -> Control state delta output
onFlag =
    Internal.onFlag


layout :
    (List (Html.Html (Delta delta))
     -> ViewConfig state
     -> Html.Html (Delta delta)
    )
    -> Control state delta output
    -> Control state delta output
layout =
    Internal.layout


dict :
    String
    -> Control keyState keyDelta comparable
    -> String
    -> Control valueState valueDelta value
    ->
        Control
            ( State (List (State ( State keyState, ( State valueState, End ) ))), End )
            ( Delta (ListDelta ( Delta keyDelta, ( Delta valueDelta, End ) )), End )
            (Dict.Dict comparable value)
dict =
    Internal.dict
