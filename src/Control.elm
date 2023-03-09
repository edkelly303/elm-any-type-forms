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
    -> c
    -> Internal.AdvancedControl input state delta output
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> ( { field : Internal.ControlFns input state delta output, fromInput : c, access : Internal.Access }, d ) -> e
            , states : Path.Path -> ( Internal.State state, f ) -> g
            , updater : h -> { newStates : restStates -> recordState0, newCmds : List (Cmd recordDelta) } -> restFns -> restDeltaSetters -> restDeltas -> restStates -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> k -> l -> m -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) output1 -> o -> p -> Result (List ( String, String )) output2
            , idleSetter : q -> r -> s -> s
            , initialiser : t -> recordInput -> u -> v
            , before : ( Internal.Delta w, x ) -> y
            , befores : ( ( Internal.Delta w, x ) -> y, z ) -> a1
            , after : b1
            , afters : c1
            , makeSetters : d1 -> befores -> afters -> next
            , flagEmitter : e1 -> List Internal.Flag -> f1 -> g1 -> List Internal.Flag
            , flagReceiver : h1 -> List Internal.Flag -> List ( String, String ) -> i1 -> j1 -> List ( String, String )
            , receiverCollector : k1 -> List Internal.Flag -> l1 -> m1 -> List Internal.Flag
            }
            n1
    ->
        Internal.Builder
            { index : number
            , labels : List String
            , toOutput : b
            , fields : Path.Path -> d -> e
            , states : Path.Path -> f -> g
            , updater : h -> { newStates : ( Internal.State o1, restStates ) -> recordState0, newCmds : List (Cmd recordDelta) } -> ( Internal.RecordFns p1 o1 q1 r1 recordOutput, restFns ) -> ( Internal.Delta q1 -> recordDelta, restDeltaSetters ) -> ( Internal.Delta q1, restDeltas ) -> ( Internal.State o1, restStates ) -> { newStates : recordState1, newCmds : List (Cmd recordDelta) }
            , viewer : i -> List (Html.Html (Internal.Delta j)) -> List Internal.Flag -> ( Internal.RecordFns s1 t1 u1 v1 w1, k ) -> ( Internal.Delta u1 -> j, l ) -> ( Internal.State t1, m ) -> List (Html.Html (Internal.Delta j))
            , parser : n -> Result (List ( String, String )) (output0 -> output1) -> ( Internal.RecordFns x1 y1 z1 output0 a2, o ) -> ( Internal.State y1, p ) -> Result (List ( String, String )) output2
            , idleSetter : q -> ( Internal.RecordFns b2 c2 d2 e2 f2, r ) -> ( Internal.State c2, s ) -> ( Internal.State c2, s )
            , initialiser : t -> recordInput -> ( Internal.RecordFns g2 h2 i2 g2 recordInput, u ) -> ( Internal.State h2, v )
            , before : x -> y
            , befores : z -> a1
            , after : ( Internal.Delta j2, b1 )
            , afters : ( b1, c1 )
            , makeSetters : d1 -> ( ( value, after ) -> k2, befores ) -> ( after, afters ) -> ( value -> k2, next )
            , flagEmitter : e1 -> List Internal.Flag -> ( Internal.RecordFns l2 m2 n2 o2 p2, f1 ) -> ( Internal.State m2, g1 ) -> List Internal.Flag
            , flagReceiver : h1 -> List Internal.Flag -> List ( String, String ) -> ( Internal.RecordFns q2 r2 s2 t2 u2, i1 ) -> ( Internal.State r2, j1 ) -> List ( String, String )
            , receiverCollector : k1 -> List Internal.Flag -> ( Internal.RecordFns v2 w2 x2 y2 z2, l1 ) -> ( Internal.State w2, m1 ) -> List Internal.Flag
            }
            n1
field =
    Internal.field


readOnlyField =
    Internal.readOnlyField


hiddenField =
    Internal.hiddenField


end : Internal.Builder
          { d
              | afters : afters1
              , befores : Internal.End -> befores1
              , fields :
                    Path.Path
                    -> Internal.End
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
              , flagEmitter :
                    (
                    List Internal.Flag
                    -> Internal.End
                    -> Internal.End
                    -> List Internal.Flag
                    )
                    -> List Internal.Flag
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.State state, restStates )
                    -> List Internal.Flag
              , flagReceiver :
                    (
                    List Internal.Flag
                    -> List ( String, String )
                    -> Internal.End
                    -> Internal.End
                    -> List ( String, String )
                    )
                    -> List Internal.Flag
                    -> List ( String, String )
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.State state, restStates )
                    -> List ( String, String )
              , idleSetter :
                    (Internal.End -> Internal.End -> Internal.End)
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.State state, restStates )
                    -> ( Internal.State state, restStates )
              , initialiser :
                    (input -> Internal.End -> Internal.End)
                    -> input
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.State state, restStates )
              , labels : List String
              , makeSetters :
                    (Internal.End -> Internal.End -> Internal.End)
                    -> befores1
                    -> afters1
                    -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
              , parser :
                    (
                    Result (List ( String, String )) output
                    -> Internal.End
                    -> Internal.End
                    -> Result (List ( String, String )) output
                    )
                    -> Result (List ( String, String )) output1_1
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.State state, restStates )
                    -> Result (List ( String, String )) output
              , receiverCollector :
                    (
                    List Internal.Flag
                    -> Internal.End
                    -> Internal.End
                    -> List Internal.Flag
                    )
                    -> List Internal.Flag
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.State state, restStates )
                    -> List Internal.Flag
              , states :
                    Path.Path -> Internal.End -> ( Internal.State state, restStates )
              , toOutput : output1_1
              , updater :
                    (
                    { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                    , newStates : Internal.End -> ( Internal.State state, restStates )
                    }
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                       , newStates :
                             Internal.End -> ( Internal.State state, restStates )
                       }
                    )
                    -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                       , newStates :
                             ( Internal.State state, restStates )
                             -> ( Internal.State state, restStates )
                       }
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
                    -> ( Internal.Delta delta, restDeltas )
                    -> ( Internal.State state, restStates )
                    -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                       , newStates :
                             Internal.End -> ( Internal.State state, restStates )
                       }
              , viewer :
                    (
                    List
                        (
                        Html.Html
                            (Internal.Delta ( Internal.Delta delta, restDeltas ))
                        )
                    -> List Internal.Flag
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> List
                           (
                           Html.Html
                               (Internal.Delta ( Internal.Delta delta, restDeltas ))
                           )
                    )
                    -> List
                           (
                           Html.Html
                               (Internal.Delta ( Internal.Delta delta, restDeltas ))
                           )
                    -> List Internal.Flag
                    -> ( Internal.RecordFns input2 state delta output2 recordOutput
                       , restFns1
                       )
                    -> ( Internal.Delta delta -> recordDelta, restDeltaSetters )
                    -> ( Internal.State state, restStates )
                    -> List
                           (
                           Html.Html
                               (Internal.Delta ( Internal.Delta delta, restDeltas ))
                           )
          }
          { e
              | applyInputs :
                    (state1 -> Internal.End -> state1)
                    -> (stateSetter -> state0)
                    -> ( stateSetter, restStateSetters )
                    -> input
                    -> Internal.State ( Internal.State state, restStates )
              , deltaAfters : afters
              , deltaBefores : Internal.End -> befores
              , destructor : stateSetter -> state0
              , flagEmitter :
                    (
                    List Internal.Flag
                    -> Int
                    -> Internal.End
                    -> Internal.End
                    -> List Internal.Flag
                    )
                    -> List Internal.Flag
                    -> Int
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.State state, restStates )
                    -> List Internal.Flag
              , flagReceiver :
                    (
                    List Internal.Flag
                    -> List ( String, String )
                    -> Internal.End
                    -> Internal.End
                    -> List ( String, String )
                    )
                    -> List Internal.Flag
                    -> List ( String, String )
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.State state, restStates )
                    -> List ( String, String )
              , fns :
                    Path.Path
                    -> Internal.End
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
              , idleSetter :
                    (Int -> Internal.End -> Internal.End -> Internal.End)
                    -> Int
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.State state, restStates )
                    -> ( Internal.State state, restStates )
              , labels : List String
              , makeDeltaSetters :
                    (Internal.End -> Internal.End -> Internal.End)
                    -> befores
                    -> afters
                    -> ( Internal.Delta delta -> ( Internal.Delta delta, restDeltas )
                       , restSetters
                       )
              , makeStateSetters :
                    (
                    a1
                    -> b1
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    )
                    -> c
                    -> ( Internal.State state, restStates )
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> f
                    -> g
                    -> h
                    -> ( stateSetter, restStateSetters )
              , parser :
                    (a -> b -> Internal.End -> Internal.End -> a)
                    -> Result (List ( String, String )) value
                    -> Int
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.State state, restStates )
                    -> Result (List ( String, String )) output
              , receiverCollector :
                    (
                    List Internal.Flag
                    -> Internal.End
                    -> Internal.End
                    -> List Internal.Flag
                    )
                    -> List Internal.Flag
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.State state, restStates )
                    -> List Internal.Flag
              , stateAfters : h
              , stateBefores : Internal.End -> g
              , stateInserter : c
              , states :
                    Path.Path -> Internal.End -> ( Internal.State state, restStates )
              , toArgStates : Internal.End -> f
              , updater :
                    (
                    { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                    , newStates : Internal.End -> ( Internal.State state, restStates )
                    }
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                       , newStates :
                             Internal.End -> ( Internal.State state, restStates )
                       }
                    )
                    -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                       , newStates :
                             ( Internal.State state, restStates )
                             -> ( Internal.State state, restStates )
                       }
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.Delta delta -> ( Internal.Delta delta, restDeltas )
                       , restSetters
                       )
                    -> ( Internal.Delta delta, restDeltas )
                    -> ( Internal.State state, restStates )
                    -> { newCmds : List (Cmd ( Internal.Delta delta, restDeltas ))
                       , newStates :
                             Internal.End -> ( Internal.State state, restStates )
                       }
              , viewer :
                    (
                    Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
                    -> List Internal.Flag
                    -> Int
                    -> Internal.End
                    -> Internal.End
                    -> Internal.End
                    -> Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
                    )
                    -> Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
                    -> List Internal.Flag
                    -> Int
                    -> ( Internal.ControlFns input1 state delta output1, restFns )
                    -> ( Internal.Delta delta -> ( Internal.Delta delta, restDeltas )
                       , restSetters
                       )
                    -> ( Internal.State state, restStates )
                    -> Maybe (Html.Html ( Internal.Delta delta, restDeltas ))
          }
      -> Internal.AdvancedControl
             input
             ( Internal.State state, restStates )
             ( Internal.Delta delta, restDeltas )
             output

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
