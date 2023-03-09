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
