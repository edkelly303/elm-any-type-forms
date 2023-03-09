module Control exposing (..)

import Internal


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


toForm =
    Internal.fromControl


bool =
    Internal.bool


enum =
    Internal.enum


float =
    Internal.float


int =
    Internal.int


list =
    Internal.list


maybe =
    Internal.maybe


tuple =
    Internal.tuple


string =
    Internal.string


wrapper =
    Internal.wrapper


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


create =
    Internal.makeControl


checkMsgType =
    Internal.checkMsgType


initWith =
    Internal.initWith


debounce =
    Internal.debounce


failIf =
    Internal.failIf


flagIf =
    Internal.flagIf


flagListAt =
    Internal.flagListIf


onFlag =
    Internal.onFlag


layout =
    Internal.layout
