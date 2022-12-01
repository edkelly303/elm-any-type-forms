module Example exposing (..)

import Browser
import Form exposing (..)
import Item


main =
    Browser.element
        { init = \() -> ( mainForm.init, Cmd.none )
        , view = mainForm.view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update msg model =
    case msg of
        FormMsg formMsg ->
            let
                ( newModel, cmd ) =
                    mainForm.update formMsg model

                _ =
                    Debug.log "output" (mainForm.submit newModel)
            in
            ( newModel, cmd )


type
    Msg
    -- = FormMsg (Delta String)
    -- = FormMsg (Delta SimpleRecordDelta)
    -- = FormMsg (Delta SimpleCustomTypeDelta)
    -- = FormMsg (Delta NestedRecordDelta)
    = FormMsg (Delta Item.ItemDelta)


mainForm =
    -- boundedInt
    -- simpleRecordInput
    -- simpleCustomTypeInput
    -- nestedRecordInput
    Item.item
        |> toForm "my first form example" FormMsg


boundedInt : Input String String Int
boundedInt =
    int
        |> failIf (\x -> x <= 0) "must be greater than 0"
        |> failIf (\x -> x > 120) "maximum is 120"


type alias SimpleRecord =
    { name : String
    , age : Int
    }


type alias SimpleRecordDelta =
    Deltas2
        String
        String


type alias SimpleRecordState =
    States2
        String
        String


simpleRecordInput :
    Input
        SimpleRecordState
        SimpleRecordDelta
        SimpleRecord
simpleRecordInput =
    record SimpleRecord
        |> field i0 "name" string
        |> field i1 "age" boundedInt
        |> endRecord


type SimpleCustomType
    = Red Int
    | Green String (Maybe Int)
    | Blue


type alias SimpleCustomTypeDelta =
    CustomTypeDelta
        (Deltas3
            (Deltas1 String)
            (Deltas2 String (MaybeDelta String))
            Deltas0
        )


type alias SimpleCustomTypeState =
    CustomTypeState
        (States3
            (States1 String)
            (States2 String (MaybeState String))
            States0
        )


simpleCustomTypeInput :
    Input
        SimpleCustomTypeState
        SimpleCustomTypeDelta
        SimpleCustomType
simpleCustomTypeInput =
    customType
        |> tag1 i0 "red" Red "integer" boundedInt
        |> tag2 i1 "green" Green "string" string "maybe int" (maybe int)
        |> tag0 i2 "blue" Blue
        |> endCustomType


type alias NestedRecord =
    { titles : List Int
    , record : SimpleRecord
    , custom : SimpleCustomType
    }


type alias NestedRecordDelta =
    Deltas3
        (ListDelta String)
        SimpleRecordDelta
        SimpleCustomTypeDelta


type alias NestedRecordState =
    States3
        (ListState String)
        SimpleRecordState
        SimpleCustomTypeState


nestedRecordInput :
    Input
        NestedRecordState
        NestedRecordDelta
        NestedRecord
nestedRecordInput =
    record NestedRecord
        |> field i0 "number list" (list boundedInt)
        |> field i1 "record" simpleRecordInput
        |> field i2 "custom type" simpleCustomTypeInput
        |> endRecord
