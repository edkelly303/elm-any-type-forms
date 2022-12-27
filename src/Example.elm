module Example exposing (..)

import Browser
import Form exposing (..)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Item


main =
    Browser.element
        { init = \() -> ( mainForm.init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : State Item.ItemState -> Html Msg
view model =
    H.div
        [ HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "font-family" "sans"
        , HA.style "background-color" "gray"
        ]
        [ H.div
            [ HA.style "width" "600px"
            , HA.style "outline" "solid 1px gray"
            , HA.style "border-radius" "5px"
            , HA.style "margin" "10px"
            , HA.style "padding" "20px"
            , HA.style "background-color" "white"
            ]
            [ mainForm.view model
            , H.button [ HE.onClick Submit ] [ H.text "Submit" ]
            ]
        ]


update msg model =
    case msg of
        Submit ->
            case mainForm.submit model of
                Ok _ ->
                    ( model, Cmd.none )

                Err newModel ->
                    ( newModel, Cmd.none )

        FormMsg formMsg ->
            let
                ( newModel, cmd ) =
                    mainForm.update formMsg model
            in
            ( newModel, cmd )


type
    Msg
    -- = FormMsg (Delta String)
    -- = FormMsg (Delta SimpleRecordDelta)
    -- = FormMsg (Delta SimpleCustomTypeDelta)
    -- = FormMsg (Delta NestedRecordDelta)
    = FormMsg (Delta Item.ItemDelta)
    | Submit


mainForm =
    -- boundedInt
    -- simpleRecordInput
    -- simpleCustomTypeInput
    -- nestedRecordInput
    Item.item
        |> toForm "Creating an Item" FormMsg


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
        |> field .name "name" string
        |> field .age "age" boundedInt
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
        |> tag1 "red" Red boundedInt
        |> tag2 "green" Green "string" string "maybe int" (maybe int)
        |> tag0 "blue" Blue
        |> endCustomType
            (\tag ->
                case tag of
                    Red x ->
                        initWith1Arg atField0
                            ( boundedInt, x )

                    Green x y ->
                        initWith2Args atField1
                            ( string, x )
                            ( maybe int, y )

                    Blue ->
                        initWith0Args atField2
            )


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
        |> field .titles "number list" (list boundedInt)
        |> field .record "record" simpleRecordInput
        |> field .custom "custom type" simpleCustomTypeInput
        |> endRecord
