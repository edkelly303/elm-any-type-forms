module Item exposing (Item, ItemDelta, ItemState, item)

import Form exposing (..)
import Time


type Id a
    = Id Int


type ItemId
    = ItemId


type alias IdState =
    CustomTypeState (States1 (States1 String))


type alias IdDelta =
    CustomTypeDelta (Deltas1 (Deltas1 String))


itemId : Input IdState IdDelta (Id ItemId)
itemId =
    customType
        |> tag1 i0 "id" Id "int" positiveInt
        |> endCustomType


type Gram
    = Gram


type Millilitre
    = Millilitre


type PerItem
    = PerItem


type Unit a
    = Unit Int


type alias UnitState =
    CustomTypeState (States1 (States1 String))


type alias UnitDelta =
    CustomTypeDelta (Deltas1 (Deltas1 String))


unit : Input UnitState UnitDelta (Unit a)
unit =
    customType
        |> tag1 i0 "unit" Unit "int" positiveInt
        |> endCustomType


positiveInt : Input String String Int
positiveInt =
    intConfig
        |> failIf (\x -> x < 1) "must be a positive integer"
        |> fromConfig


type alias Ratio a b =
    ( Unit a, Unit b )


type alias RatioState =
    TupleState UnitState UnitState


type alias RatioDelta =
    TupleDelta UnitDelta UnitDelta


ratio : Input RatioState RatioDelta (Ratio a b)
ratio =
    tuple unit unit


type Measure
    = Weight
    | Volume
    | Whole
    | Custom String


type alias MeasureState =
    CustomTypeState
        (States4
            States0
            States0
            States0
            (States1 String)
        )


type alias MeasureDelta =
    CustomTypeDelta
        (Deltas4
            Deltas0
            Deltas0
            Deltas0
            (Deltas1 String)
        )


measure : Input MeasureState MeasureDelta Measure
measure =
    customType
        |> tag0 i0 "weight" Weight
        |> tag0 i1 "volume" Volume
        |> tag0 i2 "whole" Whole
        |> tag1 i3 "custom" Custom "string" string
        |> endCustomType


type alias CategoryState =
    EnumState Category


type alias CategoryDelta =
    EnumDelta Category


type Category
    = Miscellaneous
    | Personal
    | Dairy
    | Chilled
    | FruitAndVeg
    | Meat
    | Fish
    | NonPerishables
    | GlutenFree
    | Snacks
    | Cleaning
    | Frozen
    | Baking
    | Drinks


category : Input CategoryState CategoryDelta Category
category =
    enumConfig
        ( "miscellaneous", Miscellaneous )
        [ ( "personal", Personal )
        , ( "dairy", Dairy )
        , ( "chilled", Chilled )
        , ( "fruit and veg", FruitAndVeg )
        , ( "meat", Meat )
        , ( "fish", Fish )
        , ( "non-perishables", NonPerishables )
        , ( "gluten-free", GlutenFree )
        , ( "snacks", Snacks )
        , ( "cleaning", Cleaning )
        , ( "frozen", Frozen )
        , ( "baking", Baking )
        , ( "drinks", Drinks )
        ]
        |> fromConfig


type alias MonthState =
    EnumState Time.Month


type alias MonthDelta =
    EnumDelta Time.Month


month : Input MonthState MonthDelta Time.Month
month =
    enumConfig
        ( "jan", Time.Jan )
        [ ( "feb", Time.Feb )
        , ( "mar", Time.Mar )
        , ( "apr", Time.Apr )
        , ( "may", Time.May )
        , ( "jun", Time.Jun )
        , ( "jul", Time.Jul )
        , ( "aug", Time.Aug )
        , ( "sep", Time.Sep )
        , ( "oct", Time.Oct )
        , ( "nov", Time.Nov )
        , ( "dec", Time.Dec )
        ]
        |> fromConfig


type alias Item =
    { id : Id ItemId
    , name : String
    , aliases : List String
    , emoji : Maybe String
    , category : Category
    , purchaseHistory : List PurchaseHistory
    , doNotSuggestUntil : Maybe Time.Posix
    , maybeVolume : Maybe VolumeRecord
    , maybeWhole : Maybe WholeRecord
    , custom : List CustomRecord
    , maybeSeasonality : Maybe Seasonality
    }


type alias ItemState =
    States11
        IdState
        String
        (ListState String)
        (MaybeState String)
        CategoryState
        (ListState PurchaseHistoryState)
        (MaybeState PosixState)
        (MaybeState VolumeRecordState)
        (MaybeState WholeRecordState)
        (ListState CustomRecordState)
        (MaybeState SeasonalityState)


type alias ItemDelta =
    Deltas11
        IdDelta
        String
        (ListDelta String)
        (MaybeDelta String)
        CategoryDelta
        (ListDelta PurchaseHistoryDelta)
        (MaybeDelta PosixDelta)
        (MaybeDelta VolumeRecordDelta)
        (MaybeDelta WholeRecordDelta)
        (ListDelta CustomRecordDelta)
        (MaybeDelta SeasonalityDelta)


item : Input ItemState ItemDelta Item
item =
    record Item
        |> field i0 "id" itemId
        |> field i1 "name" string
        |> field i2 "aliases" (list string)
        |> field i3 "emoji" (maybe emoji)
        |> field i4 "category" category
        |> field i5 "purchase history" (list purchaseHistory)
        |> field i6 "doNotSuggestUntil" (maybe posix)
        |> field i7 "maybeVolume" (maybe volumeRecord)
        |> field i8 "maybeWhole" (maybe wholeRecord)
        |> field i9 "custom" (list customRecord)
        |> field i10 "maybeSeasonality" (maybe seasonality)
        |> endRecord


emoji =
    stringConfig
        |> failIf (\x -> String.length x /= 1) "must be exactly one character"
        |> fromConfig


type alias PosixState =
    States1 String


type alias PosixDelta =
    Deltas1 String


posix : Input PosixState PosixDelta Time.Posix
posix =
    record Time.millisToPosix
        |> field i0 "posix timestamp" positiveInt
        |> endRecord


type alias PurchaseHistory =
    { time : Time.Posix
    , grams : Unit Gram
    , measure : Measure
    }


type alias PurchaseHistoryState =
    States3
        PosixState
        UnitState
        MeasureState


type alias PurchaseHistoryDelta =
    Deltas3
        PosixDelta
        UnitDelta
        MeasureDelta


purchaseHistory :
    Input
        PurchaseHistoryState
        PurchaseHistoryDelta
        PurchaseHistory
purchaseHistory =
    record PurchaseHistory
        |> field i0 "time" posix
        |> field i1 "grams" unit
        |> field i2 "measure" measure
        |> endRecord


type alias VolumeRecord =
    { ratio : Ratio Gram Millilitre }


type alias VolumeRecordState =
    States1 RatioState


type alias VolumeRecordDelta =
    Deltas1 RatioDelta


volumeRecord : Input VolumeRecordState VolumeRecordDelta VolumeRecord
volumeRecord =
    record VolumeRecord
        |> field i0 "ratio" ratio
        |> endRecord


type alias WholeRecord =
    { singularName : String
    , ratio : Ratio Gram PerItem
    }


type alias WholeRecordState =
    States2 String RatioState


type alias WholeRecordDelta =
    Deltas2 String RatioDelta


wholeRecord : Input WholeRecordState WholeRecordDelta WholeRecord
wholeRecord =
    record WholeRecord
        |> field i0 "singularName" string
        |> field i1 "ratio" ratio
        |> endRecord


type alias CustomRecord =
    { singularCollection : String
    , pluralCollection : String
    , ratio : Ratio Gram PerItem
    }


type alias CustomRecordState =
    States3 String String RatioState


type alias CustomRecordDelta =
    Deltas3 String String RatioDelta


customRecord : Input CustomRecordState CustomRecordDelta CustomRecord
customRecord =
    record CustomRecord
        |> field i0 "singularCollection" string
        |> field i1 "pluralCollection" string
        |> field i2 "ratio" ratio
        |> endRecord


type alias Seasonality =
    { startsAt : Time.Month
    , endsAt : Time.Month
    }


type alias SeasonalityState =
    States2 MonthState MonthState


type alias SeasonalityDelta =
    Deltas2 MonthDelta MonthDelta


seasonality : Input SeasonalityState SeasonalityDelta Seasonality
seasonality =
    record Seasonality
        |> field i0 "startsAt" month
        |> field i1 "endsAt" month
        |> endRecord
