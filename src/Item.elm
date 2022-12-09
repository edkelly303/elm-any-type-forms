module Item exposing (Item, ItemDelta, ItemState, item)

import Form exposing (..)
import Time


type Id a
    = Id Int


type ItemId
    = ItemId


type alias IdState =
    WrapperState String


type alias IdDelta =
    WrapperDelta String


itemId : Input IdState IdDelta (Id ItemId)
itemId =
    wrapper Id positiveInt


type Gram
    = Gram


type Millilitre
    = Millilitre


type PerItem
    = PerItem


type Unit a
    = Unit Int


type alias UnitState =
    WrapperState String


type alias UnitDelta =
    WrapperDelta String


unit : Input UnitState UnitDelta (Unit a)
unit =
    wrapper Unit nonNegativeInt


nonNegativeInt : Input String String Int
nonNegativeInt =
    int |> failIf (\x -> x < 0) "Must be zero or greater"


positiveInt : Input String String Int
positiveInt =
    int
        |> failIf (\x -> x < 1) "Must be a positive integer"
        |> noteIf (\x -> x > 199) "Wow!"


type alias Ratio a b =
    ( Unit a, Unit b )


type alias RatioState =
    TupleState UnitState UnitState


type alias RatioDelta =
    TupleDelta UnitDelta UnitDelta


ratio : String -> String -> Input RatioState RatioDelta (Ratio a b)
ratio fstId sndId =
    tuple fstId unit sndId unit


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
        |> tag0 i0 "Weight" Weight
        |> tag0 i1 "Volume" Volume
        |> tag0 i2 "Whole" Whole
        |> tag1 i3 "Custom" Custom "Name of Custom Measure" string
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
    enum
        ( "Miscellaneous", Miscellaneous )
        [ ( "Personal", Personal )
        , ( "Dairy", Dairy )
        , ( "Chilled", Chilled )
        , ( "Fruit and Veg", FruitAndVeg )
        , ( "Meat", Meat )
        , ( "Fish", Fish )
        , ( "Non-perishables", NonPerishables )
        , ( "Gluten-free", GlutenFree )
        , ( "Snacks", Snacks )
        , ( "Cleaning", Cleaning )
        , ( "Frozen", Frozen )
        , ( "Baking", Baking )
        , ( "Drinks", Drinks )
        ]


type alias MonthState =
    EnumState Time.Month


type alias MonthDelta =
    EnumDelta Time.Month


month : Input MonthState MonthDelta Time.Month
month =
    enum
        ( "Jan", Time.Jan )
        [ ( "Feb", Time.Feb )
        , ( "Mar", Time.Mar )
        , ( "Apr", Time.Apr )
        , ( "May", Time.May )
        , ( "Jun", Time.Jun )
        , ( "Jul", Time.Jul )
        , ( "Aug", Time.Aug )
        , ( "Sep", Time.Sep )
        , ( "Oct", Time.Oct )
        , ( "Nov", Time.Nov )
        , ( "Dec", Time.Dec )
        ]


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
        |> field i0 "Id" itemId
        |> field i1 "Name" (string |> debounce 1000)
        |> field i2 "Aliases" (list string)
        |> field i3 "Emoji" (maybe emoji)
        |> field i4 "Category" category
        |> field i5 "Purchase History" (list purchaseHistory)
        |> field i6 "Do Not Suggest Until" (maybe posix)
        |> field i7 "Volume" (maybe volumeRecord)
        |> field i8 "Whole" (maybe wholeRecord)
        |> field i9 "Custom" (list customRecord)
        |> field i10 "Seasonality" (maybe seasonality)
        |> endRecord


emoji : Input String String String
emoji =
    string
        |> failIf (\x -> String.length x /= 1) "Must be exactly one character"


type alias PosixState =
    States1 String


type alias PosixDelta =
    Deltas1 String


posix : Input PosixState PosixDelta Time.Posix
posix =
    record Time.millisToPosix
        |> field i0 "" positiveInt
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


purchaseHistory : Input PurchaseHistoryState PurchaseHistoryDelta PurchaseHistory
purchaseHistory =
    record PurchaseHistory
        |> field i0 "Time" posix
        |> field i1 "Grams" unit
        |> field i2 "Measure" measure
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
        |> field i0 "Ratio" (ratio "Grams" "Millilitres")
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
        |> field i0 "Singular Name" string
        |> field i1 "Ratio" (ratio "Grams" "Per Item")
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
        |> field i0 "Singular Collection" string
        |> field i1 "Plural Collection" string
        |> field i2 "Ratio" (ratio "Grams" "Per Item")
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
        |> field i0 "Starts At" month
        |> field i1 "Ends At" month
        |> endRecord
