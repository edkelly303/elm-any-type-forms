module Item exposing (Item, ItemDelta, ItemState, exampleItem, item)

import Form exposing (..)
import Html as H
import Html.Attributes as HA
import Time


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


type Id a
    = Id Int


type ItemId
    = ItemId


type alias Ratio a b =
    ( Unit a, Unit b )


type Gram
    = Gram


type Millilitre
    = Millilitre


type PerItem
    = PerItem


type Unit a
    = Unit Int


type Measure
    = Weight
    | Volume
    | Whole
    | Custom String


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


type alias PurchaseHistory =
    { time : Time.Posix
    , grams : Unit Gram
    , measure : Measure
    }


type alias VolumeRecord =
    { ratio : Ratio Gram Millilitre }


type alias WholeRecord =
    { singularName : String
    , ratio : Ratio Gram PerItem
    }


type alias CustomRecord =
    { singularCollection : String
    , pluralCollection : String
    , ratio : Ratio Gram PerItem
    }


type alias Seasonality =
    { startsAt : Time.Month
    , endsAt : Time.Month
    }


exampleItem : Item
exampleItem =
    { id = Id 100
    , name = "Bakery bread"
    , aliases = [ "sourdough" ]
    , emoji = Just "🥖"
    , category = Baking
    , purchaseHistory = []
    , doNotSuggestUntil = Nothing
    , maybeVolume = Nothing
    , maybeWhole = Nothing
    , custom = [ { singularCollection = "Loaf", pluralCollection = "Loaves", ratio = ( Unit 800, Unit 1 ) } ]
    , maybeSeasonality = Nothing
    }



{-
   d88888b  .d88b.  d8888b. .88b  d88.       .o88b.  .d88b.  d8888b. d88888b
   88'     .8P  Y8. 88  `8D 88'YbdP`88      d8P  Y8 .8P  Y8. 88  `8D 88'
   88ooo   88    88 88oobY' 88  88  88      8P      88    88 88   88 88ooooo
   88~~~   88    88 88`8b   88  88  88      8b      88    88 88   88 88~~~~~
   88      `8b  d8' 88 `88. 88  88  88      Y8b  d8 `8b  d8' 88  .8D 88.
   YP       `Y88P'  88   YD YP  YP  YP       `Y88P'  `Y88P'  Y8888D' Y88888P


-}


type alias ItemState =
    States11
        IdState
        String
        (ListState String)
        (MaybeState String)
        CategoryState
        (ListState PurchaseHistoryState)
        (MaybeState String)
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
        (MaybeDelta String)
        (MaybeDelta VolumeRecordDelta)
        (MaybeDelta WholeRecordDelta)
        (ListDelta CustomRecordDelta)
        (MaybeDelta SeasonalityDelta)


item : Input ItemState ItemDelta Item
item =
    record Item
        |> field .id "Id" (itemId |> readOnly)
        |> field .name "Name" nonEmptyString
        |> field .aliases "Aliases" (list nonEmptyString)
        |> field .emoji "Emoji" (maybe emoji)
        |> field .category "Category" category
        |> field .purchaseHistory "Purchase History" (list purchaseHistory |> readOnly)
        |> field .doNotSuggestUntil "Do Not Suggest Until" (maybe datetime |> readOnly)
        |> field .maybeVolume "Volume" (maybe volumeRecord)
        |> field .maybeWhole "Whole" (maybe wholeRecord)
        |> field .custom "Custom" (list customRecord)
        |> field .maybeSeasonality "Seasonality" (maybe seasonality)
        |> endRecord
        |> initialise exampleItem


type alias IdState =
    WrapperState String


type alias IdDelta =
    WrapperDelta String


itemId : Input IdState IdDelta (Id ItemId)
itemId =
    wrapper Id (\(Id int) -> int) positiveInt


type alias UnitState =
    WrapperState String


type alias UnitDelta =
    WrapperDelta String


positiveInt : Input String String Int
positiveInt =
    int
        |> failIf (\x -> x < 1) "Must be greater than zero"


unit : Input UnitState UnitDelta (Unit a)
unit =
    positiveInt
        |> initialise 1
        |> wrapper Unit (\(Unit int) -> int)


type alias RatioState =
    TupleState UnitState UnitState


type alias RatioDelta =
    TupleDelta UnitDelta UnitDelta


ratio : String -> String -> Input RatioState RatioDelta (Ratio a b)
ratio fstId sndId =
    tuple fstId unit sndId unit
        |> layout
            (\children _ ->
                case children of
                    [ fst, snd ] ->
                        H.div
                            [ HA.style "display" "flex"
                            , HA.style "align-items" ""
                            ]
                            [ H.div [ HA.style "margin-right" "20px" ]
                                [ H.text fstId
                                , H.div [ HA.style "margin-top" "10px" ] [ fst ]
                                ]
                            , H.text "per"
                            , H.div [ HA.style "margin-left" "20px" ]
                                [ H.text sndId
                                , H.div [ HA.style "margin-top" "10px" ] [ snd ]
                                ]
                            ]

                    _ ->
                        H.text "ERROR"
            )


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
        |> tag0 "Weight" Weight
        |> tag0 "Volume" Volume
        |> tag0 "Whole" Whole
        |> tag1 "Custom" Custom string
        |> endCustomType
            (\output ->
                case output of
                    Weight ->
                        initWith0Args atField0

                    Volume ->
                        initWith0Args atField1

                    Whole ->
                        initWith0Args atField2

                    Custom c ->
                        initWith1Arg atField3
                            ( string, c )
            )


type alias CategoryState =
    EnumState Category


type alias CategoryDelta =
    EnumDelta Category


category : Input CategoryState CategoryDelta Category
category =
    enum
        ( "Miscellaneous", Miscellaneous )
        ( "Personal", Personal )
        [ ( "Dairy", Dairy )
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
        ( "Feb", Time.Feb )
        [ ( "Mar", Time.Mar )
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


nonEmptyString : Input String String String
nonEmptyString =
    string
        |> failIf String.isEmpty "Must not be blank"


emoji : Input String String String
emoji =
    string
        |> failIf (\x -> String.length x /= 1) "Must be exactly one character"


type alias PurchaseHistoryState =
    States3
        String
        UnitState
        MeasureState


type alias PurchaseHistoryDelta =
    Deltas3
        String
        UnitDelta
        MeasureDelta


purchaseHistory : Input PurchaseHistoryState PurchaseHistoryDelta PurchaseHistory
purchaseHistory =
    record PurchaseHistory
        |> field .time "Time of purchase" datetime
        |> field .grams "Amount purchased (grams)" unit
        |> field .measure "Purchased by measure" measure
        |> endRecord


type alias VolumeRecordState =
    States1 RatioState


type alias VolumeRecordDelta =
    Deltas1 RatioDelta


volumeRecord : Input VolumeRecordState VolumeRecordDelta VolumeRecord
volumeRecord =
    record VolumeRecord
        |> field .ratio "Density" (ratio "Grams" "Millilitre")
        |> endRecord


type alias WholeRecordState =
    States2 String RatioState


type alias WholeRecordDelta =
    Deltas2 String RatioDelta


wholeRecord : Input WholeRecordState WholeRecordDelta WholeRecord
wholeRecord =
    record WholeRecord
        |> field .singularName "Singular Name" nonEmptyString
        |> field .ratio "Weight" (ratio "Grams" "Item")
        |> endRecord


type alias CustomRecordState =
    States3 String String RatioState


type alias CustomRecordDelta =
    Deltas3 String String RatioDelta


customRecord : Input CustomRecordState CustomRecordDelta CustomRecord
customRecord =
    record CustomRecord
        |> field .singularCollection "Singular Collection" nonEmptyString
        |> field .pluralCollection "Plural Collection" nonEmptyString
        |> field .ratio "Weight" (ratio "Grams" "Item")
        |> endRecord


type alias SeasonalityState =
    States2 MonthState MonthState


type alias SeasonalityDelta =
    Deltas2 MonthDelta MonthDelta


seasonality : Input SeasonalityState SeasonalityDelta Seasonality
seasonality =
    record Seasonality
        |> field .startsAt "Starts At" month
        |> field .endsAt "Ends At" (month |> initialise Time.Dec)
        |> endRecord
