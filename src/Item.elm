module Item exposing
    ( Category
    , Item
    , ItemDelta
    , ItemState
    , exampleItem
    , form
    , item
    )

import Form exposing (..)
import Html as H
import Html.Attributes as HA
import Time exposing (millisToPosix)


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
    , purchaseHistory = [ { grams = Unit 100, measure = Custom "box", time = millisToPosix 0 } ]
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


type alias ItemDelta =
    ( Delta (WrapperDelta String)
    , ( Delta String
      , ( Delta (ListDelta String)
        , ( Delta (MaybeDelta String)
          , ( Delta Category
            , ( Delta
                    (ListDelta
                        ( Delta String
                        , ( Delta (WrapperDelta String)
                          , ( Delta
                                ( Delta ()
                                , ( Delta ()
                                  , ( Delta ()
                                    , ( Delta ( Delta String, End )
                                      , End
                                      )
                                    )
                                  )
                                )
                            , End
                            )
                          )
                        )
                    )
              , ( Delta (MaybeDelta String)
                , ( Delta
                        (MaybeDelta
                            ( Delta
                                (TupleDelta
                                    (WrapperDelta String)
                                    (WrapperDelta String)
                                )
                            , End
                            )
                        )
                  , ( Delta
                        (MaybeDelta
                            ( Delta String
                            , ( Delta
                                    (TupleDelta
                                        (WrapperDelta String)
                                        (WrapperDelta String)
                                    )
                              , End
                              )
                            )
                        )
                    , ( Delta
                            (ListDelta
                                ( Delta String
                                , ( Delta String
                                  , ( Delta
                                        (TupleDelta
                                            (WrapperDelta String)
                                            (WrapperDelta String)
                                        )
                                    , End
                                    )
                                  )
                                )
                            )
                      , ( Delta
                            (MaybeDelta
                                ( Delta Time.Month
                                , ( Delta Time.Month, End )
                                )
                            )
                        , End
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


type alias ItemState =
    ( State (WrapperState String)
    , ( State String
      , ( State (List (State String))
        , ( State (MaybeState String)
          , ( State Category
            , ( State
                    (List
                        (State
                            ( State String
                            , ( State (WrapperState String)
                              , ( State
                                    { selectedTag : Int
                                    , tagStates :
                                        ( State ()
                                        , ( State ()
                                          , ( State ()
                                            , ( State
                                                    ( State String
                                                    , End
                                                    )
                                              , End
                                              )
                                            )
                                          )
                                        )
                                    }
                                , End
                                )
                              )
                            )
                        )
                    )
              , ( State (MaybeState String)
                , ( State
                        (MaybeState
                            ( State
                                (TupleState
                                    (WrapperState String)
                                    (WrapperState String)
                                )
                            , End
                            )
                        )
                  , ( State
                        (MaybeState
                            ( State String
                            , ( State
                                    (TupleState
                                        (WrapperState String)
                                        (WrapperState String)
                                    )
                              , End
                              )
                            )
                        )
                    , ( State
                            (List
                                (State
                                    ( State String
                                    , ( State String
                                      , ( State
                                            (TupleState
                                                (WrapperState String)
                                                (WrapperState String)
                                            )
                                        , End
                                        )
                                      )
                                    )
                                )
                            )
                      , ( State
                            (MaybeState
                                ( State Time.Month
                                , ( State Time.Month, End )
                                )
                            )
                        , End
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


form msg =
    fromControl "Create an Item" msg item


item =
    record Item
        |> hiddenField "Id" .id itemId
        |> field "Name" .name nonEmptyString
        |> field "Aliases" .aliases (list nonEmptyString)
        |> field "Emoji" .emoji (maybe emoji)
        |> field "Category" .category category
        |> field "Purchase History " .purchaseHistory (list purchaseHistory)
        |> field "Do Not  Suggest Until" .doNotSuggestUntil (maybe datetime)
        |> field "Volume" .maybeVolume (maybe volumeRecord)
        |> field "Whole" .maybeWhole (maybe wholeRecord)
        |> field "Custom" .custom (list customRecord)
        |> field "Seasonality" .maybeSeasonality (maybe seasonality)
        |> end


itemId =
    wrapper Id (\(Id int) -> int) positiveInt


positiveInt =
    int
        |> failIf (\x -> x < 1) "Must be greater than zero"


unit =
    positiveInt
        |> initWith 1
        |> wrapper Unit (\(Unit int) -> int)


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


measure =
    customType
        (\weight volume whole custom output ->
            case output of
                Weight ->
                    weight

                Volume ->
                    volume

                Whole ->
                    whole

                Custom str ->
                    custom str
        )
        |> tag0 "Weight" Weight
        |> tag0 "Volume" Volume
        |> tag0 "Whole" Whole
        |> tag1 "Custom" Custom string
        |> end


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


nonEmptyString =
    string
        |> failIf String.isEmpty "Must not be blank"


emoji =
    string
        |> failIf (\x -> String.length x /= 1) "Must be exactly one character"


purchaseHistory =
    record PurchaseHistory
        |> field "Time of purchase" .time datetime
        |> field "Amount purchased (grams)" .grams unit
        |> field "Purchased by measure" .measure measure
        |> end


volumeRecord =
    record VolumeRecord
        |> field "Density" .ratio (ratio "Grams" "Millilitre")
        |> end


wholeRecord =
    record WholeRecord
        |> field "Singular Name" .singularName nonEmptyString
        |> field "Weight" .ratio (ratio "Grams" "Item")
        |> end


customRecord =
    record CustomRecord
        |> field "Singular Collection" .singularCollection nonEmptyString
        |> field "Plural Collection" .pluralCollection nonEmptyString
        |> field "Weight" .ratio (ratio "Grams" "Item")
        |> end


seasonality =
    record Seasonality
        |> field "Starts At" .startsAt month
        |> field "Ends At" .endsAt (month |> initWith Time.Dec)
        |> end
