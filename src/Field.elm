module Field exposing (..)

import Internals exposing (..)


type Error
    = NotValidInt
    | NotValidFloat
    | IntTooLow
    | IntTooHigh
    | FloatTooLow
    | FloatTooHigh
    | StringTooShort
    | StringTooLong
    | StringMustNotContain String


type Field input output
    = Field
        { data : input
        , touched : Bool
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        }



-- CREATING FIELDS


stringToString : String -> List (String -> Maybe Error) -> Field String String
stringToString data validators =
    Field { data = data, touched = False, parser = Ok, validators = validators }


stringToFloat : String -> List (Float -> Maybe Error) -> Field String Float
stringToFloat data validators =
    Field { data = data, touched = False, parser = String.toFloat >> Result.fromMaybe NotValidFloat, validators = validators }


stringToInt : String -> List (Int -> Maybe Error) -> Field String Int
stringToInt data validators =
    Field { data = data, touched = False, parser = String.toInt >> Result.fromMaybe NotValidInt, validators = validators }



-- CREATING VALIDATORS


ifTrueThenJust : a -> Bool -> Maybe a
ifTrueThenJust a bool =
    if bool then
        Just a

    else
        Nothing


stringMustBeLongerThan : Int -> String -> Maybe Error
stringMustBeLongerThan numberOfChars str =
    (String.length str <= numberOfChars)
        |> ifTrueThenJust StringTooShort


stringMustBeShorterThan : Int -> String -> Maybe Error
stringMustBeShorterThan numberOfChars str =
    (String.length str >= numberOfChars)
        |> ifTrueThenJust StringTooLong


stringMustNotContain : String -> String -> Maybe Error
stringMustNotContain content str =
    String.contains content str
        |> ifTrueThenJust (StringMustNotContain content)


intMustBeGreaterThan : Int -> Int -> Maybe Error
intMustBeGreaterThan tooLow int_ =
    (int_ <= tooLow)
        |> ifTrueThenJust IntTooLow


intMustBeLessThan : Int -> Int -> Maybe Error
intMustBeLessThan tooHigh int_ =
    (int_ >= tooHigh)
        |> ifTrueThenJust IntTooHigh


floatMustBeGreaterThan : Float -> Float -> Maybe Error
floatMustBeGreaterThan tooLow float_ =
    (float_ <= tooLow)
        |> ifTrueThenJust FloatTooLow


floatMustBeLessThan : Float -> Float -> Maybe Error
floatMustBeLessThan tooHigh float_ =
    (float_ >= tooHigh)
        |> ifTrueThenJust FloatTooHigh



