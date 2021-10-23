module Field exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
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


type Field input output msg
    = Field
        { data : input
        , touched : Bool
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , toEl : input -> Bool -> (input -> msg) -> Result (List Error) output -> Element msg
        , msg : input -> msg
        }


type alias FieldState input =
    { data : input, touched : Bool }



-- CREATING FIELDS


custom : { a | data : b, parser : b -> Result Error output, validators : List (output -> Maybe Error), toEl : b -> Bool -> (b -> msg) -> Result (List Error) output -> Element msg, msg : b -> msg } -> Field b output msg
custom { data, parser, validators, toEl, msg } =
    Field
        { data = data
        , touched = False
        , parser = parser
        , validators = validators
        , toEl = toEl
        , msg = msg
        }


string : String -> List (String -> Maybe Error) -> (String -> msg) -> Field String String msg
string data validators msg =
    custom
        { data = data
        , parser = Ok
        , validators = validators
        , toEl = toEl_
        , msg = msg
        }


float : String -> List (Float -> Maybe Error) -> (String -> msg) -> Field String Float msg
float data validators msg =
    custom
        { data = data
        , parser = String.toFloat >> Result.fromMaybe NotValidFloat
        , validators = validators
        , toEl = toEl_
        , msg = msg
        }


int : String -> List (Int -> Maybe Error) -> (String -> msg) -> Field String Int msg
int data validators msg =
    custom
        { data = data
        , parser = String.toInt >> Result.fromMaybe NotValidInt
        , validators = validators
        , toEl = toEl_
        , msg = msg
        }



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


toEl_ : String -> Bool -> (String -> msg) -> Result (List Error) output -> Element msg
toEl_ input touched msg res =
    Input.text
        [ Background.color
            (case ( touched, res ) of
                ( True, Ok _ ) ->
                    rgb255 100 255 100

                ( True, Err _ ) ->
                    rgb255 255 100 100

                _ ->
                    rgb255 255 255 255
            )
        ]
        { label = Input.labelHidden "TODO"
        , text = input
        , placeholder = Nothing
        , onChange = msg
        }
