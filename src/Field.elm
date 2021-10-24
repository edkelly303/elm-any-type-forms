module Field exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Internals exposing (..)


type Field input output msg
    = Field
        { init : input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , renderer :
            { input : input
            , touched : Bool
            , msg : input -> msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> Element msg
        , msg : input -> msg
        , id : String
        , label : Maybe String
        }


type alias State input =
    { input : input
    , touched : Bool
    }


type Error
    = NotValidInt
    | NotValidFloat
    | IntTooLow Int
    | IntTooHigh Int
    | FloatTooLow Float
    | FloatTooHigh Float
    | StringTooShort Int
    | StringTooLong Int
    | StringMustNotContain String


errorToString : Error -> String
errorToString e =
    case e of
        NotValidInt ->
            "Must be a whole number"

        NotValidFloat ->
            "Must be a number"

        IntTooLow i ->
            "Must be a whole number higher than " ++ String.fromInt i

        IntTooHigh i ->
            "Must be a whole number lower than " ++ String.fromInt i

        FloatTooLow f ->
            "Must be a number higher than " ++ String.fromFloat f

        FloatTooHigh f ->
            "Must be a number lower than " ++ String.fromFloat f

        StringTooShort i ->
            "Must be longer than " ++ String.fromInt i ++ " characters"

        StringTooLong i ->
            "Must be shorter than " ++ String.fromInt i ++ " characters"

        StringMustNotContain str ->
            "Must not contain '" ++ str ++ "'"



-- CREATING FIELDS


custom : { a | init : b, parser : b -> Result Error output, renderer : { input : b, touched : Bool, msg : b -> msg, parsed : Result (List Error) output, label : Maybe String, id : String } -> Element msg, msg : b -> msg, id : String } -> Field b output msg
custom { init, parser, renderer, msg, id } =
    Field
        { id = id
        , msg = msg
        , parser = parser
        , validators = []
        , renderer = renderer
        , init = init
        , label = Nothing
        }


string : String -> (String -> msg) -> Field String String msg
string id msg =
    custom
        { init = ""
        , parser = Ok
        , renderer = toElement
        , msg = msg
        , id = id
        }


float : String -> (String -> msg) -> Field String Float msg
float id msg =
    custom
        { init = ""
        , parser = String.toFloat >> Result.fromMaybe NotValidFloat
        , renderer = toElement
        , msg = msg
        , id = id
        }


int : String -> (String -> msg) -> Field String Int msg
int id msg =
    custom
        { init = ""
        , parser = String.toInt >> Result.fromMaybe NotValidInt
        , renderer = toElement
        , msg = msg
        , id = id
        }



-- WORKING WITH FIELDS


initialize : Field input output msg -> State input
initialize (Field { init }) =
    { input = init, touched = False }


withLabel : String -> Field input output msg -> Field input output msg
withLabel l (Field f) =
    Field { f | label = Just l }


withValidator : (output -> Maybe Error) -> Field input output msg -> Field input output msg
withValidator v (Field f) =
    Field { f | validators = v :: f.validators }


withInitialState : input -> Field input output msg -> Field input output msg
withInitialState input (Field f) =
    Field { f | init = input }



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
        |> ifTrueThenJust (StringTooShort numberOfChars)


stringMustBeShorterThan : Int -> String -> Maybe Error
stringMustBeShorterThan numberOfChars str =
    (String.length str >= numberOfChars)
        |> ifTrueThenJust (StringTooLong numberOfChars)


stringMustNotContain : String -> String -> Maybe Error
stringMustNotContain content str =
    String.contains content str
        |> ifTrueThenJust (StringMustNotContain content)


intMustBeGreaterThan : Int -> Int -> Maybe Error
intMustBeGreaterThan tooLow int_ =
    (int_ <= tooLow)
        |> ifTrueThenJust (IntTooLow tooLow)


intMustBeLessThan : Int -> Int -> Maybe Error
intMustBeLessThan tooHigh int_ =
    (int_ >= tooHigh)
        |> ifTrueThenJust (IntTooHigh tooHigh)


floatMustBeGreaterThan : Float -> Float -> Maybe Error
floatMustBeGreaterThan tooLow float_ =
    (float_ <= tooLow)
        |> ifTrueThenJust (FloatTooLow tooLow)


floatMustBeLessThan : Float -> Float -> Maybe Error
floatMustBeLessThan tooHigh float_ =
    (float_ >= tooHigh)
        |> ifTrueThenJust (FloatTooHigh tooHigh)


toElement : { input : String, touched : Bool, msg : String -> msg, parsed : Result (List Error) output, id : String, label : Maybe String } -> Element msg
toElement { input, touched, msg, parsed, id, label } =
    column [ spacing 5 ]
        [ Input.text
            [ Background.color
                (case ( touched, parsed ) of
                    ( True, Ok _ ) ->
                        rgb255 100 255 100

                    ( True, Err _ ) ->
                        rgb255 255 100 100

                    _ ->
                        rgb255 255 255 255
                )
            , htmlAttribute (Html.Attributes.id id)
            ]
            { label = Input.labelAbove [Font.size 18] (text (label |> Maybe.withDefault id))
            , text = input
            , placeholder = Nothing
            , onChange = msg
            }
        , case ( touched, parsed ) of
            ( True, Err errs ) ->
                column [ Font.size 12, spacing 5 ] (List.map (errorToString >> text) errs)

            _ ->
                none
        ]
