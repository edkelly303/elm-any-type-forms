module Field exposing
    ( Error(..)
    , Field(..)
    , State
    , custom
    , float
    , floatMustBeGreaterThan
    , floatMustBeLessThan
    , initialize
    , int
    , intMustBeGreaterThan
    , intMustBeLessThan
    , string
    , stringMustBeLongerThan
    , stringMustBeShorterThan
    , stringMustNotContain
    , withInitialState
    , withLabel
    , withRenderer
    , withValidator
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Internals exposing (..)


type Field input delta output element msg
    = Field
        { index : Int
        , init : input
        , msg : delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool
            , onBlurMsg : msg
            , onFocusMsg : msg
            , msg : delta -> msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
        , label : Maybe String
        }


type alias State input =
    { input : input }


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


custom :
    { a
        | init : input
        , msg : delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool
            , onBlurMsg : msg
            , onFocusMsg : msg
            , msg : delta -> msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
    }
    -> Field input delta output element msg
custom { init, msg, updater, parser, renderer, id } =
    Field
        { index = 0
        , init = init
        , msg = msg
        , updater = updater
        , parser = parser
        , validators = []
        , renderer = renderer
        , id = id
        , label = Nothing
        }


string : String -> (String -> msg) -> Field String String String (Element msg) msg
string id msg =
    custom
        { init = ""
        , msg = msg
        , updater = \delta _ -> delta
        , parser = Ok
        , renderer = toElement
        , id = id
        }


float : String -> (String -> msg) -> Field String String Float (Element msg) msg
float id msg =
    custom
        { init = ""
        , msg = msg
        , updater = \delta _ -> delta
        , parser = String.toFloat >> Result.fromMaybe NotValidFloat
        , renderer = toElement
        , id = id
        }


int : String -> (String -> msg) -> Field String String Int (Element msg) msg
int id msg =
    custom
        { init = ""
        , msg = msg
        , updater = \delta _ -> delta
        , parser = String.toInt >> Result.fromMaybe NotValidInt
        , renderer = toElement
        , id = id
        }



-- WORKING WITH FIELDS


initialize : Field input delta output element msg -> State input
initialize (Field { init }) =
    { input = init }


withLabel : String -> Field input delta output element msg -> Field input delta output element msg
withLabel l (Field f) =
    Field { f | label = Just l }


withValidator : (output -> Maybe Error) -> Field input delta output element msg -> Field input delta output element msg
withValidator v (Field f) =
    Field { f | validators = v :: f.validators }


withInitialState : input -> Field input delta output element msg -> Field input delta output element msg
withInitialState input (Field f) =
    Field { f | init = input }


withRenderer :
    ({ input : input
     , touched : Bool
     , focused : Bool
     , typing : Bool
     , onBlurMsg : msg
     , onFocusMsg : msg
     , msg : delta -> msg
     , parsed : Result (List Error) output
     , label : Maybe String
     , id : String
     }
     -> element2
    )
    -> Field input delta output element msg
    -> Field input delta output element2 msg
withRenderer r (Field f) =
    Field
        { index = f.index
        , init = f.init
        , msg = f.msg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , renderer = r
        , id = f.id
        , label = f.label
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


toElement :
    { onFocusMsg : msg
    , onBlurMsg : msg
    , input : String
    , touched : Bool
    , focused : Bool
    , typing : Bool
    , msg : String -> msg
    , parsed : Result (List Error) output
    , id : String
    , label : Maybe String
    }
    -> Element msg
toElement { input, touched, typing, onFocusMsg, onBlurMsg, focused, msg, parsed, id, label } =
    column
        [ spacing 10
        , padding 20
        , width fill
        , Background.color
            (if focused then
                rgb255 220 255 220

             else
                rgb255 255 255 255
            )
        , Border.rounded 5
        ]
        [ Input.text
            [ Background.color
                (case ( touched, parsed, typing ) of
                    ( True, Ok _, False ) ->
                        rgb255 100 255 100

                    ( True, Err _, False ) ->
                        rgb255 255 100 100

                    _ ->
                        rgb255 255 255 255
                )
            , htmlAttribute (Html.Attributes.id id)
            , Events.onFocus onFocusMsg
            , Events.onLoseFocus onBlurMsg
            ]
            { label = Input.labelAbove [ Font.size 18 ] (text (label |> Maybe.withDefault id))
            , text = input
            , placeholder = Nothing
            , onChange = msg
            }
        , if typing then
            text "..."

          else
            none
        , case ( touched, parsed, typing ) of
            ( True, Err errs, False ) ->
                column [ Font.size 12, spacing 5 ] (List.map (errorToString >> text) errs)

            _ ->
                none
        ]
