module Field exposing
    ( Delta(..)
    , DeltaContext
    , Error(..)
    , Field(..)
    , RendererConfig
    , State
    , Status(..)
    , custom
    , errorToString
    , floatMustBeGreaterThan
    , floatMustBeLessThan
    , initialize
    , intMustBeGreaterThan
    , intMustBeLessThan
    , stringMustBeLongerThan
    , stringMustBeShorterThan
    , stringMustNotContain
    , withCmd
    , withInitialState
    , withLabel
    , withRenderer
    , withValidator
    )

import Dict


type Delta delta
    = Delta DeltaContext delta


type alias DeltaContext =
    { cmdName : String, debounce : Float }


type Status
    = Intact
    | Idle
    | Changing
    | Loading


type Field input delta output element msg
    = Field
        { index : Int
        , init : input
        , deltaMsg : Delta delta -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , renderer : RendererConfig input delta output msg -> element
        , id : String
        , label : String
        , loadCmd : Dict.Dict String (input -> Cmd msg)
        }


type alias RendererConfig input delta output msg =
    { input : input
    , status : Status
    , focused : Bool
    , delta : delta -> msg
    , debouncedDelta : Float -> delta -> msg
    , effectfulDelta : String -> delta -> msg
    , debouncedEffectfulDelta : Float -> String -> delta -> msg
    , focusMsg : msg
    , requestCmdMsg : String -> msg
    , parsed : Result (List Error) output
    , label : String
    , id : String
    }


type alias State input output =
    { input : input, validated : Result (List Error) output }


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
    | NoDateSelected
    | Custom String


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

        NoDateSelected ->
            "No date selected"

        Custom str ->
            str



-- CREATING FIELDS


custom :
    { init : input
    , deltaMsg : Delta delta -> msg
    , updater : delta -> input -> input
    , parser : input -> Result Error output
    , renderer : RendererConfig input delta output msg -> element
    , label : String
    }
    -> Field input delta output element msg
custom { init, deltaMsg, updater, parser, renderer, label } =
    Field
        { index = 0
        , init = init
        , deltaMsg = deltaMsg
        , updater = updater
        , parser = parser
        , validators = []
        , renderer = renderer
        , id = ""
        , label = label
        , loadCmd = Dict.singleton "" (always Cmd.none)
        }



-- EXTRACTING STATE FROM FIELDS


initialize : Field input delta output element msg -> State input output
initialize (Field { init }) =
    { input = init, validated = Err [] }



-- BUILDING FIELDS


withInitialState : input -> Field input delta output element msg -> Field input delta output element msg
withInitialState input (Field f) =
    Field { f | init = input }


withLabel : String -> Field input delta output element msg -> Field input delta output element msg
withLabel l (Field f) =
    Field { f | label = l }


withCmd : String -> (result -> delta) -> (input -> Cmd result) -> Field input delta output element msg -> Field input delta output element msg
withCmd cmdName toDelta toReturn (Field f) =
    let
        toCmdMsg input =
            input
                |> toReturn
                |> Cmd.map toDelta
                |> Cmd.map (Delta { debounce = 0, cmdName = "" })
                |> Cmd.map f.deltaMsg
    in
    Field { f | loadCmd = Dict.insert cmdName toCmdMsg f.loadCmd }


withValidator : (output -> Maybe Error) -> Field input delta output element msg -> Field input delta output element msg
withValidator v (Field f) =
    Field { f | validators = v :: f.validators }


withRenderer :
    (RendererConfig input delta output msg -> element2)
    -> Field input delta output element msg
    -> Field input delta output element2 msg
withRenderer r (Field f) =
    Field
        { index = f.index
        , init = f.init
        , deltaMsg = f.deltaMsg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , renderer = r
        , id = f.id
        , label = f.label
        , loadCmd = f.loadCmd
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
