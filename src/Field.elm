module Field exposing
    ( Delta(..)
    , DeltaContext
    , Error(..)
    , Field(..)
    , State
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
    , withDebounce
    , withInitialState
    , withLabel
    , withLoadCmd
    , withRenderer
    , withValidator
    )


type Delta input delta msg
    = Delta (DeltaContext input msg) delta


type alias DeltaContext input msg =
    { cmd : input -> Cmd msg, debounce : Float }


type Field input delta output element msg
    = Field
        { index : Int
        , init : input
        , deltaMsg : Delta input delta msg -> msg
        , updater : delta -> input -> input
        , parser : input -> Result Error output
        , validators : List (output -> Maybe Error)
        , renderer :
            { input : input
            , touched : Bool
            , focused : Bool
            , typing : Bool
            , deltaMsg : Delta input delta msg -> msg
            , focusMsg : msg
            , requestCmdMsg : msg
            , parsed : Result (List Error) output
            , label : Maybe String
            , id : String
            }
            -> element
        , id : String
        , label : Maybe String
        , loadCmd : input -> Cmd msg
        , inputTimeout : Float
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
    , deltaMsg : Delta input delta msg -> msg
    , updater : delta -> input -> input
    , parser : input -> Result Error output
    , renderer :
        { input : input
        , touched : Bool
        , focused : Bool
        , typing : Bool
        , requestCmdMsg : msg
        , focusMsg : msg
        , deltaMsg : Delta input delta msg -> msg
        , parsed : Result (List Error) output
        , label : Maybe String
        , id : String
        }
        -> element
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
        , label = Just label
        , loadCmd = always Cmd.none
        , inputTimeout = 0
        }



-- EXTRACTING STATE FROM FIELDS


initialize : Field input delta output element msg -> State input
initialize (Field { init }) =
    { input = init }



-- BUILDING FIELDS


withInitialState : input -> Field input delta output element msg -> Field input delta output element msg
withInitialState input (Field f) =
    Field { f | init = input }


withLabel : String -> Field input delta output element msg -> Field input delta output element msg
withLabel l (Field f) =
    Field { f | label = Just l }


withDebounce : Float -> Field input delta output element msg -> Field input delta output element msg
withDebounce timeout (Field f) =
    Field { f | inputTimeout = timeout }


withLoadCmd : (input -> Cmd msg) -> Field input delta output element msg -> Field input delta output element msg
withLoadCmd loadCmd (Field f) =
    Field { f | loadCmd = loadCmd }


withValidator : (output -> Maybe Error) -> Field input delta output element msg -> Field input delta output element msg
withValidator v (Field f) =
    Field { f | validators = v :: f.validators }


withRenderer :
    ({ input : input
     , touched : Bool
     , focused : Bool
     , typing : Bool
     , requestCmdMsg : msg
     , focusMsg : msg
     , deltaMsg : Delta input delta msg -> msg
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
        , deltaMsg = f.deltaMsg
        , updater = f.updater
        , parser = f.parser
        , validators = f.validators
        , renderer = r
        , id = f.id
        , label = f.label
        , loadCmd = f.loadCmd
        , inputTimeout = f.inputTimeout
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
