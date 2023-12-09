module Studio exposing (Debug, Model, Msg(..), Page(..), stylesheet, view)

import DebugParser
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Parser as P exposing ((|.))
import Path


type alias Debug state delta output =
    ( Maybe state, Maybe delta, Maybe output )


type alias Metadata =
    { label : String
    , class : List String
    , id : Maybe String
    }


type alias Model state delta =
    { css : String
    , oldState : state
    , newState : state
    , page : Page
    , deltas : List delta
    }


type alias Feedback =
    { fail : Bool
    , path : Path.Path
    , label : String
    , message : String
    }


type Page
    = Debug
    | Style
    | Deploy


type Msg delta
    = FormChanged delta
    | CssChanged String
    | PageChanged Page
    | TimeTravelled Int


view :
    { debugToString : Debug state delta output -> String
    , metadata : Path.Dict Metadata
    , outputParsingResult : Result (List Feedback) output
    , validationErrors : List Feedback
    , oldState : state
    , newState : state
    , deltas : List delta
    }
    -> H.Html (Msg delta)
view { debugToString, metadata, outputParsingResult, validationErrors, oldState, newState, deltas } =
    let
        viewErrors errs =
            H.pre []
                [ H.p [] [ H.text "Failure! Your form has errors on the following controls:" ]
                , H.ul []
                    (errs
                        |> List.sortWith (\feedback1 feedback2 -> Path.compare feedback1.path feedback2.path)
                        |> List.map
                            (\feedback ->
                                H.li [] [ H.text (Path.toString feedback.path ++ ": " ++ feedback.label ++ "\n"), H.text feedback.message ]
                            )
                    )
                ]
    in
    H.div [ HA.id "studio-debug-container" ]
        [ H.div [ HA.id "studio-debug-state" ]
            [ H.h3 [] [ H.text "State" ]
            , H.pre [ HA.id "studio-debug-state-list" ]
                (stateToHtml debugToString metadata oldState newState)
            ]
        , H.div [ HA.id "studio-debug-deltas-and-output" ]
            [ H.div [ HA.id "studio-debug-deltas" ]
                [ H.h3 [] [ H.text "Deltas" ]
                , H.pre [ HA.id "studio-debug-deltas-list" ]
                    (case deltas of
                        [] ->
                            [ H.text "[ no deltas ]" ]

                        _ ->
                            List.indexedMap (\idx delta -> deltaToHtml idx metadata debugToString delta) deltas
                    )
                ]
            , H.div [ HA.id "studio-debug-output" ]
                [ H.h3 [] [ H.text "Output" ]
                , H.pre []
                    (case ( outputParsingResult, validationErrors ) of
                        ( Ok output, [] ) ->
                            [ H.p [] [ H.text "Success! Your form produced this value:" ]
                            , H.p [] [ outputToHtml debugToString output ]
                            ]

                        ( Err pErrs, vErrs ) ->
                            [ viewErrors (pErrs ++ vErrs) ]

                        ( _, vErrs ) ->
                            [ viewErrors vErrs ]
                    )
                ]
            ]
        ]


stateToHtml : (Debug state delta output -> String) -> Path.Dict Metadata -> state -> state -> List (H.Html msg)
stateToHtml debugToString metadata oldState newState =
    let
        oldParsed =
            ( Just oldState, Nothing, Nothing )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig

        newParsed =
            ( Just newState, Nothing, Nothing )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig
    in
    case ( oldParsed, newParsed ) of
        ( Ok (Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "Just" [ oldParsedState ]), _, _ ])), Ok (Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "Just" [ newParsedState ]), _, _ ])) ) ->
            parsedStateToHtml metadata oldParsedState newParsedState

        _ ->
            []


deltaToHtml : Int -> Path.Dict Metadata -> (Debug state delta output -> String) -> delta -> H.Html (Msg delta)
deltaToHtml idx metadata debugToString delta =
    let
        parsed =
            ( Nothing, Just delta, Nothing )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig
    in
    case parsed of
        Ok (Expandable _ (ElmSequence SeqTuple [ _, Expandable _ (ElmType "Just" [ Expandable _ (ElmType _ [ parsedDelta ]) ]), _ ])) ->
            H.button
                [ HE.onClick (TimeTravelled idx), HA.class "studio-debug-delta-button" ]
                (parsedDeltaToHtml metadata parsedDelta)

        _ ->
            H.text "ERROR!"


outputToHtml : (Debug state delta output -> String) -> output -> H.Html msg
outputToHtml debugToString output =
    let
        parsed =
            ( Nothing, Nothing, Just output )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig
    in
    case parsed of
        Ok (Expandable _ (ElmSequence SeqTuple [ _, _, Expandable _ (ElmType "Just" [ parsedOutput ]) ])) ->
            H.text (elmValueToString parsedOutput)

        _ ->
            H.text "ERROR!"


elmValueToString : ElmValue -> String
elmValueToString elmValue =
    case elmValue of
        Plain (ElmString s) ->
            "\"" ++ s ++ "\""

        Plain (ElmChar c) ->
            String.fromChar c

        Plain (ElmNumber f) ->
            String.fromFloat f

        Plain (ElmBool b) ->
            if b then
                "True"

            else
                "False"

        Plain ElmFunction ->
            "<Function>"

        Plain ElmInternals ->
            "<Internal>"

        Plain ElmUnit ->
            "()"

        Plain (ElmFile s) ->
            "<File:: " ++ s ++ " >"

        Plain (ElmBytes i) ->
            "<Bytes:: " ++ String.fromInt i ++ " >"

        Expandable _ (ElmSequence _ listValues) ->
            let
                innards =
                    listValues
                        |> List.map elmValueToString
                        |> String.join "\n, "
            in
            "[ " ++ innards ++ " ]"

        Expandable _ (ElmType typeName listValues) ->
            let
                innards =
                    List.map elmValueToString listValues
                        |> String.join " "
            in
            typeName ++ " " ++ innards

        Expandable _ (ElmRecord listFields) ->
            let
                innards =
                    listFields
                        |> List.map (\( k, v ) -> k ++ " = " ++ elmValueToString v)
                        |> String.join "\n, "
            in
            "{ " ++ innards ++ "\n}"

        Expandable _ (ElmDict listKeyValues) ->
            "Dict.fromList [ " ++ (List.map (\( k, v ) -> "( " ++ elmValueToString k ++ ", " ++ elmValueToString v ++ " )") listKeyValues |> String.join ", ") ++ " ]"


parsedStateToHtml : Path.Dict Metadata -> ElmValue -> ElmValue -> List (H.Html msg)
parsedStateToHtml metadata oldState newState =
    parsedStateToHtmlHelper metadata 1 Path.root oldState newState


parsedStateToHtmlHelper : Path.Dict Metadata -> Int -> Path.Path -> ElmValue -> ElmValue -> List (H.Html msg)
parsedStateToHtmlHelper metadata idx path oldState newState =
    case ( oldState, newState ) of
        ( Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "State" [ _, oldFst ]), Expandable _ (ElmType "End" []) ]), Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "State" [ _, newFst ]), Expandable _ (ElmType "End" []) ]) ) ->
            parsedStateToHtmlHelper metadata 1 (Path.add idx path) oldFst newFst

        ( Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "State" [ _, oldFst ]), oldSnd ]), Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "State" [ _, newFst ]), newSnd ]) ) ->
            parsedStateToHtmlHelper metadata 1 (Path.add idx path) oldFst newFst
                ++ parsedStateToHtmlHelper metadata (idx + 1) path oldSnd newSnd

        _ ->
            let
                oldStrings =
                    elmValueToString oldState |> String.lines

                newStrings =
                    elmValueToString newState |> String.lines

                label =
                    Path.dict.get path metadata
                        |> Maybe.map .label
                        |> Maybe.withDefault "ERROR"
            in
            [ H.div
                [ HA.classList
                    [ ( "studio-debug-state-item", True )
                    , ( "updated", oldStrings /= newStrings )
                    ]
                ]
                [ H.p []
                    [ H.text (Path.toString path ++ ": ")
                    , H.text label
                    ]
                , H.p []
                    (List.map2
                        (\oldLine newLine ->
                            if oldLine /= newLine then
                                H.div []
                                    [ H.p [ HA.class "diff-deleted" ] [ H.text oldLine ]
                                    , H.p [ HA.class "diff-inserted" ] [ H.text newLine ]
                                    ]

                            else
                                H.p [] [ H.text oldLine ]
                        )
                        oldStrings
                        newStrings
                    )
                ]
            ]


parsedDeltaToHtml : Path.Dict Metadata -> ElmValue -> List (H.Html (Msg delta))
parsedDeltaToHtml metadata deltaValue =
    parsedDeltaToHtmlHelper metadata 1 Path.root deltaValue


parsedDeltaToHtmlHelper : Path.Dict Metadata -> Int -> Path.Path -> ElmValue -> List (H.Html (Msg delta))
parsedDeltaToHtmlHelper metadata idx path deltaValue =
    case deltaValue of
        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "Skip" _), Expandable _ (ElmType "End" _) ]) ->
            []

        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "Skip" _), next ]) ->
            parsedDeltaToHtmlHelper metadata (idx + 1) path next

        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "ChangeStateOnInput" [ this ]), Expandable _ (ElmType "End" _) ]) ->
            parsedDeltaToHtmlHelper metadata 1 (Path.add idx path) this

        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "ChangeStateOnInput" [ this ]), next ]) ->
            parsedDeltaToHtmlHelper metadata 1 (Path.add idx path) this ++ parsedDeltaToHtmlHelper metadata (idx + 1) path next

        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "ChangeStateInternally" [ this ]), Expandable _ (ElmType "End" _) ]) ->
            parsedDeltaToHtmlHelper metadata 1 (Path.add idx path) this

        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "ChangeStateInternally" [ this ]), next ]) ->
            parsedDeltaToHtmlHelper metadata 1 (Path.add idx path) this ++ parsedDeltaToHtmlHelper metadata (idx + 1) path next

        Expandable _ (ElmSequence SeqTuple [ (Expandable _ (ElmType _ _)) as this, Expandable _ (ElmType "End" _) ]) ->
            parsedDeltaToHtmlHelper metadata 1 (Path.add idx path) this

        Expandable _ (ElmSequence SeqTuple [ (Expandable _ (ElmType _ _)) as this, next ]) ->
            parsedDeltaToHtmlHelper metadata 1 (Path.add idx path) this ++ parsedDeltaToHtmlHelper metadata (idx + 1) path next

        _ ->
            let
                label =
                    Path.dict.get path metadata |> Maybe.map .label |> Maybe.withDefault "ERROR"
            in
            [ H.span [] [ H.text (Path.toString path ++ ": "), H.text label ]
            , H.div [] [ H.text (elmValueToString deltaValue) ]
            ]


sanitiseCss : String -> String
sanitiseCss css =
    case P.run cssParser css of
        Ok list ->
            String.join "\n" list

        Err _ ->
            ""


cssParser : P.Parser (List String)
cssParser =
    P.loop [] cssParserHelper


cssParserHelper : List String -> P.Parser (P.Step (List String) (List String))
cssParserHelper revStmts =
    P.oneOf
        [ commentParser |> P.map (\_ -> P.Loop revStmts)
        , statementParser |> P.mapChompedString (\str () -> P.Loop (("#studio-form " ++ str) :: revStmts))
        , P.chompIf (\c -> List.member c [ ' ', '\n', '\t' ]) |> P.map (\_ -> P.Loop revStmts)
        , P.succeed () |> P.map (\_ -> P.Done (List.reverse revStmts))
        ]


commentParser : P.Parser ()
commentParser =
    P.succeed ()
        |. P.symbol "/*"
        |. P.chompUntil "*/"
        |. P.symbol "*/"


statementParser : P.Parser ()
statementParser =
    P.succeed ()
        |. P.chompUntil "{"
        |. P.chompUntil ":"
        |. P.chompUntil ";"
        |. P.chompUntil "}"
        |. P.symbol "}"


stylesheet : String -> String
stylesheet formCss =
    """
/* RESET */
/* Box sizing rules */
*,
*::before,
*::after {
    box-sizing: border-box;
}

/* Remove default margin */
body,
h1,
h2,
h3,
h4,
p,
figure,
blockquote,
dl,
dd {
    margin: 0;
}

/* Remove list styles on ul, ol elements with a list role, which suggests default styling will be removed */
ul[role='list'],
ol[role='list'] {
    list-style: none;
}

/* Set core root defaults */
html:focus-within {
    scroll-behavior: smooth;
}

/* Set core body defaults */
body {
    min-height: 100dvh;
    text-rendering: optimizeSpeed;
    line-height: 1.5;
}

/* A elements that don't have a class get default styles */
a:not([class]) {
    text-decoration-skip-ink: auto;
}

/* Make images easier to work with */
img,
picture {
    max-width: 100%;
    display: block;
}

/* Inherit fonts for inputs and buttons */
input,
button,
textarea,
select {
    font: inherit;
}

/* Remove all animations, transitions and smooth scroll for people that prefer not to see them */
@media (prefers-reduced-motion: reduce) {
    html:focus-within {
        scroll-behavior: auto;
    }

    *,
    *::before,
    *::after {
        animation-duration: 0.01ms !important;
        animation-iteration-count: 1 !important;
        transition-duration: 0.01ms !important;
        scroll-behavior: auto !important;
    }
}

#studio-container {
    display: flex;
    flex-direction: column;
    width: 100dvw;
    height: 100dvh;
}

#studio-header {
    background-color: #151530;
    color: white;
    font-family: sans-serif;
    text-align: center;    
    flex: 0;
}

#studio-body {
    display: flex;
    flex: 1;
}

#studio-form {
    flex: 1;
    padding: 30px;
    border: solid 10px #151530;
    overflow: scroll;
}

#studio-tools {
    flex: 1;
    padding: 10px;
    background-color: #151530;
    color: white;
    overflow: scroll;
    font-family: sans-serif;
    display: flex;
    flex-direction: column;
}

#studio-nav {
    margin-bottom: 20px;
    flex: 0 0;
}

#studio-nav button {
    color: white;
    background-color: inherit;
    font-size: 12pt;
    font-weight: 800;
    border: none;
    padding: 10px;
}

#studio-nav button.selected {
    border-bottom: white solid 1px;
}

#studio-debug {
    flex: 1;
    display: flex;
    flex-direction: row;
    justify-content: stretch;
    align-items: stretch;
    
}

#studio-debug-container {
    flex: 1;
    display: flex;
    gap: 20px;
}

#studio-debug-state {
    flex: 1;
    display: flex;
    flex-direction: column;
}

#studio-debug-state-list {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 10px;
}

.studio-debug-state-item {
    padding: 10px;
    border: 1px solid gray;
    border-radius: 5px;
}

.studio-debug-state-item.updated {
    background-color: white;
    border-color: white;
    color: #151530;
}

#studio-debug-deltas-and-output {
    display: flex;
    flex-direction: column;
    flex: 1;
    align-items: stretch;
}

#studio-debug-deltas {
    flex: 1;
    display: flex;
    flex-direction: column;
}

#studio-debug-deltas-list {
    display: flex;
    flex-direction: column;
    max-height: 400px;
    overflow: auto;
    gap: 10px;
}

.studio-debug-delta-button {
    width: 100%;
    text-align: left;
    background-color: #151530;
    color: white;
    padding: 10px;
    border: 1px solid gray;
    border-radius: 5px;
}

#studio-debug-output {
    flex: 1;
}

#studio-style {
    flex: 1;
    display: flex;
    justify-content: stretch;
    gap: 20px;
}

#studio-style-controls-list {
    flex: 1;
}

.studio-style-controls-item {
    display: flex;
    flex-direction: column;
}

#studio-style-css-editor {
    flex: 2;
    display: flex;
    flex-direction: column;
}

#studio-css-editor {
    width: 100%;
    overflow: scroll;
    flex-grow: 1;
    font-family: monospace;
    padding: 10px;
}

#studio-deploy {
    display: flex;
    flex-direction: column;
}

#studio-deploy pre {
    user-select: all; 
    overflow: scroll;
    flex: 1 1;
    border: 1px solid white;
    padding: 10px;
}

.diff-inserted {
    background-color: palegreen;
}

.diff-deleted {
    background-color: pink;
}
""" ++ sanitiseCss formCss
