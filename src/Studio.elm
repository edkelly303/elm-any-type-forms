module Studio exposing (Debug, Model, Msg(..), Page(..), stylesheet, view)

import DebugParser
import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
import Dict
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
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
    { css : Dict.Dict Int { url : String, content : String, autoReload : Bool }
    , autoReloadInterval : Float
    , oldState : state
    , newState : state
    , page : Page
    , deltas : List delta
    , expanded : Path.Set
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
    | CssLoaded Int (Result Http.Error String)
    | CssChanged String
    | CssReloadRequested
    | CssAutoreloadToggled Int
    | PageChanged Page
    | TimeTravelled Int
    | ExpandStateClicked Path.Path
    | CollapseStateClicked Path.Path


view :
    { debugToString : Debug state delta output -> String
    , metadata : Path.Dict Metadata
    , outputParsingResult : Result (List Feedback) output
    , validationErrors : List Feedback
    , oldState : state
    , newState : state
    , deltas : List delta
    , expanded : Path.Set
    }
    -> H.Html (Msg delta)
view { debugToString, metadata, outputParsingResult, validationErrors, oldState, newState, deltas, expanded } =
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

        pathsAndDeltas =
            List.indexedMap (\idx delta -> deltaToHtml idx metadata debugToString delta) deltas
                |> List.concat
    in
    H.div [ HA.id "studio-debug-container" ]
        [ H.div [ HA.id "studio-debug-state" ]
            [ H.h3 [] [ H.text "State" ]
            , H.pre [ HA.id "studio-debug-state-list" ]
                (stateToHtml debugToString metadata pathsAndDeltas expanded oldState newState)
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


stateToHtml debugToString metadata pathsAndDeltas expanded oldState newState =
    let
        oldParsed =
            ( Just oldState, Nothing, Nothing )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig

        newParsed =
            ( Just newState, Nothing, Nothing )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig

        extractParsedState elmValueResult =
            case elmValueResult of
                Ok (Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "Just" [ parsedState ]), _, _ ])) ->
                    Just parsedState

                _ ->
                    Nothing
    in
    case ( extractParsedState oldParsed, extractParsedState newParsed ) of
        ( Just oldParsedState, Just newParsedState ) ->
            parsedStateToHtml metadata pathsAndDeltas expanded oldParsedState newParsedState

        _ ->
            []


deltaToHtml : Int -> Path.Dict Metadata -> (Debug state delta output -> String) -> delta -> List ( Path.Path, H.Html (Msg delta) )
deltaToHtml idx metadata debugToString delta =
    let
        parsed =
            ( Nothing, Just delta, Nothing )
                |> debugToString
                |> DebugParser.parseValue DebugParser.defaultConfig
    in
    case parsed of
        Ok (Expandable _ (ElmSequence SeqTuple [ _, Expandable _ (ElmType "Just" [ Expandable _ (ElmType _ [ parsedDelta ]) ]), _ ])) ->
            parsedDeltaToHtml metadata parsedDelta
                |> List.map
                    (Tuple.mapSecond
                        (H.button
                            [ HE.onClick (TimeTravelled idx), HA.class "studio-debug-delta-button" ]
                        )
                    )

        _ ->
            []


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


parsedStateToHtml :
    Path.Dict Metadata
    -> List ( Path.Path, H.Html (Msg delta) )
    -> Path.Set
    -> ElmValue
    -> ElmValue
    -> List (H.Html (Msg delta))
parsedStateToHtml metadata pathsAndDeltas expanded oldState newState =
    parsedStateToHtmlHelper metadata pathsAndDeltas expanded 1 Path.root oldState newState


parsedStateToHtmlHelper :
    Path.Dict Metadata
    -> List ( Path.Path, H.Html (Msg delta) )
    -> Path.Set
    -> Int
    -> Path.Path
    -> ElmValue
    -> ElmValue
    -> List (H.Html (Msg delta))
parsedStateToHtmlHelper metadata pathsAndDeltas expanded idx path oldState newState =
    case Path.dict.get path metadata of
        Nothing ->
            [ H.text "ERROR: missing path" ]

        Just meta ->
            let
                collapseButton =
                    if idx == 1 then
                        H.div
                            [ HA.style "position" "relative"
                            , HA.style "left" (String.fromInt ((Path.depth path - 1) * 10) ++ "px")
                            ]
                            [ H.button [ HE.onClick (CollapseStateClicked path) ] [ H.text "-" ], H.text meta.label ]

                    else
                        H.text ""

                expandButton =
                    H.div
                        [ HA.style "position" "relative"
                        , HA.style "left" (String.fromInt ((Path.depth path - 1) * 10) ++ "px")
                        ]
                        [ H.button [ HE.onClick (ExpandStateClicked path) ] [ H.text "+" ], H.text meta.label ]
            in
            case ( classifyState oldState, classifyState newState ) of
                ( FinalCombinator thisStateOld, FinalCombinator thisStateNew ) ->
                    if Path.set.member path expanded then
                        collapseButton
                            :: parsedStateToHtmlHelper metadata pathsAndDeltas expanded 1 (Path.add idx path) thisStateOld thisStateNew

                    else
                        [ expandButton ]

                ( Combinator thisStateOld nextStateOld, Combinator thisStateNew nextStateNew ) ->
                    if Path.set.member path expanded then
                        collapseButton
                            :: parsedStateToHtmlHelper metadata pathsAndDeltas expanded 1 (Path.add idx path) thisStateOld thisStateNew
                            ++ parsedStateToHtmlHelper metadata pathsAndDeltas expanded (idx + 1) path nextStateOld nextStateNew

                    else
                        [ expandButton ]

                ( Simple, Simple ) ->
                    let
                        oldStrings =
                            elmValueToString oldState |> String.lines

                        newStrings =
                            elmValueToString newState |> String.lines

                        deltas =
                            List.filterMap
                                (\( p, html ) ->
                                    if p == path then
                                        Just html

                                    else
                                        Nothing
                                )
                                pathsAndDeltas
                    in
                    [ H.div
                        [ HA.classList
                            [ ( "studio-debug-state-item", True )
                            , ( "updated", oldStrings /= newStrings )
                            ]
                        ]
                        [ H.p []
                            [ H.text "Label: "
                            , H.strong [] [ H.text meta.label ]
                            ]
                        , H.p []
                            [ H.text "Path: "
                            , H.text (Path.toString path)
                            ]
                        , H.p []
                            [ H.text "HTML id: "
                            , H.text (Maybe.withDefault ("control-" ++ Path.toString path) meta.id)
                            ]
                        , H.p []
                            [ H.text "HTML class: "
                            , H.text ("\"" ++ String.join " " meta.class ++ "\"")
                            ]
                        , H.p []
                            [ H.text "State:"
                            , H.div []
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
                        , H.details []
                            [ H.summary [] [ H.text "Deltas" ]
                            , H.div [ HA.id "studio-debug-deltas-list" ] deltas
                            ]
                        ]
                    ]

                _ ->
                    [ H.text "ERROR: mismatched states!" ]


type ClassifiedState
    = Combinator ElmValue ElmValue
    | FinalCombinator ElmValue
    | Simple


classifyState : ElmValue -> ClassifiedState
classifyState state =
    case state of
        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "State" [ _, thisState ]), Expandable _ (ElmType "End" []) ]) ->
            FinalCombinator thisState

        Expandable _ (ElmSequence SeqTuple [ Expandable _ (ElmType "State" [ _, thisState ]), nextState ]) ->
            Combinator thisState nextState

        _ ->
            Simple


parsedDeltaToHtml : Path.Dict Metadata -> ElmValue -> List ( Path.Path, List (H.Html (Msg delta)) )
parsedDeltaToHtml metadata deltaValue =
    parsedDeltaToHtmlHelper metadata 1 Path.root deltaValue


parsedDeltaToHtmlHelper : Path.Dict Metadata -> Int -> Path.Path -> ElmValue -> List ( Path.Path, List (H.Html (Msg delta)) )
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
                    Path.dict.get path metadata
                        |> Maybe.map .label
                        |> Maybe.withDefault "ERROR"
            in
            [ ( path, [ H.text (elmValueToString deltaValue) ] ) ]


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
