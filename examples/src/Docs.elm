module Docs exposing (main)

import Browser
import Control as C
import Html as H
import Html.Attributes as HA
import Markdown.Parser as Markdown
import Markdown.Renderer


main =
    Browser.document
        { init = \() -> form.init
        , update =
            \msg model ->
                case msg of
                    Nothing ->
                        ( model, Cmd.none )

                    Just delta ->
                        form.update delta model
        , view =
            \model ->
                { title = "Docs"
                , body = [ form.view model ]
                }
        , subscriptions = form.subscriptions
        }


form =
    C.form
        { control = lessons
        , onSubmit = Nothing
        , onUpdate = Just
        }


type Lessons l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 l11 l12
    = BasicControls l01 --BasicControls
    | YourFirstForm l02 --TuplesAndTriples
    | TuplesAndTriples l03 --TuplesAndTriples
    | Records l04 --Records
    | MaybeAndResult l05 --MaybeAndResult
    | CustomTypes l06 --CustomTypes
    | Lists l07 --Lists
    | DictsSetsAndArrays l08 --DictsSetsAndArrays
    | CreateYourOwn l08 --CreateYourOwn
    | Mapping l10 --Mapping
    | Validation l11 --Validation
    | MultiValidation l12 --MultiValidation


lessons =
    C.customType
        (\l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 l11 l12 tag ->
            case tag of
                BasicControls data ->
                    l01 data

                YourFirstForm data ->
                    l02 data

                TuplesAndTriples data ->
                    l03 data

                Records data ->
                    l04 data

                MaybeAndResult data ->
                    l05 data

                CustomTypes data ->
                    l06 data

                Lists data ->
                    l07 data

                DictsSetsAndArrays data ->
                    l08 data

                CreateYourOwn data ->
                    l09 data

                Mapping data ->
                    l10 data

                Validation data ->
                    l11 data

                MultiValidation data ->
                    l12 data
        )
        |> C.tag1 "1: Basic controls" BasicControls basicControls
        |> C.tag1 "2: Your first form" YourFirstForm yourFirstForm
        |> C.tag1 "3: Tuples and Triples" TuplesAndTriples tuplesAndTriples
        |> C.tag1 "4: Records" Records records
        |> C.tag1 "5: Maybe and Result" MaybeAndResult maybeAndResult
        |> C.tag1 "6: Custom Types" CustomTypes customTypes
        |> C.tag1 "7: Lists" Lists lists
        |> C.tag1 "8: Dicts, Sets, and Arrays" DictsSetsAndArrays dictsSetsAndArrays
        |> C.tag1 "9: Create your own" CreateYourOwn createYourOwn
        |> C.tag1 "10: Mapping" Mapping mapping
        |> C.tag1 "11: Validation" Validation validation
        |> C.tag1 "12: Multi-field validation" MultiValidation multivalidation
        |> C.end
        |> C.label "Lessons"


basicControls =
    C.record (\bool string char int float -> { bool = bool, string = string, char = char, int = int, float = float })
        |> C.field .bool
            (C.bool |> mdBefore """
The most basic control is probably `Control.bool`, which we render using a standard HTML `<input type="checkbox">` element.""")
        |> C.field .string
            (C.string |> mdBefore """
`Control.string` is rendered as `<input type="text">`.""")
        |> C.field .char
            (C.char |> mdBefore """
`Control.char` is very similar, except that it provides built-in validation to ensure that the user enters exactly one character.""")
        |> C.field .int
            (C.int |> mdBefore """
`Control.int` and `Control.float` are both rendered as `<input type="number">`. Both provide built-in validation to ensure that the user enters the right type of number.""")
        |> C.field .float C.float
        |> C.end
        |> mdBefore """
## Basic controls
Let's start by looking at the simple controls that `elm-any-type-forms` provides for Elm's primitive types: `Bool`, `String`, `Char`, `Int` and `Float`.
"""
        |> mdAfter """
All controls are displayed with `<label>` elements to help with accessibility. Each control is wrapped in a `<div class="control-container">`, which contains the label, the input, and potentially also a `<div class="control-feedback-container">` that contains a list of feedback from validation.

But how do we actually use a control? First, we need to convert it into a form..."""


yourFirstForm =
    C.bool
        |> mdBefore """
## Your first form
Let's get up and running by building the simplest possible thing: a form that consists of just a single `Bool` input.

Create a new project folder, run `elm init` and then `elm install edkelly303/elm-any-type-forms`.

Next, create a file called 'Main.elm' in the `/src` folder. Open `Main.elm` in your code editor and paste in the following:

```
module Main exposing (main)

import Control

main =
    Control.sandbox
        { control = Control.bool
        , outputToString = Debug.toString
        }
```

If you now run `elm reactor` from the root of your project folder and visit [http://localhost:8000/src/Main.elm](http://localhost:8000/src/Main.elm), you should see a webpage with a control something like this:
"""
        |> mdAfter """
(Although the styling will be different, because `elm reactor` doesn't include any CSS.)

Try swapping `Control.bool` for `Control.string` or `Control.int` to see different types of controls in action.

However, most useful forms contain more than one control. How can we _combine_ controls to make something a bit more interesting?
"""


tuplesAndTriples =
    C.int


maybeAndResult =
    C.int


lists =
    C.int


dictsSetsAndArrays =
    C.int


records =
    C.int


customTypes =
    C.int


createYourOwn =
    C.int


mapping =
    C.int


validation =
    C.int


multivalidation =
    C.int



{-
   db   db d888888b .88b  d88. db           db   db d88888b db      d8888b. d88888b d8888b. .d8888.
   88   88 `~~88~~' 88'YbdP`88 88           88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
   88ooo88    88    88  88  88 88           88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
   88~~~88    88    88  88  88 88           88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
   88   88    88    88  88  88 88booo.      88   88 88.     88booo. 88      88.     88 `88. db   8D
   YP   YP    YP    YP  YP  YP Y88888P      YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'
-}


para =
    H.p []


el htmlTag txt =
    htmlTag [] [ H.text txt ]


h2 =
    el H.h2


code =
    el H.code


text =
    H.text


link address =
    H.a [ HA.href address ] [ H.text address ]


pre strings =
    H.pre [] [ H.text (String.join "\n" strings) ]



{-
   .88b  d88.  .d8b.  d8888b. db   dD d8888b.  .d88b.  db   d8b   db d8b   db      db   db d88888b db      d8888b. d88888b d8888b. .d8888.
   88'YbdP`88 d8' `8b 88  `8D 88 ,8P' 88  `8D .8P  Y8. 88   I8I   88 888o  88      88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
   88  88  88 88ooo88 88oobY' 88,8P   88   88 88    88 88   I8I   88 88V8o 88      88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
   88  88  88 88~~~88 88`8b   88`8b   88   88 88    88 Y8   I8I   88 88 V8o88      88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
   88  88  88 88   88 88 `88. 88 `88. 88  .8D `8b  d8' `8b d8'8b d8' 88  V888      88   88 88.     88booo. 88      88.     88 `88. db   8D
   YP  YP  YP YP   YP 88   YD YP   YD Y8888D'  `Y88P'   `8b8' `8d8'  VP   V8P      YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'


-}


mdBefore str =
    C.htmlBefore (md str)


mdAfter str =
    C.htmlAfter (md str)


md : String -> H.Html msg
md markdownInput =
    case
        markdownInput
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen
                (\ast ->
                    Markdown.Renderer.render
                        Markdown.Renderer.defaultHtmlRenderer
                        ast
                )
    of
        Ok rendered ->
            H.div [] rendered

        Err errors ->
            text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
