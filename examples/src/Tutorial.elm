module Tutorial exposing (main)

import Browser
import Control
import Date
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Markdown.Parser as Markdown
import Markdown.Renderer
import Time


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init () =
    let
        ( initialForm, cmd ) =
            form.blank
    in
    ( { form = initialForm
      , output = Nothing
      , bool = True
      }
    , cmd
    )


update msg model =
    case msg of
        Nothing ->
            let
                ( newForm, result ) =
                    form.submit model.bool model.form
            in
            ( { model
                | form = newForm
                , output = Just result
              }
            , Cmd.none
            )

        Just delta ->
            let
                ( newForm, cmd ) =
                    form.update model.bool delta model.form
            in
            ( { model | form = newForm }
            , cmd
            )


subscriptions model =
    form.subscriptions model.bool model.form


view model =
    { title = "elm-any-type-forms tutorial"
    , body =
        [ H.div [] [ form.view model.bool model.form ]
        ]
    }


form =
    Control.simpleFormWithContext
        { control = lessons
        , onSubmit = Nothing
        , onUpdate = Just
        }


type Lessons l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 l11
    = BasicControls l01 --BasicControls
    | YourFirstForm l02 --TuplesAndTriples
    | TuplesAndTriples l03 --TuplesAndTriples
    | Records l04 --Records
    | EnumsAndCustomTypes l05 --CustomTypes
    | ListsDictsSetsAndArrays l06 --DictsSetsAndArrays
    | Mapping l07 --Mapping
    | Validation l08 --Validation
    | MultiValidation l09 --MultiValidation
    | CreateYourOwn l10 --CreateYourOwn
    | LeavingTheSandbox l11


lessons =
    Control.customType
        (\l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 l11 variant ->
            case variant of
                YourFirstForm data ->
                    l01 data

                BasicControls data ->
                    l02 data

                TuplesAndTriples data ->
                    l03 data

                Records data ->
                    l04 data

                EnumsAndCustomTypes data ->
                    l05 data

                ListsDictsSetsAndArrays data ->
                    l06 data

                Mapping data ->
                    l07 data

                Validation data ->
                    l08 data

                MultiValidation data ->
                    l09 data

                CreateYourOwn data ->
                    l10 data

                LeavingTheSandbox data ->
                    l11 data
        )
        |> Control.variant1 "Your first form" YourFirstForm yourFirstForm
        |> Control.variant1 "Basic controls" BasicControls basicControls
        |> Control.variant1 "Tuples and triples" TuplesAndTriples tuplesAndTriples
        |> Control.variant1 "Records and labels" Records records
        |> Control.variant1 "Enums and custom types" EnumsAndCustomTypes enumsAndCustomTypes
        |> Control.variant1 "Lists, Dicts, Sets & Arrays" ListsDictsSetsAndArrays listsDictsSetsAndArrays
        |> Control.variant1 "Converting controls" Mapping mapping
        |> Control.variant1 "Validating controls" Validation validation
        |> Control.variant1 "Multi-control validation" MultiValidation multivalidation
        |> Control.variant1 "Creating your own controls" CreateYourOwn createYourOwn
        |> Control.variant1 "Leaving the sandbox" LeavingTheSandbox leavingTheSandbox
        |> Control.endCustomType
        |> Control.layout
            (\config subcontrols ->
                let
                    navBar =
                        H.div [ HA.id config.id ]
                            (List.map
                                (\sc ->
                                    H.button
                                        [ HE.onClick (config.selectMsg sc.index)
                                        , HA.type_ "button"
                                        , HA.class
                                            (if sc.index == config.selected then
                                                "lesson-selected"

                                             else
                                                "lesson-not-selected"
                                            )
                                        ]
                                        [ H.text sc.label
                                        ]
                                )
                                subcontrols
                            )

                    subcontrolViews =
                        List.map
                            (\sc ->
                                if sc.index == config.selected then
                                    H.div
                                        [ HA.id (String.fromInt sc.index)
                                        , HA.class "lesson-page"
                                        ]
                                        (sc.html ++ nextButton)

                                else
                                    H.text ""
                            )
                            subcontrols

                    nextLabel =
                        List.filter (\sc -> sc.index == config.selected + 1) subcontrols
                            |> List.map .label
                            |> List.head
                            |> Maybe.withDefault "ERROR"

                    nextButton =
                        [ if config.selected == List.length subcontrols then
                            H.text ""

                          else
                            H.button
                                [ HA.id "next-button"
                                , HA.type_ "button"
                                , HE.onClick (config.selectMsg (config.selected + 1))
                                ]
                                [ H.text ("Next: " ++ nextLabel) ]
                        ]
                in
                navBar :: subcontrolViews
            )
        |> Control.label "Lessons"
        |> Control.id "lessons"
        |> htmlBefore lessonsHeading


lessonsHeading =
    md "# An introduction to `elm-any-type-forms`"


yourFirstForm =
    Control.bool
        |> htmlBefore yourFirstFormIntro
        |> htmlAfter yourFirstFormOutro


yourFirstFormIntro =
    md """
## Your first form
Let's get up and running by building the simplest possible thing: a form that consists of just a single `Bool` control.

Create a new project folder, open your terminal, run `elm init` and then `elm install edkelly303/elm-any-type-forms`.

Next, create a file called 'Main.elm' in the `/src` subfolder. Open `Main.elm` in your code editor and paste in the 
following:

```
module Main exposing (main)

import Control

main =
    Control.sandbox
        { control = Control.bool
        , outputToString = Debug.toString
        }
```

If you now run `elm reactor` from the root of your project folder and visit 
[http://localhost:8000/src/Main.elm](http://localhost:8000/src/Main.elm), you should see a webpage with a control 
something like this:
"""


yourFirstFormOutro =
    md """
(Although the styling will be different, because `elm reactor` doesn't include any CSS.)

Next up, let's take a look at some of the other basic controls included in this package.
    """


basicControls =
    Control.record (\bool string char int float -> { bool = bool, string = string, char = char, int = int, float = float })
        |> Control.field .bool
            (Control.bool |> htmlBefore (md """
As we've already seen, there's `Control.bool`, which we render using a standard HTML `<input type="checkbox">` 
element."""))
        |> Control.field .string
            (Control.string |> htmlBefore (md """
`Control.string` is rendered as `<input type="text">`."""))
        |> Control.field .char
            (Control.char |> htmlBefore (md """
`Control.char` is very similar, except that it provides built-in validation to ensure that the user enters exactly one 
character."""))
        |> Control.field .int
            (Control.int |> htmlBefore (md """
`Control.int` and `Control.float` are both rendered as `<input type="number">`. Both provide built-in validation to 
ensure that the user enters the right type of number."""))
        |> Control.field .float Control.float
        |> Control.endRecord
        |> htmlBefore basicControlsIntro
        |> htmlAfter basicControlsOutro


basicControlsIntro =
    md """
## Basic controls
The package includes simple controls for all of Elm's primitive types: `Bool`, 
`String`, `Char`, `Int` and `Float`.
"""


basicControlsOutro =
    md """
All controls are displayed with `<label>` elements to help with accessibility. Each control is wrapped in a 
`<div class="control-container">`, which contains the label, the input, and potentially also a 
`<div class="control-feedback-container">` that contains a list of feedback from validation.

You can try out any of these controls by simply swapping the relevant function into your `main` definition - for example, here's `Control.string`:
```
main =
    Control.sandbox
        { control = Control.string
        , outputToString = Debug.toString
        }
```
However, most useful forms contain more than one control. How can we _combine_ controls to make something a bit more 
interesting?"""


tuplesAndTriples =
    Control.record (\_ _ -> ())
        |> Control.field (\() -> ( 1, "hello" )) (Control.tuple Control.int Control.string)
        |> Control.field (\() -> ( 1, "hello", 1.0 )) (Control.triple Control.int Control.string Control.float |> htmlBefore tripleIntro)
        |> Control.endRecord
        |> htmlBefore tuplesAndTriplesIntro
        |> htmlAfter tuplesAndTriplesOutro


tuplesAndTriplesIntro =
    md
        """
## Tuples and Triples

The simplest way of combining two values in Elm is to use a tuple. We can create tuples by passing two `Control`s to the
 `Control.tuple` combinator.

For example, change your code as follows to create a form that produces an `( Int, String )` tuple:

```
module Main exposing (main)

import Control

main =
    Control.sandbox
        { control = control
        , outputToString = Debug.toString
        }

control = 
    Control.tuple Control.int Control.string
```

Which should look something like this:
"""


tripleIntro =
    md
        """
Triples work too - if you change your code to: 

```
control = 
    Control.triple Control.int Control.string Control.float
```

You'll get an `( Int, String, Float )` triple like this:
"""


tuplesAndTriplesOutro =
    md
        """
But tuples and triples are Elm's least-loved data structures - if you want to combine multiple values, records tend to 
be much more flexible and user-friendly.

So... how do we create a form that produces a record?
"""


records =
    Control.tuple
        (Control.record (\name age -> { name = name, age = age })
            |> Control.field .name Control.string
            |> Control.field .age Control.int
            |> Control.endRecord
            |> htmlAfter recordMiddle
        )
        (Control.record (\name age -> { name = name, age = age })
            |> Control.field .name (Control.string |> Control.label "Name")
            |> Control.field .age (Control.int |> Control.label "Age")
            |> Control.endRecord
        )
        |> Control.layout (\config subcontrols -> List.concatMap .html subcontrols)
        |> htmlBefore recordIntro
        |> htmlAfter recordOutro


recordIntro =
    md
        """
## Records and labels

Imagine we are building a customer relationship management (CRM) system for a company that sells stickers 
to adorn the laptops of happy developers worldwide.

To represent our customers, let's use a record type:

```
type alias Customer = 
    { name : String
    , age : Int 
    }
```

We can build a control that produces these `Customer` records with the `Control.record` combinator:

```    
customerControl =
    Control.record (\\name age -> { name = name, age = age })
        |> Control.field .name Control.string
        |> Control.field .age Control.int
        |> Control.endRecord
```

Or if you prefer brevity to explicitness, you could even use the `Customer` constructor directly:

```
customerControl =
    Control.record Customer
        |> Control.field .name Control.string
        |> Control.field .age Control.int
        |> Control.endRecord
```

### Wiring it up

Let's take a look at this `customerControl` in our sandbox:

```
main =
    Control.sandbox
        { control = customerControl
        , outputToString = Debug.toString
        }
```

And you should see a form that looks like this:
"""


recordMiddle =
    md
        """
### Labelling controls
That's ok...ish. But one of the nice things about records is that their fields are _named_. So really, we want the 
controls to be labelled with the names of the fields. 

That's where `Control.label` comes in. Change your code to:

```    
customerControl =
    Control.record (\\name age -> { name = name, age = age })
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.endRecord
```

And you should now see something like this:
"""


recordOutro =
    md
        """
**Note:** We're going to see other functions that work like `Control.label` later - this is a common pattern for 
configuring controls. 

### A bit of refactoring

To keep things tidy, it's often better to pull out each control into a separate function, where 
you can apply as many configuration functions as you like without making your `Control.record` definitions too complex. 

With that in mind, let's refactor our code to this:

```    
customerControl =
    Control.record (\\name age -> { name = name, age = age })
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.endRecord

nameControl = 
    Control.string 
        |> Control.label "Name"

ageControl = 
    Control.int 
        |> Control.label "Age"
```

Now, with tuples, triples and records, we have multiple options for controls that produce types that contain multiple 
values. 

But Elm also has another kind of complex type: the custom type. How do we model those?
"""


enumsAndCustomTypes =
    Control.record (\a b -> ( a, b ))
        |> Control.field Tuple.first
            (simpleStickerControl
                |> htmlBefore enumsAndCustomTypesIntro
            )
        |> Control.field Tuple.second
            (enumsAndCustomTypesCustomerControl
                |> htmlBefore customTypesIntro
                |> htmlAfter customTypesOutro
            )
        |> Control.endRecord


enumsAndCustomTypesCustomerControl =
    Control.record
        (\name age sticker ->
            { name = name
            , age = age
            , sticker = sticker
            }
        )
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.field .sticker stickerControl
        |> Control.endRecord


type SimpleSticker
    = SimpleRectangular
    | SimpleCircular
    | SimpleHeartShaped


simpleStickerControl =
    Control.enum
        ( "Circular", SimpleCircular )
        ( "Heart-shaped", SimpleHeartShaped )
        [ ( "Rectangular", SimpleRectangular ) ]
        |> Control.label "Sticker type"


type Sticker
    = Circular
    | Rectangular Int Int
    | HeartShaped String


stickerControl =
    Control.customType
        (\circular heartShaped rectangular variant ->
            case variant of
                Circular ->
                    circular

                HeartShaped message ->
                    heartShaped message

                Rectangular width height ->
                    rectangular width height
        )
        |> Control.variant0 "Circular"
            Circular
        |> Control.variant1 "Heart-shaped"
            HeartShaped
            (Control.string |> Control.label "Message")
        |> Control.variant2 "Rectangular"
            Rectangular
            (Control.int |> Control.label "Width")
            (Control.int |> Control.label "Height")
        |> Control.endCustomType
        |> Control.label "Sticker type"


enumsAndCustomTypesIntro =
    md """
## Enums and Custom Types

Our company sells three main types of products: rectangular stickers, circular stickers, and heart-shaped stickers.
In our CRM system, we want to be able to track which products each customer has purchased, so we need some 
way of representing the type of sticker. 

How can we do that? Let's use a custom type!

### How to talk about custom types
Let's get our terminology clear before we start. In Elm, a custom type is composed of one or more _variants_. 
Each variant consists of a _tag_ followed by zero or more _arguments_.

For example, let's say we have a type like `Maybe Int`: 
* The tags of its variants are `Just` and `Nothing`. 
* The `Just` variant has one argument, an `Int`. 
* The `Nothing` variant has no arguments.

An _enum_ is a special kind of custom type that has at least two variants, and none of its variants have 
any arguments. Like `Bool` for example:

```
type Bool
    = True
    | False
```

### Enums
As a first prototype for modelling the company's products in our system, let's start with a simple enum type:

```
type Sticker
   = Circular
   | HeartShaped
   | Rectangular
```

We can create a control for this type using `Control.enum`. Since an enum type must have at least two variants, 
`Control.enum` requires us to provide at least two tags, and each tag must be accompanied by a `String` to use as a label. 
If the type has more than two tags, we can put the rest in a list.

```
stickerControl =
    Control.enum
        ( "Circular", Circular ) -- first variant
        ( "Heart-shaped", HeartShaped ) -- second variant
        [ ( "Rectangular", Rectangular ) ] -- list of other variants
        |> Control.label "Sticker type"
```

The result should look something like this:
"""


customTypesIntro =
    md """
### Custom Types 
But... that's not going to be enough. One of the key selling-points of our stickers is that we can customise them
for each customer's specific needs. Our product managers explain:
* We can print rectangular stickers in any size - the customer should be able to specify the width and height as `Int`s.
* We can print a custom message on heart-shaped stickers - the customer should be able to specify the text as a `String`.
* Circular stickers aren't customisable yet, but the product team is working on it!

We'll capture these details by changing our enum to a more complex custom type:

```
type Sticker
    = Circular
    | HeartShaped String
    | Rectangular Int Int
```

Let's see how we can build a control to represent these exciting products with `Control.customType`. This might look a 
bit daunting at first, but we'll walk through it step by step:

```
stickerControl =
    
    -- First, we call `Control.customType` and pass it a function that can 
    -- destructure a `Sticker` type and give us access to its arguments.
    
    Control.customType
        (\\circular heartShaped rectangular variant ->
            case variant of
                Circular ->
                    circular

                HeartShaped message ->
                    heartShaped message

                Rectangular width height ->
                    rectangular width height
        )

        -- Next, we teach the control how to construct our variants. Since the 
        -- `Circular` variant doesn't have any arguments, we use `Control.variant0` 
        -- and just supply the tag

        |> Control.variant0 "Circular"
            Circular

        -- Next, we handle `HeartShaped String` using `Control.variant1`, supplying 
        -- the tag followed by `Control.string` as the single argument.

        |> Control.variant1 "Heart-shaped"
            HeartShaped
            (Control.string |> Control.label "Message")
            
        -- Finally, since the `Rectangular Int Int` variant has two arguments, 
        -- we use `Control.variant2`, supplying the tag and two `Control.int`s.
        
        |> Control.variant2 "Rectangular"
            Rectangular
            (Control.int |> Control.label "Width")
            (Control.int |> Control.label "Height")

        -- Now just call `Control.endCustomType` to declare that we've finished adding 
        -- variants, and then `Control.label` to give the control an appropriate 
        -- label.
        
        |> Control.endCustomType
        |> Control.label "Sticker type"
```

### Wiring it up

Now we can add the new field to our `Customer` control as follows:

```
customerControl =
    Control.record 
        (\\name age sticker -> 
            { name = name
            , age = age
            , sticker = sticker
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .sticker stickerControl
        |> Control.endRecord
```

And you'll see something like this:
"""


customTypesOutro =
    md
        """
### Maybe and Result
You could easily implement Elm's `Maybe` and `Result` custom types using `Control.customType`. But 
there's no need - they're included as `Control.maybe` and `Control.result`.

Next up, we'll look at controls for data structures that can include multiple values of a given type: `List`, and other 
list-like things.

"""


listsDictsSetsAndArrays =
    stickerListControl
        |> htmlBefore listsIntro
        |> htmlAfter listsOutro


stickerListControl =
    Control.list stickerControl
        |> Control.label "Stickers purchased"


listsIntro =
    md
        """
## Lists, Dicts, Sets and Arrays

Hang on a minute - if each customer can only purchase a single sticker, the company is probably not going to
be very successful! 

What we really want our system to do is keep track of _all_ the stickers that each customer buys. Perhaps we could use 
some nifty data structure like a `List`?

```
type alias Customer = 
    { name : String
    , age : Int 
    , stickers : List Sticker
    , id : Id
    }
```

Fortunately, it's easy to turn any control into a list of controls by passing it to `Control.list`:

```
stickerListControl = 
    Control.list stickerControl
        |> Control.label "Stickers purchased"
```

This will give you a form that produces a list of stickers:
"""


listsOutro =
    md
        """
### Wiring it up 

Now you can add your new `stickerListControl` to your `customerControl` as follows:

```
customerControl =
    Control.record 
        (\\name age stickers -> 
            { name = name
            , age = age
            , stickers = stickers
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .stickers stickerListControl
        |> Control.endRecord
```

### Other list-like things

The package includes built-in combinators for three other list-like data structures from Elm's standard library: 
`Array`, `Set` and `Dict`.

`Control.array` and `Control.set` have exactly the same API as `Control.list` - just pass them a control of any type and 
you'll get a control that produces an `Array` or `Set` of that type. 

`Control.dict` is similar, except that it takes _two_ controls as arguments. It uses the first as the key and the second 
as the value for the `Dict` it produces.
"""


type Id
    = Id Int


mapping =
    idControl
        |> htmlBefore mappingIntro
        |> htmlAfter mappingOutro


idControl =
    Control.int
        |> Control.label "ID number"
        |> Control.map
            { convert = \int -> Id int
            , revert = \(Id int) -> int
            }


mappingIntro =
    md
        """
## Converting control types

In some circumstances, you may want to convert the type produced by a control to some other type. That's where 
`Control.map` becomes useful.

For example, suppose you want each of your customers to have a unique ID number. The number itself can be a simple `Int`, 
but to make your code more type-safe, you decide to wrap that `Int` in a custom type:

```
type Id = 
    Id Int

type alias Customer = 
    { name : String
    , age : Int 
    , stickers : List Sticker
    , id : Id
    }
```

To create a control for this new `Id` type, we just need to use `Control.map` to describe how to convert an `Int` into
an `Id`, and vice versa. So we need to supply two functions: `convert`, which turns an `Int` into an `Id`, and `revert`,
which turns an `Id` back into an `Int`:

```
idControl = 
    Control.int
        |> Control.label "ID number"
        |> Control.map 
            { convert = \\int -> Id int
            , revert = \\(Id int) -> int 
            }
```
It'll look something like this:
"""


mappingOutro =
    md
        """
### Wiring it up

You can add this new field to your `Customer` control as follows:

```
customerControl =
    Control.record 
        (\\name age stickers id -> 
            { name = name
            , age = age
            , stickers = stickers 
            , id = id
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .stickers stickerListControl
        |> Control.field .id idControl
        |> Control.endRecord
```        
"""


validation =
    nameControl
        |> htmlBefore validationIntro
        |> htmlAfter validationOutro


nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIfWithContext (\ctx name -> String.isEmpty name) "Name cannot be blank"
        |> Control.noteIfWithContext (\ctx name -> String.length name == 1) "Is that the full name?"


validationIntro =
    md
        """
## Validating controls

We've shown how we can build controls that produce pretty much any Elm type - but what if just producing any old value 
of that type isn't enough? What if we want to be more specific about which values we want our controls to accept?

### Showing errors

It's time to introduce some validation. For example, perhaps we want to ensure that our customer's name isn't left blank. 
We can do that with a function called `Control.failIf`:

```
nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf (\\name -> String.isEmpty name) "Name cannot be blank"
```

### Showing notifications

There might also be occasions where we want to notify the user that the data they've input might not be correct - but 
we're not _certain_ that the input is actually invalid. 

In these cases, we can use `Control.noteIf`:

```
nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf (\\name -> String.isEmpty name) "Name cannot be blank"
        |> Control.noteIf (\\name -> String.length name == 1) "Is that the full name?"
```

### What's the difference?

The difference between the two functions is that `Control.failIf` will cause the control to fail validation when the 
form is submitted, while `Control.noteIf` will allow it to pass. 

There's also a difference in the HTML produced by each function. Messages produced by `Control.failIf` are assigned an 
HTML attribute `class="control-feedback-fail"`, while those produced by `Control.noteIf` are given 
`class="control-feedback-note"`.

This makes it easy to style errors and notifications differently with CSS, as you can see below:
"""


validationOutro =
    md
        """
### Debouncing

You'll notice that the field doesn't validate itself instantly when you type into it. This is because by 
default, `Control.string` is set to debounce for 500 milliseconds before it shows the results of validation. 

You can configure the debouncing interval with `Control.debounce`, providing a value in milliseconds. For example, the
following code will create a control that displays validation messages immediately:

```
nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf (\\name -> String.isEmpty name) "Name cannot be blank"
        |> Control.noteIf (\\name -> String.length name == 1) "Is that the full name?"
        |> Control.debounce 0
```
"""


multivalidation =
    Control.record
        (\name age stickers id password ->
            { name = name
            , age = age
            , stickers = stickers
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.field .stickers stickerListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
        |> htmlBefore multivalidationIntro
        |> htmlAfter multivalidationOutro


passwordControl =
    Control.record (\choose confirm -> { choose = choose, confirm = confirm })
        |> Control.field .choose choosePasswordControl
        |> Control.field .confirm confirmPasswordControl
        |> Control.endRecord
        |> Control.alertIfWithContext
            (\ctx { choose, confirm } -> choose /= confirm)
            "password-mismatch"
        |> Control.map
            { convert = .choose
            , revert = \p -> { choose = p, confirm = p }
            }


choosePasswordControl =
    Control.string
        |> Control.label "Choose password"


confirmPasswordControl =
    Control.string
        |> Control.label "Confirm password"
        |> Control.respond
            { alert = "password-mismatch"
            , fail = True
            , message = "Passwords must match"
            , class = "control-feedback-fail"
            }


multivalidationIntro =
    md
        """
## Multi-control validation

Sometimes you might need to validate the input of one control based on the input of another. The classic example is
checking that passwords match, so let's try that:

```
type alias Passwords =
    { choose : String
    , confirm : String
    }

passwordControl =
    Control.record (\\choose confirm -> { choose = choose, confirm = confirm })
        |> Control.field .choose choosePasswordControl
        |> Control.field .confirm confirmPasswordControl
        |> Control.endRecord

choosePasswordControl =
    Control.string
        |> Control.label "Choose password"

confirmPasswordControl =
    Control.string
        |> Control.label "Confirm password"
```

The challenge here is that `confirmPasswordControl` has no way of knowing what's been entered in 
`choosePasswordControl`, so it can't tell whether the contents of the two controls match or not. That means we can't use
`Control.failIf` to handle this validation rule.

### Going up a level

We can solve this problem by moving the validation into the `passwordControl` record, which contains both fields and can
therefore check the data in both of them. If the fields don't match, we can use `Control.alertIf` to emit an alert:

```
passwordControl =
    Control.record (\\choose confirm -> { choose = choose, confirm = confirm })
        |> Control.field .choose choosePasswordControl
        |> Control.field .confirm confirmPasswordControl
        |> Control.endRecord
        |> Control.alertIf
            (\\{ choose, confirm } -> choose /= confirm)
            "password-mismatch"
```

Now, we use `Control.respond` to tell `confirmPasswordControl` to listen out for the `"password-mismatch"` alert. It can 
then respond by showing an error message to the user and causing the form to fail validation:

```
confirmPasswordControl =
    Control.string
        |> Control.label "Confirm password"
        |> Control.respond
            { alert = "password-mismatch"
            , fail = True
            , message = "Passwords must match"
            }
```

### Wiring it up

Finally, let's add the password to our `Customer` type, represented as a `String`. So our type will be:

```
type alias Customer = 
    { name : String
    , age : Int 
    , stickers : List Sticker
    , id : Id
    , password : String
    }
```

But... our `passwordControl` doesn't produce a `String`, it produces `{ choose : String, confirm : String }`. Uh oh!

### Control.map to the rescue!

Fortunately, all is not lost. We can use `Control.map` to convert the output type of `passwordControl` to a `String`, 
as we learned in the previous lesson:

```
passwordControl =
    Control.record (\\choose confirm -> { choose = choose, confirm = confirm })
        |> Control.field .choose choosePasswordControl
        |> Control.field .confirm confirmPasswordControl
        |> Control.endRecord
        |> Control.alertIf
            (\\{ choose, confirm } -> choose /= confirm)
            "password-mismatch"
        |> Control.map
            { convert = \\{ choose, confirm } -> choose
            , revert = \\password -> { choose = password, confirm = password }
            }
```

### Wiring it up... again

And now we just add `passwordControl` to `customerControl`, as usual:

```
customerControl =
    Control.record
        (\\name age stickers id password ->
            { name = name
            , age = age
            , stickers = stickers
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .stickers stickerListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
```

And you should see something a little like this:
"""


multivalidationOutro =
    md
        """
We've now covered all the basics for building controls with primitives and combinators. The next thing we'll cover is 
what to do when you want to create a completely new type of control from scratch.
"""


createYourOwn =
    customerControl
        |> htmlBefore createYourOwnIntro
        |> htmlAfter createYourOwnOutro


customerControl =
    Control.record
        (\name dateOfBirth stickers id password ->
            { name = name
            , dateOfBirth = dateOfBirth
            , stickers = stickers
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .dateOfBirth dateControl
        |> Control.field .stickers stickerListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord


dateControl =
    Control.define
        { label = "Date of birth"
        , blank = ( "1970-01-01", Cmd.none )
        , prefill = \date -> ( Date.format "yyyy-MM-dd" date, Cmd.none )
        , update =
            \delta state -> ( delta, Cmd.none )
        , view =
            \{ state, id, label, name, class } ->
                [ H.label [ HA.for id ]
                    [ H.text label
                    ]
                , H.input
                    [ HA.type_ "date"
                    , HA.value state
                    , HA.id id
                    , HA.class class
                    , HA.name name
                    , HE.onInput identity
                    ]
                    []
                ]
        , subscriptions =
            \state -> Sub.none
        , parse =
            \state ->
                case Date.fromIsoString state of
                    Ok date ->
                        Ok date

                    Err error ->
                        Err [ error ]
        }


createYourOwnIntro =
    md
        """
## Creating your own controls

One final issue with our `customerControl`: why the heck are we including the customer's current age? In a year's time, 
that data is going to be completely stale and useless. Instead, it would be much better to capture their date of birth. 

### Playing the dating game

The first thing we'll need is a `Date` type. There isn't one in `elm/core`, so let's go to the terminal and do 
`elm install justinmimbs/date`. 

Once the package has been installed, add a few imports to the top of the `Main.elm` module:
```
import Date
import Html
import Html.Attributes
```

Now, change our `Customer` type as follows:

```
type alias Customer = 
    { name : String
    , dateOfBirth : Date.Date
    , stickers : List Sticker
    , id : Id
    , password : String
    }
```

### Building a Date control

We _could_ pull together a date control using the combinators we've already learned - something like this:

```
boringDateControl =
    Control.record Date.fromCalendarDate
        |> Control.field Date.year
            (Control.int
                |> Control.label "Year"
            )
        |> Control.field Date.month
            (Control.int
                |> Control.label "Month"
                |> Control.map
                    { convert = Date.numberToMonth
                    , revert = Date.monthToNumber
                    }
            )
        |> Control.field Date.day
            (Control.int
                |> Control.label "Day"
            )
        |> Control.endRecord
```

(Notice that although we're using `Control.record`, we're not actually creating a record here! We're passing the values 
produced by the three fields to the `Date.fromCalendarDate` function.)

### Building a Date control _from scratch_

But let's not use `Control.record` - let's say we want to use HTML's built-in `<input type="date">` element to render 
our `Date` control. 

We can do this with `Control.create`, which gives us the flexibility to build completely bespoke controls for any Elm 
type.

```
dateControl =
    Control.create
        { label = "Date of birth"
        , initBlank = ( "1970-01-01", Cmd.none )
        , initPrefilled = \\date -> ( Date.format "yyyy-MM-dd" date, Cmd.none )
        , update = \\delta state -> ( delta, Cmd.none )
        , view =
            \\{ state, id, label, name, class } ->
                [ Html.label [ Html.Attributes.for id ] [ Html.text label ]
                , Html.input
                    [ Html.Attributes.type_ "date"
                    , Html.Attributes.value state
                    , Html.Attributes.id id
                    , Html.Attributes.class class
                    , Html.Attributes.name name
                    ]
                    []
                ]
        , subscriptions = \\state -> Sub.none
        , parse =
            \\state ->
                case Date.fromIsoString state of
                    Ok date ->
                        Ok date

                    Err error ->
                        Err [ error ]
        }
```

This looks like a lot to digest, but we can take it one field at a time.

#### label : `String`
This is the default label that will be displayed on the control.

#### initBlank : `( state, Cmd delta )`
This specifies the default internal `state` of the control when it's initialised, 
together with a `Cmd` to send during initialisation if necessary. In our case, the `state` is just a `String`, and we 
don't need to send any `Cmd`s.

#### initPrefilled : `output -> ( state, Cmd delta )`
This defines how to initialise the `state` of the control from a value of its `output` type, and also send an initial 
`Cmd` if needed. In this case, we're teaching it how to turn a `Date` into a `String` and there's no `Cmd` to send.

#### update : `delta -> state -> ( state, Cmd delta )`
This is exactly like a normal Elm app's `update` function - for 
`delta`, think `Msg`, and for `state`, think `Model`. In this case, both the `state` and `delta` are `String`s, and all 
we need to do in our update function is replace the existing `state` with the new `delta`.

#### view : `{ state : state, label : String, id : String, name : String, class : String } -> List (Html delta)` 
This is very similar to a normal Elm app's `view` function, but with two differences. First, in addition to the `state`, 
it also gives us access to some other stuff that we can include in our view's HTML attributes. Second, it produces a 
list of HTML elements, rather than a single element.

#### subscriptions : `state -> Sub delta`
This is exactly like a normal Elm app's `subscriptions` function. Here, we don't 
need to manage any subscriptions, so we can just return `Sub.none`.

#### parse : `state -> Result (List String) output`
This attempts to turn the control's `state` into a value of the 
control's `output` type, returning a list of errors if it fails. In this case, it's trying to parse a `String` into a 
`Date`.

### Wiring it up

Finally, let's update `customerControl` to replace the `age` field with our new `dateOfBirth` field:

```
customerControl = 
    Control.record
        (\\name dateOfBirth stickers id password ->
            { name = name
            , dateOfBirth = dateOfBirth
            , stickers = stickers
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .dateOfBirth dateControl
        |> Control.field .stickers stickerListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
        |> htmlBefore createYourOwnIntro
        |> htmlAfter createYourOwnOutro
```

And the final result should look like this:
"""


createYourOwnOutro =
    md
        """
Now our customer form is done... but to make it useful, we're going to want to embed it into a bigger Elm app. How can 
we do that?
"""



leavingTheSandbox =
    customerControl
        |> htmlBefore leavingTheSandboxIntro
        |> htmlAfter leavingTheSandboxOutro


leavingTheSandboxIntro =
    md
        """
## Leaving the sandbox

So, we've designed our `customerControl`, and tested it out in `Control.sandbox`... but where do we go from 
there?

Well, in practice, we probably want to integrate it into a larger Elm application - in this case, the customer 
relationship management (CRM) system we're building for our sticker company.

### Initial setup

Let's rename our `Main.elm` file to `Customer.elm`, and rename `customerControl` to just `control`. Then we'll make a few 
changes to the exports:

```
module Customer exposing (Customer, Id, Sticker, control, main)
```

And we'll implement a very rubbish CRM application in a file called `Crm.elm`:

```
module Crm exposing (main)

import Browser
import Control
import Customer

type alias Model = 
    { customers : List Customer.Customer }

type Msg 
    = SoldStickerToCustomer Customer.Id Customer.Sticker

main = 
    Browser.document 
        { init = init 
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init flags = 
    ( { customers = [] }
    , Cmd.none
    )

view model =
    { title = "Sticker Company CRM"
    , body = 
        [ Html.div [] (List.map .name model.customers) ] 
    }

update msg model =
    case msg of
        SoldStickerToCustomer customerId sticker ->
            ( { customers = 
                List.map 
                    (\\customer -> 
                        if customer.id == customerId then 
                            { customer | stickers = sticker :: customer.stickers } 
                        else customer
                    ) 
                    model.customers 
              }
            , Cmd.none
            )

subscriptions model = 
    Sub.none
```

So, how do we add our form to this app? 

### Working out the types

**Warning:** this is the scariest bit of the tutorial. Take a deep breath before you read the next section.

First, we need to know what the types should be for the `state` of our form (which 
is this package's equivalent of a `Model` type), and its `delta` (equivalent to a `Msg` type).

These types will be quite complicated, and it would be painful to work them out by hand. Fortunately, we don't have to, 
because we can ask the Elm compiler to do it for us.

Open your terminal in the project root folder and type `elm repl`. Then, at the REPL prompt, type:

```
> import Customer
> Customer.main
```

This should print out the type signature for our sandbox program, which should look something like this:
```
<function>
    : Program
          ()
          (
          Control.State
              (
              Control.Record
                  (
                  Control.Field
                      String
                      (
                      Control.Field
                          String
                          (
                          Control.Field
                              (
                              Control.List_
                                  (
                                  Control.CustomType
                                      (
                                      Control.Variant
                                          Control.EndVariant
                                          (
                                          Control.Variant
                                              (Control.Arg String Control.EndVariant)
                                              (
                                              Control.Variant
                                                  (
                                                  Control.Arg
                                                      String
                                                      (
                                                      Control.Arg
                                                          String
                                                          Control.EndVariant
                                                      )
                                                  )
                                                  Control.EndCustomType
                                              )
                                          )
                                      )
                                  )
                              )
                              (
                              Control.Field
                                  (Control.Mapping String)
                                  (
                                  Control.Field
                                      (
                                      Control.Mapping
                                          (
                                          Control.Record
                                              (
                                              Control.Field
                                                  String
                                                  (
                                                  Control.Field
                                                      String
                                                      Control.EndRecord
                                                  )
                                              )
                                          )
                                      )
                                      Control.EndRecord
                                  )
                              )
                          )
                      )
                  )
              )
          )
          (
          Control.Delta
              (
              Control.Record
                  (
                  Control.Field
                      String
                      (
                      Control.Field
                          String
                          (
                          Control.Field
                              (
                              Control.List_
                                  (
                                  Control.CustomType
                                      (
                                      Control.Variant
                                          Control.EndVariant
                                          (
                                          Control.Variant
                                              (Control.Arg String Control.EndVariant)
                                              (
                                              Control.Variant
                                                  (
                                                  Control.Arg
                                                      String
                                                      (
                                                      Control.Arg
                                                          String
                                                          Control.EndVariant
                                                      )
                                                  )
                                                  Control.EndCustomType
                                              )
                                          )
                                      )
                                  )
                              )
                              (
                              Control.Field
                                  (Control.Mapping String)
                                  (
                                  Control.Field
                                      (
                                      Control.Mapping
                                          (
                                          Control.Record
                                              (
                                              Control.Field
                                                  String
                                                  (
                                                  Control.Field
                                                      String
                                                      Control.EndRecord
                                                  )
                                              )
                                          )
                                      )
                                      Control.EndRecord
                                  )
                              )
                          )
                      )
                  )
              )
          )
```

Aaargh! Right?

Don't worry, it's not as bad as it looks - and we'll get through this _together_.

The `state` for our form will be the whole section that starts with `Control.State`, and the `delta` will be the 
section that starts with `Control.Delta`.

Let's copy-paste those relevant bits into a couple of type aliases in `Crm.elm`:

```
type alias CustomerFormState =
    Control.State
        (Control.Record
            (Control.Field
                String
                (Control.Field
                    String
                    (Control.Field
                        (Control.List_
                            (Control.CustomType
                                (Control.Variant
                                    Control.EndVariant
                                    (Control.Variant
                                        (Control.Arg String Control.EndVariant)
                                        (Control.Variant
                                            (Control.Arg
                                                String
                                                (Control.Arg
                                                    String
                                                    Control.EndVariant
                                                )
                                            )
                                            Control.EndCustomType
                                        )
                                    )
                                )
                            )
                        )
                        (Control.Field
                            (Control.Mapping String)
                            (Control.Field
                                (Control.Mapping
                                    (Control.Record
                                        (Control.Field
                                            String
                                            (Control.Field
                                                String
                                                Control.EndRecord
                                            )
                                        )
                                    )
                                )
                                Control.EndRecord
                            )
                        )
                    )
                )
            )
        )
```

And:

```
type alias CustomerFormDelta =
    Control.Delta
        (Control.Record
            (Control.Field
                String
                (Control.Field
                    String
                    (Control.Field
                        (Control.List_
                            (Control.CustomType
                                (Control.Variant
                                    Control.EndVariant
                                    (Control.Variant
                                        (Control.Arg String Control.EndVariant)
                                        (Control.Variant
                                            (Control.Arg
                                                String
                                                (Control.Arg
                                                    String
                                                    Control.EndVariant
                                                )
                                            )
                                            Control.EndCustomType
                                        )
                                    )
                                )
                            )
                        )
                        (Control.Field
                            (Control.Mapping String)
                            (Control.Field
                                (Control.Mapping
                                    (Control.Record
                                        (Control.Field
                                            String
                                            (Control.Field
                                                String
                                                Control.EndRecord
                                            )
                                        )
                                    )
                                )
                                Control.EndRecord
                            )
                        )
                    )
                )
            )
        )
```

Phew - job done! Now we don't have to think about those horrible types again.

### Extending the `Model` and `Msg` types

Now, in `Crm.elm`, we'll add a field to the `Model` to hold the form's state:

```
type alias Model = 
    { customers : List Customer.Customer 
    , customerFormState : CustomerFormState
    }

```

Next, we'll add two new variants to the `Msg` type - one for updating the form's state, and one for submitting it:

```
type Msg 
    = SoldStickerToCustomer Customer.Id Customer.Sticker
    | UpdatedCustomerForm CustomerFormDelta
    | SubmittedCustomerForm
```

### Instantiating our form

Now, in `Crm.elm`, let's use `Control.simpleForm` to turn our `control` into a basic form that will render as an HTML 
`<form>` element, with a submit button at the bottom:

```
customerForm = 
    Control.simpleForm 
        { control = Customer.control
        , onUpdate = UpdatedCustomerForm
        , onSubmit = SubmttedCustomerForm
        }
```

This `customerForm` is a record that contains the functions we'll need to bring our form to life. Next, we'll integrate 
these functions into our CRM app's `init`, `view`, `update` and `subscriptions` functions. 

### Wiring it up

Let's start with our app's `init` function:

```
init flags = 
    let
        ( formState, cmd ) = 
            customerForm.blank
    in
    ( { customers = [] 
      , customerFormState = formState
      }
    , cmd
    )
```

Now `view`:

```
view model =
    { title = "Sticker Company CRM"
    , body = 
        [ Html.div [] (List.map .name model.customers) 
        , customerForm.view model.customerFormState
        ] 
    }
```

And `update`:

```
update msg model =
    case msg of
        SoldStickerToCustomer customerId sticker ->
            ...

        UpdatedCustomerForm delta ->
            let
                ( newFormState, cmd ) =
                    customerForm.update delta model.customerFormState
            in
            ( { model | customerFormState = newFormState }
            , cmd
            )

        SubmittedCustomerForm ->
            let
                ( newFormState, result ) =
                    customerForm.submit model.customerFormState
            in
            case result of
                Ok customer ->
                    ( { model 
                        | customers = customer :: model.customers 
                        , customerFormState = newFormState
                      }
                    , Cmd.none
                    )
                Err errors ->
                    -- in a real app you'd probably do something 
                    -- with the errors, but I'll leave that as an
                    -- exercise for the reader; here, we'll just
                    -- update the form's state.
                    ( { model 
                        | customerFormState = newFormState
                      }
                    , Cmd.none
                    )
```

And finally, `subscriptions`:

```
subscriptions model = 
    customerForm.subscriptions model.customerFormState
```

Voila! Job done! If you open `Crm.elm` in `elm reactor`, you should now see a list of customer names, followed by 
something like this:
"""


leavingTheSandboxOutro =
    md
        """
Congratulations! You made it through the tutorial. There's quite a lot more to learn about this package, but that's 
beyond the scope of this introduction. For a deeper dive, check out the docs at 
[package.elm-lang.org](https://package.elm-lang.org/packages/edkelly303/elm-any-type-forms/latest).
"""



{-
   .88b  d88.  .d8b.  d8888b. db   dD d8888b.  .d88b.  db   d8b   db d8b   db      db   db d88888b db      d8888b. d88888b d8888b. .d8888.
   88'YbdP`88 d8' `8b 88  `8D 88 ,8P' 88  `8D .8P  Y8. 88   I8I   88 888o  88      88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
   88  88  88 88ooo88 88oobY' 88,8P   88   88 88    88 88   I8I   88 88V8o 88      88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
   88  88  88 88~~~88 88`8b   88`8b   88   88 88    88 Y8   I8I   88 88 V8o88      88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
   88  88  88 88   88 88 `88. 88 `88. 88  .8D `8b  d8' `8b d8'8b d8' 88  V888      88   88 88.     88booo. 88      88.     88 `88. db   8D
   YP  YP  YP YP   YP 88   YD YP   YD Y8888D'  `Y88P'   `8b8' `8d8'  VP   V8P      YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'


-}


htmlBefore str =
    Control.wrapView (\v -> str :: v)


htmlAfter str =
    Control.wrapView (\v -> v ++ [ str ])


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
        Ok [ rendered ] ->
            rendered

        Ok rendered ->
            H.div [] rendered

        Err errors ->
            H.text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
