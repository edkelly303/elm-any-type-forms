module Docs exposing (main)

import Browser
import Control
import Html as H
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
    Control.form
        { control = lessons
        , onSubmit = Nothing
        , onUpdate = Just
        }


type Lessons l01 l02 l03 l04 l05 l06 l07 l08 l09 l10
    = BasicControls l01 --BasicControls
    | YourFirstForm l02 --TuplesAndTriples
    | TuplesAndTriples l03 --TuplesAndTriples
    | Records l04 --Records
    | CustomTypes l05 --CustomTypes
    | ListsDictsSetsAndArrays l06 --DictsSetsAndArrays
    | Mapping l07 --Mapping
    | Validation l08 --Validation
    | MultiValidation l09 --MultiValidation
    | CreateYourOwn l10 --CreateYourOwn


lessons =
    Control.customType
        (\l01 l02 l03 l04 l05 l06 l07 l08 l09 l10 tag ->
            case tag of
                BasicControls data ->
                    l01 data

                YourFirstForm data ->
                    l02 data

                TuplesAndTriples data ->
                    l03 data

                Records data ->
                    l04 data

                CustomTypes data ->
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
        )
        |> Control.tag1 "1: Basic controls" BasicControls basicControls
        |> Control.tag1 "2: Your first form" YourFirstForm yourFirstForm
        |> Control.tag1 "3: Tuples and triples" TuplesAndTriples tuplesAndTriples
        |> Control.tag1 "4: Records and labels" Records records
        |> Control.tag1 "5: Custom types" CustomTypes customTypes
        |> Control.tag1 "6: Lists, Dicts, Sets, and Arrays" ListsDictsSetsAndArrays listsDictsSetsAndArrays
        |> Control.tag1 "7: Converting control types" Mapping mapping
        |> Control.tag1 "8: Validating controls" Validation validation
        |> Control.tag1 "9: Multi-control validation" MultiValidation multivalidation
        |> Control.tag1 "10: Creating your own controls" CreateYourOwn createYourOwn
        |> Control.end
        |> Control.label "Lessons"
        |> mdBefore "# Tutorial"


basicControls =
    Control.record (\bool string char int float -> { bool = bool, string = string, char = char, int = int, float = float })
        |> Control.field .bool
            (Control.bool |> mdBefore """
The most basic control is probably `Control.bool`, which we render using a standard HTML `<input type="checkbox">` 
element.""")
        |> Control.field .string
            (Control.string |> mdBefore """
`Control.string` is rendered as `<input type="text">`.""")
        |> Control.field .char
            (Control.char |> mdBefore """
`Control.char` is very similar, except that it provides built-in validation to ensure that the user enters exactly one 
character.""")
        |> Control.field .int
            (Control.int |> mdBefore """
`Control.int` and `Control.float` are both rendered as `<input type="number">`. Both provide built-in validation to 
ensure that the user enters the right type of number.""")
        |> Control.field .float Control.float
        |> Control.end
        |> mdBefore """
## Basic controls
Let's start by looking at simple controls for Elm's primitive types: `Bool`, 
`String`, `Char`, `Int` and `Float`.
"""
        |> mdAfter """
All controls are displayed with `<label>` elements to help with accessibility. Each control is wrapped in a 
`<div class="control-container">`, which contains the label, the input, and potentially also a 
`<div class="control-feedback-container">` that contains a list of feedback from validation.

But how do we actually use a control? First, we need to convert it into a form..."""


yourFirstForm =
    Control.bool
        |> mdBefore """
## Your first form
Let's get up and running by building the simplest possible thing: a form that consists of just a single `Bool` input.

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
        |> mdAfter """
(Although the styling will be different, because `elm reactor` doesn't include any CSS.)

Try swapping `Control.bool` for `Control.string` or `Control.int` to see different types of controls in action.

However, most useful forms contain more than one control. How can we _combine_ controls to make something a bit more 
interesting?
"""


tuplesAndTriples =
    Control.record (\_ _ -> ())
        |> Control.field (\() -> ( 1, "hello" )) (Control.tuple Control.int Control.string)
        |> Control.field (\() -> ( 1, "hello", 1.0 )) (Control.triple Control.int Control.string Control.float |> mdBefore tripleIntro)
        |> Control.end
        |> mdBefore tuplesAndTriplesIntro
        |> mdAfter tupleAndTripleOutro


tuplesAndTriplesIntro =
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
    """
Triples work too - if you change your code to: 

```
control = 
    Control.triple Control.int Control.string Control.float
```

You'll get an `( Int, String, Float )` triple like this:
"""


tupleAndTripleOutro =
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
            |> Control.end
            |> mdAfter recordMiddle
        )
        (Control.record (\name age -> { name = name, age = age })
            |> Control.field .name (Control.string |> Control.label "Name")
            |> Control.field .age (Control.int |> Control.label "Age")
            |> Control.end
        )
        |> Control.layout
            (\kids static _ ->
                Maybe.withDefault [] (Maybe.map List.singleton static.before)
                    ++ kids
                    ++ Maybe.withDefault [] (Maybe.map List.singleton static.after)
            )
        |> mdBefore recordIntro
        |> mdAfter recordOutro


recordIntro =
    """
## Records and labels

Imagine we have a record type:

```
type alias Customer = 
    { name : String
    , age : Int 
    }
```

We can build a control that produces these `Customer` records with the `Control.record` combinator:

```    
control =
    Control.record (\\name age -> { name = name, age = age })
        |> Control.field .name Control.string
        |> Control.field .age Control.int
        |> Control.end
```

Or if you prefer brevity to explicitness, you could even use the `Customer` constructor directly:

```
control =
    Control.record Customer
        |> Control.field .name Control.string
        |> Control.field .age Control.int
        |> Control.end
```
This code will generate a form that looks like this:
"""


recordMiddle =
    """
That's ok...ish. But one of the nice things about records is that their fields are _named_. So really, we want the 
controls to be labelled with the names of the fields. 

That's where `Control.label` comes in. Change your code to:

```    
control =
    Control.record (\\name age -> { name = name, age = age })
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.end
```

And you should now see something like this:
"""


recordOutro =
    """
**Note:** We're going to see other functions that work like `Control.label` later - this is a common pattern for 
configuring controls. 

To keep things tidy, it's often better to pull out each control into a separate function, where 
you can apply as many configuration functions as you like without making your `Control.record` definitions too complex. 

With that in mind, let's refactor our code to this:

```    
control =
    Control.record (\\name age -> { name = name, age = age })
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.end

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


type SecurityChallenge
    = FavouriteWord String
    | PIN Char Char Char


customTypes =
    customTypesCustomerControl
        |> mdBefore customTypesIntro
        |> mdAfter customTypesOutro


customTypesCustomerControl =
    Control.record
        (\name age securityChallenge ->
            { name = name
            , age = age
            , securityChallenge = securityChallenge
            }
        )
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.field .securityChallenge securityChallengeControl
        |> Control.end


securityChallengeControl =
    Control.customType
        (\favouriteWord pin tag ->
            case tag of
                FavouriteWord string ->
                    favouriteWord string

                PIN digit1 digit2 digit3 ->
                    pin digit1 digit2 digit3
        )
        |> Control.tag1 "Favourite word"
            FavouriteWord
            (Control.string |> Control.label "What is your favourite word?")
        |> Control.tag3 "PIN"
            PIN
            (Control.char |> Control.label "First digit")
            (Control.char |> Control.label "Second digit")
            (Control.char |> Control.label "Third digit")
        |> Control.end
        |> Control.label "Security challenge"


customTypesIntro =
    """
## Custom types

Let's imagine that we want to improve security by asking our `Customer`s to nominate either a favourite word or a 
three-digit PIN that we can use to verify their identity.

```
type alias Customer = 
    { name : String
    , age : Int 
    , securityChallenge : SecurityChallenge
    }

type SecurityChallenge
    = FavouriteWord String
    | PIN Char Char Char
```

We can create a control for this `SecurityChallenge` type using `Control.customType`. 

```
securityChallengeControl = 
    Control.customType
        (\\favouriteWord pin tag ->
            case tag of
                FavouriteWord string ->
                    favouriteWord string

                PIN digit1 digit2 digit3 ->
                    pin digit1 digit2 digit3
        )
        |> Control.tag1 "Favourite word" 
            FavouriteWord 
            (Control.string |> Control.label "Customer's favourite word")
        |> Control.tag3 "PIN"
            PIN
            (Control.char |> Control.label "First digit of PIN")
            (Control.char |> Control.label "Second digit of PIN")
            (Control.char |> Control.label "Third digit of PIN")
        |> Control.end
        |> Control.label "Security challenge"
```

(Notice how the `FavouriteWord` tag has one argument (a `String`), so we use `Control.tag1`. But for the `PIN` tag, which 
has three arguments, we use `Control.tag3`.)

Now we can add the new field to our `Customer` control as follows:
```
control =
    Control.record 
        (\\name age securityChallenge -> 
            { name = name
            , age = age
            , securityChallenge = securityChallenge 
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .securityChallenge securityChallengeControl
        |> Control.end
```

And you'll see something like this:
"""


customTypesOutro =
    """
**Side note:** You could easily implement Elm's `Maybe` and `Result` custom types using `Control.customType`. But 
there's no need - they're included as `Control.maybe` and `Control.result`.

Next up, we'll look at controls for data structures that can include multiple values of a given type: `List`, and other 
list-like things.

"""


listsDictsSetsAndArrays =
    Control.list customTypesCustomerControl
        |> Control.label "List of customers"
        |> mdBefore listsIntro
        |> mdAfter listsOutro


listsIntro =
    """
## Lists, Dicts, Sets and Arrays

It's easy to turn any control into a list of controls by passing it to `Control.list`:

```
control = 
    Control.list customerControl

customerControl =
    Control.record 
        (\\name age securityChallenge -> 
            { name = name
            , age = age
            , securityChallenge = securityChallenge 
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .securityChallenge securityChallengeControl
        |> Control.end
```

This will give you a form that produces a list of customers:
"""


listsOutro =
    """
`Control.array` and `Control.set` have exactly the same API - just pass them a control of any type and you'll get a 
control that produces an `Array` or `Set` of that type. 

`Control.dict` is similar, except that it takes _two_ controls as arguments. It uses the first as the key and the second 
as the value for the `Dict` it produces.
"""


type Id
    = Id Int


mapping =
    idControl
        |> mdBefore mappingIntro
        |> mdAfter mappingOutro


idControl =
    Control.int
        |> Control.label "ID number"
        |> Control.map
            { convert = Id
            , revert = \(Id int) -> int
            }


mappingIntro =
    """
## Converting control types

In some circumstances, you may want to convert the type produced by a control to some other type. That's where 
`Control.map` becomes useful.

For example, suppose you want each of your customers to have a unique ID number. The number itself can be a simple `Int`, 
but to make your code more type-safe, you decide to wrap that `Int` in a custom type tag:

```
type Id = 
    Id Int

type alias Customer = 
    { name : String
    , age : Int 
    , securityChallenge : SecurityChallenge
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
            { convert = Id
            , revert = \\(Id int) -> int 
            }
```
It'll look something like this:
"""


mappingOutro =
    """
And again, you can add this new field to your `Customer` control as follows:

```
customer =
    Control.record 
        (\\name age securityChallenge id -> 
            { name = name
            , age = age
            , securityChallenge = securityChallenge 
            , id = id
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .securityChallenge securityChallengeControl
        |> Control.field .id idControl
        |> Control.end
```        
"""


validation =
    nameControl
        |> mdBefore validationIntro
        |> mdAfter validationOutro


nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf (\name -> String.isEmpty name) "Name cannot be blank"
        |> Control.noteIf (\name -> String.length name == 1) "Is that the full name?"


validationIntro =
    """
## Validating controls

We've shown how we can build controls that produce pretty much any Elm type - but what if just producing any old value 
of that type isn't enough? What if we want to be more specific about which values we want our controls to accept?

It's time to introduce some validation. For example, perhaps we want to ensure that our customer's name isn't left blank. 
We can do that with a function called `Control.failIf`:

```
nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf (\\name -> String.isEmpty name) "Name cannot be blank"
```

There might also be occasions where we want to alert the user that the data they've input might not be correct - but 
we're not _certain_ that the input is actually invalid. 

In these cases, we can use `Control.noteIf`:

```
nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf (\\name -> String.isEmpty name) "Name cannot be blank"
        |> Control.noteIf (\\name -> String.length name == 1) "Is that the full name?"
```
The difference between the two functions is that `Control.failIf` will cause the control to fail validation when the 
form is submitted, while `Control.noteIf` will allow it to pass. 

There's also a difference in the HTML produced by each function. Messages produced by `Control.failIf` are assigned an 
HTML attribute `class="control-feedback-fail"`, while those produced by `Control.noteIf` are given 
`class="control-feedback-note"`.
"""


validationOutro =
    """
**Side note:** You'll notice that the field doesn't validate itself instantly when you type into it. This is because by 
default, `Control.string` is set to debounce for 500 milliseconds before it shows the results of validation. 

You can configure the debouncing interval with `Control.debounce`, providing a value in milliseconds. For example, this
will show the validation messages immediately:

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
        (\name age securityChallenge id choosePassword confirmPassword ->
            { name = name
            , age = age
            , securityChallenge = securityChallenge
            , id = id
            , choosePassword = choosePassword
            , confirmPassword = confirmPassword
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.field .securityChallenge securityChallengeControl
        |> Control.field .id idControl
        |> Control.field .choosePassword choosePasswordControl
        |> Control.field .confirmPassword confirmPasswordControl
        |> Control.end
        |> Control.throw
            { flag = "password"
            , when = \{ choosePassword, confirmPassword } -> choosePassword == confirmPassword
            }
        |> mdBefore multivalidationIntro
        |> mdAfter multivalidationOutro


choosePasswordControl =
    Control.string
        |> Control.label "Choose password"
        |> Control.catch
            { flag = "password"
            , fail = True
            , message = "Passwords must match"
            }


confirmPasswordControl =
    Control.string
        |> Control.label "Confirm password"
        |> Control.catch
            { flag = "password"
            , fail = True
            , message = "Passwords must match"
            }


multivalidationIntro =
    """
## Multi-control validation

Sometimes you might need to validate the input of one control based on the input of another. The classic example is
checking that passwords match, so let's add some password fields to our `Customer` record:

```
type alias Customer = 
    { name : String
    , age : Int 
    , securityChallenge : SecurityChallenge
    , id : Id
    , choosePassword : String
    , confirmPassword : String
    }

customerControl =
    Control.record 
        (\\name age securityChallenge id choosePassword confirmPassword -> 
            { name = name
            , age = age
            , securityChallenge = securityChallenge 
            , id = id
            , choosePassword = choosePassword
            , confirmPassword = confirmPassword
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .securityChallenge securityChallengeControl
        |> Control.field .id idControl
        |> Control.field .choosePassword choosePasswordControl
        |> Control.field .confirmPassword confirmPasswordControl
        |> Control.end

choosePasswordControl =
    Control.string
        |> Control.label "Choose password"

confirmPasswordControl =
    Control.string
        |> Control.label "Confirm password"
```
The challenge here is that `confirmPasswordControl` has no way of knowing what's been entered in 
`choosePasswordControl`, so it can't tell whether the contents of the two controls match or not.

We can solve this problem by going up a level and checking the contents of both fields in `customerControl`. If they 
don't match, we can use `Control.throw` to throw a "flag" - a message that other controls can listen out for and react 
to:

```
customerControl =
    Control.record 
        ...
        |> Control.field .choosePassword choosePasswordControl
        |> Control.field .confirmPassword confirmPasswordControl
        |> Control.end
        |> Control.throw
            { flag = "passwords-don't-match"
            , when = \\{ choosePassword, confirmPassword } -> choosePassword /= confirmPassword
            }
```

Now, we just need `confirmPasswordControl` to listen out for the `"passwords-don't-match"` flag, and respond by alerting
the user and causing the form to fail validation. We can do this using `Control.catch`:

```
confirmPasswordControl =
    Control.string
        |> Control.catch 
            { flag = "passwords-don't-match"
            , fail = True
            , message = "Passwords must match"
            }
```
"""


multivalidationOutro =
    """
"""


createYourOwn =
    Control.int



{-
   .88b  d88.  .d8b.  d8888b. db   dD d8888b.  .d88b.  db   d8b   db d8b   db      db   db d88888b db      d8888b. d88888b d8888b. .d8888.
   88'YbdP`88 d8' `8b 88  `8D 88 ,8P' 88  `8D .8P  Y8. 88   I8I   88 888o  88      88   88 88'     88      88  `8D 88'     88  `8D 88'  YP
   88  88  88 88ooo88 88oobY' 88,8P   88   88 88    88 88   I8I   88 88V8o 88      88ooo88 88ooooo 88      88oodD' 88ooooo 88oobY' `8bo.
   88  88  88 88~~~88 88`8b   88`8b   88   88 88    88 Y8   I8I   88 88 V8o88      88~~~88 88~~~~~ 88      88~~~   88~~~~~ 88`8b     `Y8b.
   88  88  88 88   88 88 `88. 88 `88. 88  .8D `8b  d8' `8b d8'8b d8' 88  V888      88   88 88.     88booo. 88      88.     88 `88. db   8D
   YP  YP  YP YP   YP 88   YD YP   YD Y8888D'  `Y88P'   `8b8' `8d8'  VP   V8P      YP   YP Y88888P Y88888P 88      Y88888P 88   YD `8888Y'


-}


mdBefore str =
    Control.htmlBefore (md str)


mdAfter str =
    Control.htmlAfter (md str)


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
            H.text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
