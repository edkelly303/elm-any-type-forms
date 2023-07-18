module Docs exposing (main)

import Browser
import Control
import Date
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Markdown.Parser as Markdown
import Markdown.Renderer


main =
    Browser.document
        { init =
            \() ->
                let
                    ( initialForm, cmd ) =
                        form.init
                in
                ( { form = initialForm
                  , output = Nothing
                  }
                , cmd
                )
        , update =
            \msg model ->
                case msg of
                    Nothing ->
                        let
                            ( newForm, result ) =
                                form.submit model.form
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
                                form.update delta model.form
                        in
                        ( { model | form = newForm }
                        , cmd
                        )
        , view =
            \model ->
                { title = "elm-any-type-forms tutorial"
                , body =
                    [ H.div [] [ form.view model.form ]
                    ]
                }
        , subscriptions = \model -> form.subscriptions model.form
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
                YourFirstForm data ->
                    l01 data

                BasicControls data ->
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
        |> Control.tag1 "Your first form" YourFirstForm yourFirstForm
        |> Control.tag1 "Basic controls" BasicControls basicControls
        |> Control.tag1 "Tuples and triples" TuplesAndTriples tuplesAndTriples
        |> Control.tag1 "Records and labels" Records records
        |> Control.tag1 "Custom types" CustomTypes customTypes
        |> Control.tag1 "Lists, Dicts, Sets & Arrays" ListsDictsSetsAndArrays listsDictsSetsAndArrays
        |> Control.tag1 "Converting controls" Mapping mapping
        |> Control.tag1 "Validating controls" Validation validation
        |> Control.tag1 "Multi-control validation" MultiValidation multivalidation
        |> Control.tag1 "Creating your own controls" CreateYourOwn createYourOwn
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
                        List.concatMap
                            (\sc ->
                                if sc.index == config.selected then
                                    sc.html

                                else
                                    []
                            )
                            subcontrols

                    backNext =
                        [ if config.selected == (List.length subcontrols) then
                            H.text ""

                          else
                            H.button
                                [ HA.id "next-button"
                                , HA.type_ "button"
                                , HE.onClick (config.selectMsg (config.selected + 1))
                                ]
                                [ H.text "Next" ]
                        ]
                in
                [ navBar, H.div [ HA.id "lesson-page" ] (subcontrolViews ++ backNext) ]
            )
        |> Control.label "Lessons"
        |> Control.id "lessons"
        |> mdBefore "# An introduction to `elm-any-type-forms`"


yourFirstForm =
    Control.bool
        |> mdBefore """
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
        |> mdAfter """
(Although the styling will be different, because `elm reactor` doesn't include any CSS.)

Next up, let's take a look at some of the other basic controls included in this package.
"""


basicControls =
    Control.record (\bool string char int float -> { bool = bool, string = string, char = char, int = int, float = float })
        |> Control.field .bool
            (Control.bool |> mdBefore """
As we've already seen, there's `Control.bool`, which we render using a standard HTML `<input type="checkbox">` 
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
        |> Control.endRecord
        |> mdBefore """
## Basic controls
The package includes simple controls for all of Elm's primitive types: `Bool`, 
`String`, `Char`, `Int` and `Float`.
"""
        |> mdAfter """
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
        |> Control.field (\() -> ( 1, "hello", 1.0 )) (Control.triple Control.int Control.string Control.float |> mdBefore tripleIntro)
        |> Control.endRecord
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
            |> Control.endRecord
            |> mdAfter recordMiddle
        )
        (Control.record (\name age -> { name = name, age = age })
            |> Control.field .name (Control.string |> Control.label "Name")
            |> Control.field .age (Control.int |> Control.label "Age")
            |> Control.endRecord
        )
        |> Control.layout (\config subcontrols -> List.concatMap .html subcontrols)
        |> mdBefore recordIntro
        |> mdAfter recordOutro


recordIntro =
    """
## Records and labels

Imagine we are building a customer relationship management system for a company called Shapes.com. The company sells 
a variety of two-dimensional geometric shapes to happy customers worldwide.

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


type Product
    = Circle Int
    | Triangle Int Int Int
    | Rectangle Int Int


customTypes =
    customTypesCustomerControl
        |> mdBefore customTypesIntro
        |> mdAfter customTypesOutro


customTypesCustomerControl =
    Control.record
        (\name age product ->
            { name = name
            , age = age
            , product = product
            }
        )
        |> Control.field .name (Control.string |> Control.label "Name")
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.field .product productControl
        |> Control.endRecord


productControl =
    Control.customType
        (\circle triangle rectangle tag ->
            case tag of
                Circle radius ->
                    circle radius

                Triangle side1 side2 side3 ->
                    triangle side1 side2 side3

                Rectangle width height ->
                    rectangle width height
        )
        |> Control.tag1 "Circle"
            Circle
            (Control.int |> Control.label "Radius")
        |> Control.tag3 "Triangle"
            Triangle
            (Control.int |> Control.label "First side")
            (Control.int |> Control.label "Second side")
            (Control.int |> Control.label "Third side")
        |> Control.tag2 "Rectangle"
            Rectangle
            (Control.int |> Control.label "Width")
            (Control.int |> Control.label "Height")
        |> Control.endCustomType
        |> Control.label "Product"


customTypesIntro =
    """
## Custom types

Shapes.com sells circles, triangles and rectangles to its customers. The company's unique selling point is that it 
can custom-engineer these shapes in any size the customer desires! 

We need to capture the required dimensions of each shape in our system, to be sure that we're giving the customer 
exactly what they want. So we'll specify circles by their radius (a single `Int`), triangles by the lengths of their 
sides (three `Int`s), and rectangles by their width and height (two `Ints`):

```
type Product
    = Circle Int
    | Triangle Int Int Int
    | Rectangle Int Int
```

Let's see how we can build a control to represent these exciting products with `Control.customType`. This might look a 
bit daunting at first, but we'll walk through it step by step:

```
productControl =
    
    -- First, we call `Control.customType` and pass it a function that can 
    -- destructure a `Product` tag and give us access to its arguments.
    
    Control.customType
        (\\circle triangle rectangle tag ->
            case tag of
                Circle radius ->
                    circle radius

                Triangle side1 side2 side3 ->
                    triangle side1 side2 side3

                Rectangle width height ->
                    rectangle width height
        )

        -- Next, we teach the control how to construct a `Circle` from a single
        -- `Control.int` control, using `Control.tag1`.
        
        |> Control.tag1 "Circle"
            Circle
            (Control.int |> Control.label "Radius")

        -- Now we do the same for `Triangle` - this time, it's composed of three
        -- `Control.int` controls, so we use `Control.tag3`.

        |> Control.tag3 "Triangle"
            Triangle
            (Control.int |> Control.label "First side")
            (Control.int |> Control.label "Second side")
            (Control.int |> Control.label "Third side")

        -- And finally, we handle `Rectangle`'s two `Control.int` controls with 
        -- `Control.tag2`.

        |> Control.tag2 "Rectangle"
            Rectangle
            (Control.int |> Control.label "Width")
            (Control.int |> Control.label "Height")

        -- Now just call `Control.endCustomType` to declare that we've finished adding 
        -- tags, and then `Control.label` to give the control an appropriate 
        -- label.
        
        |> Control.endCustomType
        |> Control.label "Product"
```

### Wiring it up

Now we can add the new field to our `Customer` control as follows:

```
customerControl =
    Control.record 
        (\\name age product -> 
            { name = name
            , age = age
            , product = product
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .product productControl
        |> Control.endRecord
```

And you'll see something like this:
"""


customTypesOutro =
    """
### Maybe and Result
You could easily implement Elm's `Maybe` and `Result` custom types using `Control.customType`. But 
there's no need - they're included as `Control.maybe` and `Control.result`.

Next up, we'll look at controls for data structures that can include multiple values of a given type: `List`, and other 
list-like things.

"""


listsDictsSetsAndArrays =
    productListControl
        |> mdBefore listsIntro
        |> mdAfter listsOutro


productListControl =
    Control.list productControl
        |> Control.label "List of products"


listsIntro =
    """
## Lists, Dicts, Sets and Arrays

Hang on a minute - if each Shapes.com customer can only purchase a single product, the company is probably not going to
be very successful! 

What we really want our system to do is keep track of _all_ the products that each customer buys. Perhaps we could use 
some nifty data structure like a `List`?

```
type alias Customer = 
    { name : String
    , age : Int 
    , products : List Product
    , id : Id
    }
```

Fortunately, it's easy to turn any control into a list of controls by passing it to `Control.list`:

```
productListControl = 
    Control.list productControl
```

This will give you a form that produces a list of products:
"""


listsOutro =
    """
### Wiring it up 

Now you can add your new `productListControl` to your `customerControl` as follows:

```
customerControl =
    Control.record 
        (\\name age products -> 
            { name = name
            , age = age
            , products = products
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .products productListControl
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
    , products : List Product
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
### Wiring it up

You can add this new field to your `Customer` control as follows:

```
customerControl =
    Control.record 
        (\\name age products id -> 
            { name = name
            , age = age
            , products = products 
            , id = id
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .products productListControl
        |> Control.field .id idControl
        |> Control.endRecord
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
        (\name age products id password ->
            { name = name
            , age = age
            , products = products
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age (Control.int |> Control.label "Age")
        |> Control.field .products productListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
        |> mdBefore multivalidationIntro
        |> mdAfter multivalidationOutro


passwordControl =
    Control.record (\choose confirm -> { choose = choose, confirm = confirm })
        |> Control.field .choose choosePasswordControl
        |> Control.field .confirm confirmPasswordControl
        |> Control.endRecord
        |> Control.alertIf
            (\{ choose, confirm } -> choose /= confirm)
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
            }


multivalidationIntro =
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
    , products : List Product
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
        (\\name age products id password ->
            { name = name
            , age = age
            , products = products
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .products productListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
```

And you should see something a little like this:
"""


multivalidationOutro =
    """
We're nearly done with this tutorial - just one more lesson to go. The final thing we'll cover is what to do when you 
want to create a completely new type of control from scratch.
"""


createYourOwn =
    Control.record
        (\name dateOfBirth products id password ->
            { name = name
            , dateOfBirth = dateOfBirth
            , products = products
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .dateOfBirth dateControl
        |> Control.field .products productListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
        |> mdBefore createYourOwnIntro
        |> mdAfter createYourOwnOutro


dateControl =
    Control.create
        { label = "Date of birth"
        , initEmpty = ( "1970-01-01", Cmd.none )
        , initWith = \date -> ( Date.format "yyyy-MM-dd" date, Cmd.none )
        , update = \delta state -> ( delta, Cmd.none )
        , view =
            \{ state, id, label, name, class } ->
                [ H.label [ HA.for id ] [ H.text label ]
                , H.input
                    [ HA.type_ "date"
                    , HA.value state
                    , HA.id id
                    , HA.class class
                    , HA.name name
                    ]
                    []
                ]
        , subscriptions = \state -> Sub.none
        , parse =
            \state ->
                case Date.fromIsoString state of
                    Ok date ->
                        Ok date

                    Err error ->
                        Err [ error ]
        }
        




createYourOwnIntro =
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
    , products : List Product
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
        , initEmpty = ( "1970-01-01", Cmd.none )
        , initWith = \\date -> ( Date.format "yyyy-MM-dd" date, Cmd.none )
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

#### initEmpty : `( state, Cmd delta )`
This specifies the default internal `state` of the control when it's initialised, 
together with a `Cmd` to send during initialisation if necessary. In our case, the `state` is just a `String`, and we 
don't need to send any `Cmd`s.

#### initWith : `output -> ( state, Cmd delta )`
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
        (\\name dateOfBirth products id password ->
            { name = name
            , dateOfBirth = dateOfBirth
            , products = products
            , id = id
            , password = password
            }
        )
        |> Control.field .name nameControl
        |> Control.field .dateOfBirth dateControl
        |> Control.field .products productListControl
        |> Control.field .id idControl
        |> Control.field .password passwordControl
        |> Control.endRecord
        |> mdBefore createYourOwnIntro
        |> mdAfter createYourOwnOutro
```

And the final result should look like this:
"""


createYourOwnOutro =
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


mdBefore str =
    Control.wrapView (\v -> md str :: v)


mdAfter str =
    Control.wrapView (\v -> v ++ [ md str ])


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
