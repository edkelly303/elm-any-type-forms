# Any-type forms

## DISCLAIMER

I am uneasy about publishing this package in its current rough and unfinished 
state. I wanted to share the work I've done so far, and I also want to try it 
out in some of my own projects and see if it's useful.

I would *not* recommend using this package in production, and you'll probably 
see why if you read the section about `State` and `Delta` types.

Nevertheless, I think the techniques I used to implement control combinators 
with nested tuples may be of interest to people who like using Elm to do weird, 
unElmish things.

## What does it do?

This package allows you to build up user input forms that you can use to create 
or update any arbitrary Elm type. Forms are composed from individual controls,
and the internal state of each control can be any arbitrary Elm type.

This gives the user a lot of flexibility to create whatever controls they
like, as well as making it easy to create forms for complex data structures 
by combining simpler controls, without too much wiring or boilerplate.

## What's in the package?

The package provides basic controls that produce primitive Elm types, such as 
`String`, `Int`, `Float`, etc. If you need a completely custom 
control, you can easily create one from scratch using `Control.create`.

It also provides combinators that allow the user to combine controls to create 
more complex types. In addition to standard combinators for `List`, `Array`, `Dict`, `Set`,
`Maybe`, `Result`, `Tuple` and `Triple` types, you can also create combinators 
for records and custom types, with an API similar to `miniBill/elm-codec`.

Finally, it provides functions to validate and debounce controls, without 
worrying about any annoying state management or book-keeping.

## What's nice about it?

* Simple(?), composable API
* Custom controls with whatever types you like
* Less wiring & boilerplate
* Built-in validation (including multi-field validation)
* Built-in debouncing

## What's nasty about it?

* The default controls don't look very nice (this is fixable, just needs some CSS).
* The error messages the compiler gives you when you get something wrong are awful.
* The types of the forms it generates are complex and weird-looking.
* The type signatures of some of the functions in the `Control` module are quite terrifying.
* The implementation is very complex, undocumented, and difficult to understand.
* There are a few known bugs - see "Known bugs and future work" at the end of this README.

# Getting started

## How do I build up some controls?

Here's an example that shows the `record` and `customType` combinators in action:

```elm
module User exposing (User, control)

import Control

type alias User = 
    { name : String
    , age : Int
    , role : Role
    }

control = 
    Control.record 
        (\name age role -> 
            { name = name
            , age = age 
            , role = role
            }
        )
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .role roleControl
        |> Control.end

nameControl = 
    Control.string 
        |> Control.label "Name"
        |> Control.failIf String.isEmpty "Name cannot be blank"

ageControl =
    Control.int
        |> Control.label "Age"
        |> Control.failIf (\age -> age < 0) "Age cannot be a negative number"
            
type Role
    = Regular
    | AdminLevel Int

roleControl = 
    Control.customType
        (\regular adminLevel tag ->
            case tag of
                Regular -> 
                    regular
                AdminLevel level ->
                    adminLevel level
        )
        |> Control.tag0 "Regular" Regular
        |> Control.tag1 "Admin" AdminLevel 
            (Control.int 
                |> Control.label "Security clearance level"
            )
        |> Control.end
```

## How do I turn a control into a form?

Taking the example of the `User.control` defined in the previous section, we'll 
start by using `Control.sandbox` to turn it into a form that we can test in the 
browser.

Make a new file called `Sandbox.elm`

```elm
module Sandbox exposing (main)

import Control
import User

main = 
    Control.sandbox 
        { control = User.control
        , outputToString = Debug.toString
        }
```

If you open this `Sandbox.elm` file in `elm reactor` (or your preferred dev 
server - try `lydell/elm-watch`, you won't regret it), you'll be able to play 
with your form and make sure it does what you want.

## How do I embed a form into a larger Elm app?

Once you're happy with your form, you'll probably want to embed it into a larger 
Elm app. This is where things get a bit tricky.

The big tradeoff of this package is that its forms build up quite large and 
complex `State` and `Delta` types (which are the equivalent of an Elm program's 
`Model` and `Msg` types, respectively). 

When using `Control.sandbox`, you don't need to worry about what those types 
are. There's no need to define them or write type annotations. 

But when you want to integrate a form into a real Elm application, your `Model` 
and `Msg` will need to include those `State` and `Delta` types somehow. So we 
need a way to work out what the types should be.

For example, let's say this `Main.elm` file is the entrypoint for our 
application:

```elm
module Main exposing (main)

import Control
import User

type Model = 
    { formState : ??? -- what `State` type do we need here?
    -- ... other `Model` fields
    }

type Msg
    = FormUpdated ??? -- what `Delta` type do we need here?
    | FormSubmitted
    | --... other `Msg` variants
```

### 1. Let `elm repl` tell us the types

Fortunately, the Elm compiler has our back; we can use it to tell us the types 
we need. In your terminal, go to your project root folder, fire up the 
`elm repl`, and ask it to tell you the type of `Sandbox.main`:

```
$ elm repl
---- Elm 0.19.1 ----------------------------------------------------------------
Say :help for help and :exit to exit! More at <https://elm-lang.org/0.19.1/repl>
--------------------------------------------------------------------------------
> import Sandbox
> Sandbox.main
<function>
    : Program
          ()
          (
          Control.State
              ( Control.State String
              , ( Control.State String
                , ( Control.State
                        ( Control.State ()
                        , ( Control.State ( Control.State String, Control.End )
                          , Control.End
                          )
                        )
                  , Control.End
                  )
                )
              )
          )
          (
          Control.Delta
              ( Control.Delta String
              , ( Control.Delta String
                , ( Control.Delta
                        ( Control.Delta ()
                        , ( Control.Delta ( Control.Delta String, Control.End )
                          , Control.End
                          )
                        )
                  , Control.End
                  )
                )
              )
          )
```

Yeah. Those are our types. Sorry not sorry.

### 2. Make some nice type aliases

Copy and paste the types from the terminal into your application to create type 
aliases for your form's `State` and `Delta` types:

```elm
module Main exposing (main)

import Control
import User

type alias Model = 
    { formState : FormState
    -- ... other `Model` fields
    }

type Msg
    = FormUpdated FormDelta
    | FormSubmitted
    | --... other `Msg` variants

type alias FormState = 
    Control.State
        ( Control.State String
        , ( Control.State String
          , ( Control.State
              ( Control.State ()
              , ( Control.State ( Control.State String, Control.End )
                , Control.End
                )
              )
            , Control.End
            )
          )
        )

type alias FormDelta = 
    Control.Delta
        ( Control.Delta String
        , ( Control.Delta String
          , ( Control.Delta
              ( Control.Delta ()
              , ( Control.Delta ( Control.Delta String, Control.End )
                , Control.End
                )
              )
            , Control.End
            )
          )
        )
```

### 3. Instantiate your form

Use `Control.form` to create an embeddable version of your form:

```elm
userForm = 
    Control.form 
        { control = User.control 
        , onUpdate = FormUpdated
        , onSubmit = FormSubmitted
        }
```

### 4. Plumb your form into your app's `main` function

Finally, integrate the form into your `main` function:

```elm
main : Program () Model Msg
main =
    Browser.element
        { init = 
            \() -> 
                let
                    (formState, formCmd) = 
                        userForm.init
                in
                ( { formState = formState 
                  , -- your app's other `Model` fields
                  }
                , Cmd.batch 
                    [ formCmd 
                    , -- your app's other initial Cmds
                    ]
                )
        , view = 
            \model -> 
                Html.div [] 
                    [ userForm.view model.formState
                    , -- your app's other view functions
                    ]
        , update =
            \msg model ->
                case msg of
                    FormUpdated delta ->
                        let
                            ( newFormState, formCmd ) =
                                userForm.update delta model.formState
                        in
                        ( { model | formState = newFormState }
                        , formCmd 
                        )

                    FormSubmitted ->
                        case userForm.submit model.formState of 
                            (newFormState, Ok user) ->
                                -- in a real app, you'd probably do something 
                                -- with the `user` here.
                                ( { model | formState = newFormState }
                                , Cmd.none 
                                )

                            (newFormState, Err errors) ->
                                -- in a real app, you might choose to do 
                                -- something with the `errors` here.
                                ( { model | formState = newFormState }
                                , Cmd.none 
                                )
                    
                    -- your app's other `Msg` variants

        , subscriptions = 
            \model -> 
                Sub.batch 
                    [ userForm.subscriptions model.formState
                    , -- your app's other subscriptions
                    ]
        }
```

## How do I run the examples?

```bash
$ cd examples
$ npm install
$ . run
```

## Known bugs and future work

- `Control.initWith` doesn't send out initial Cmds for record fields or custom 
type variants.
- Lack of consistency in list indices (should lists be 0- or 1-indexed?)
