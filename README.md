# Elm forms

A package for making user input forms that produce any arbitrary Elm type, 
composed from controls whose internal state can be any (non-recursive, 
non-function) Elm type.

## What does it do?

The package provides basic controls that produce primitive Elm types (such as 
`String`, `Int`, `Float`, etc.), and if you need a completely custom 
control, you can easily create one from scratch.

It also provides combinators that allow the user to combine controls to create 
more complex types. In addition to standard combinators for `List`, `Dict`, 
`Maybe`, `Tuple` and `Wrapper` types, you can also create combinators 
for records and custom types, with an API similar to `miniBill/elm-codec`.

## What does it look like?

### A minimal example

Here is a very basic example, showing how a form can be created from a simple 
control and wired into an Elm program:

```elm
type alias Model = 
    Control.State String


type Msg 
    = FormUpdated (Control.Delta String)
    | FormSubmitted


exampleForm : Control.Form String String String Msg
exampleForm =
    Control.toForm 
        "A form with one text input"
        FormUpdated
        FormSubmitted
        Control.string


main : Browser.Program () Model Msg
main = 
    Browser.element
        { init = \() -> ( exampleForm.init, Cmd.none )
        , view = exampleForm.view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormUpdated delta ->
            exampleForm.update delta model
            

        FormSubmitted ->
            let
                ( newForm, result ) =
                    exampleForm.submit model
            in
            case result of
                Ok output ->
                    let
                        _ =
                            Debug.log "Success!" output
                    in
                    ( newForm, Cmd.none )

                Err errors ->
                    let
                        _ =
                            Debug.log "Failure!" errors
                    in
                    ( newForm, Cmd.none )
```

## A more complex control

Here are some examples of the `record` and `customType` combinators in action.

```elm
type alias User = 
    { name : String
    , age : Int
    , role : Role
    }

userControl = 
    Control.record 
        (\name age role -> 
            { name = name
            , age = age 
            , role = role
            }
        )
        |> Control.field "Name" .name Control.string
        |> Control.field "Age" .age Control.int
        |> Control.field "Role" .role roleControl
        |> Control.end

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
        |> Control.tag1 "Admin Level" AdminLevel Control.int
        |> Control.end
```

## What's nice about this?

Typically, when you're building a form, you're not particularly interested in 
the form itself. What you're interested in is the data structure that the form 
produces (which I am calling its `output`). 

This `output` is probably a type that is part of your domain model, and gets 
used elsewhere in your codebase - like the `User` type in the example above.

However, the ugly fact is that the type you need to use to represent the 
internal state of the form might not look anything like that `output` type. 

### The `Float` problem
As a notorious example, take a simple control that produces a `Float`. Very 
probably, you want to give the user a text control where they can type 
digits, periods and hyphens. 

If you use a `Float` as the internal state of this control and store it in your 
`Model`, your user is going to have a bad time. You'll have to immediately 
convert whatever they type into a `Float` in your `update` function, and provide
a default value (probably `0`) in case their input isn't valid. 

That means as soon as they type something like `1.`, the text box will reset to 
`0`, and they'll have to go through some ridiculous shenanigans to input the 
number they actually want.

Instead, what you probably want is a text input whose internal state is a 
`String`. And when you're creating a form with lots of controls, you'll end up 
defining all your types twice over:

```elm
type alias RealThingIWant = 
    { x : Float
    , y : Float
    , z : Float
    }

type alias StupidInternalStateForControl = 
    { x : String
    , y : String
    , z : String
    }
```

And _then_ you have to initialise all these fields in your `Model` individually, 
and write a bunch of `Msg` variants to update each of them individually, and 
handle those variants in your `update` function individually, and display each 
field in your `view` individually, and yadda yadda. It's all a lot of ceremony.

### What it looks like with this package

```elm
realThingForm = 
    Control.record RealThingIWant
        |> Control.field "x" .x Control.float
        |> Control.field "y" .y Control.float
        |> Control.field "z" .z Control.float
        |> Control.end
        |> Control.toForm "How to build a real thing" FormUpdated FormSubmitted 

main : Browser.Program () Model Msg
main = 
    Browser.element
        { init = \flags -> ( realThingForm.init, Cmd.none )
        , view = \model -> realThingForm.view model
        , update = 
            \msg model ->
                case msg of 
                    FormUpdated delta -> 
                        realThingForm.update delta model
                    FormSubmitted ->
                        let 
                            (newModel, result) = 
                                realThingForm.submit model
                        in
                        (newModel, Cmd.none)
        , subscriptions = \model -> Sub.none
        }
```
Aaaand... that's kinda sorta almost it.

## But where's the catch?

Oh boy. Everything is tradeoffs, right? 

The catch, and it's a doozy, is the types. While you don't actually have to 
figure out the annotations for the form's internal `state` and `delta` types 
yourself, they are not pretty, and you'll need to declare them so that you can 
include them in the definition of your Elm program's `Model` and `Msg`.

Under the hood, this package builds up the `state` and `delta` types for 
combinators such as records and custom types using nested tuples. As an example,
the `Msg` type for `realThingForm` would be:

```elm
type alias Msg 
    = FormUpdated 
        (Control.Delta 
            ( Control.Delta String
            , ( Control.Delta String
              , ( Control.Delta String
                , Control.End
                )
              )
            )
        )
    | FormSubmitted
```

And that is a very mild, inoffensive example. With `customType`, it gets 
positively Lovecraftian.

If you really want to witness the full horror, take a look in the `Control` 
module docs for the type annotation on the `tag0`, `tag1` and `tag2` functions.