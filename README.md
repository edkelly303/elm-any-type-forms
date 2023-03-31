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

## What's nice about it?

* Simple API(?)
* Composable
* Completely custom controls, with whatever types you like
* Less wiring to make simple changes (such as adding a field to a record)
* Built-in validation (including multi-field validation)
* Built-in debouncing

## What's nasty about it?

* Default controls don't look nice (this is fixable, just needs more work)
* Form types are confusing (see "How do I include a form in my `Model` and `Msg` types?")
* The types of some of the functions in the `Control` module are... interesting
* The implementation is pretty difficult to understand

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

## How do I include a form in my `Model` and `Msg` types?

The big tradeoff with this package is that its forms build up quite large and 
complex `State` and `Delta` types (which are the equivalent of an Elm program's 
`Model` and `Msg` types). 

When you want to hook up a form in your Elm application, your `Model` and `Msg` 
will need to include those `State` and `Delta` types - which means you have to 
be able to work out what they are.

Fortunately, the Elm compiler has our back here, and we can use it to tell us 
what types we need. We'll give it some deliberately incorrect type annotations, 
and see what it gives us as an error message:

```elm
type alias Model = 
    { state : () } -- we'll get an error here, because the form's `state` won't be `()`


type Msg 
    = FormUpdated () -- we'll get an error here, because the form's `delta` won't be `()`
    | FormSubmitted


myForm =
    Control.toForm
        "My form"
        FormUpdated
        FormSubmitted
        myControl


myControl =
    Control.record
        (\name age -> { name = name, age = age })
        |> Control.field "Name" .name Control.string
        |> Control.field "Age" .age Control.int
        |> Control.end


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( { state = myForm.init }, Cmd.none )
        , view = \model -> myForm.view model.state
        , update =
            \msg model ->
                case msg of
                    FormUpdated delta ->
                        let
                            ( state, cmd ) =
                                myForm.update delta model.state
                        in
                        ( { model | state = state }, cmd )

                    _ ->
                        ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
```

And you should get some errors like this:

```text
-- TYPE MISMATCH ----------------------------------------------

Something is off with the body of the `main` definition:

    Browser.element
        { init = \() -> ( { state = myForm.init }, Cmd.none )
        , view = \model -> myForm.view model.state
        , update =
            \msg model ->
                case msg of
                    FormUpdated delta ->
                        let
                            ( state, cmd ) =
                                myForm.update delta model.state
                        in
                        ( { model | state = state }, cmd )

                    _ ->
                        ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }

This `element` call produces:

    Program
        ()
        { state :
              Control.State
                  ( Control.State String, ( Control.State String, Control.End )
                  )
        }
        Msg

But the type annotation on `main` says it should be:

    Program () Model Msg

-- TYPE MISMATCH ----------------------------------------------

The 1st argument to this function is not what I expect:

                                myForm.update delta model.state
                                              ^^^^^
This `delta` value is a:

    ()

But this function needs the 1st argument to be:

    Control.Delta
        ( Control.Delta String, ( Control.Delta String, Control.End ) )
```

So, your types are going to be:

```elm
type alias Model = 
    { state : Control.State ( Control.State String, ( Control.State String, Control.End ) ) 
    }


type Msg 
    = FormUpdated (Control.Delta ( Control.Delta String, ( Control.Delta String, Control.End ) ))
    | FormSubmitted
```