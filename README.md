# Any-type forms

## DISCLAIMER

I am uneasy about publishing this package in its current rough and 
unfinished state. However, impending fatherhood means I won't be doing much 
programming over the next few months, and I wanted to share the work I've done 
so far.

I would *not* recommend using this package in production, and you'll probably 
see why if you read to the end of this document.

Nevertheless, I think the techniques I used to implement control combinators 
with nested tuples may be of interest to people who like using Elm to do weird, 
unElmish things.

Perhaps by the time our baby lets me get back to coding again, someone will 
have been inspired to build something less ridiculous and more useful with 
nested tuples.

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
more complex types. In addition to standard combinators for `List`, `Dict`, 
`Maybe`, `Tuple` and `Wrapper` types, you can also create combinators 
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
* The types of the forms it generates are a bit unintuitive (see "How do I include a form in my `Model` and `Msg` types?").
* The type signatures of some of the functions in the `Control` module are... interesting.
* The implementation is pretty difficult to understand.

## How do I build up some controls?

Here's an example that shows the `record` and `customType` combinators in action:

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

## How do I turn a control into a form and wire it into my Elm app?

The big tradeoff of this package is that its forms build up quite large and 
complex `State` and `Delta` types (which are the equivalent of an Elm program's 
`Model` and `Msg` types, respectively). 

When you want to hook up a form in your Elm application, your `Model` and `Msg` 
will need to include those `State` and `Delta` types - which means you have to 
be able to work out what they are.

Fortunately, the Elm compiler has our back here, and we can use it to tell us 
what types we need. We'll give it some deliberately incorrect type annotations, 
and see what error messages it gives us.

```elm
type alias Model = 
    { state : () } -- we'll get an error here, because the form's `state` won't be `()`


type Msg 
    = FormUpdated () -- we'll get an error here, because the form's `delta` won't be `()`
    | FormSubmitted


userForm =
    Control.toForm
        "Let's make a User"
        FormUpdated
        FormSubmitted
        userControl


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( { state = userForm.init }, Cmd.none )
        , view = \model -> userForm.view model.state
        , update =
            \msg model ->
                case msg of
                    FormUpdated delta ->
                        let
                            ( state, cmd ) =
                                userForm.update delta model.state
                        in
                        ( { model | state = state }, cmd )

                    _ ->
                        ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
```

When you compile this, you should get some errors like this:

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
                  ( Control.State String
                  , ( Control.State String
                    , ( Control.State
                            ( Control.State ()
                            , ( Control.State
                                    ( Control.State String, Control.End )
                              , Control.End
                              )
                            )
                      , Control.End
                      )
                    )
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

So, your types are going to be:

```elm
type alias Model = 
    { state : Control.State
                  ( Control.State String
                  , ( Control.State String
                    , ( Control.State
                            ( Control.State ()
                            , ( Control.State
                                    ( Control.State String, Control.End )
                              , Control.End
                              )
                            )
                      , Control.End
                      )
                    )
                  ) 
    }


type Msg 
    = FormUpdated 
        (  Control.Delta
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
        ))
    | FormSubmitted
```

Yeah. Do you hate it yet?