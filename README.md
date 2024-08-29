# Any-type forms

## DISCLAIMER

This package is experimental, and the API is likely to change. It's also pretty 
weird. I would *not* recommend using it in production.

If this doesn't put you off, the documentation might because the type signatures are pretty crazy. For a gentler introduction, it's best to start with the [tutorial](https://edkelly303.github.io/elm-any-type-forms/).

## What does it do?

This package allows you to build up user input forms that you can use to create 
or update any arbitrary Elm type. Forms are composed from individual controls,
and the internal state of each control can be any arbitrary Elm type.

This gives the user a lot of flexibility to create whatever controls they
like, as well as making it easy to create forms for complex data structures 
by combining simpler controls, without too much wiring or boilerplate.

## What's in the package?

The package provides basic controls that produce primitive Elm types, such as 
`String`, `Int`, `Float`, etc. 

It also provides combinators which you can use to create more complex types of 
controls. In addition to standard combinators for `List`, `Array`, `Dict`, 
`Set`, `Maybe`, `Result`, `Tuple` and `Triple` types, you can also create 
controls for records and custom types, with an API similar to 
`miniBill/elm-codec`.

If you can't build what you need with these primitives and combinators, you can 
easily define a custom control from scratch using `Control.define`.

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

# Getting started

The best way to get started with this package is to follow the 
[tutorial](https://edkelly303.github.io/elm-any-type-forms/), which will give 
you a solid introduction to building forms and integrating them with your Elm 
apps.

It's probably best to do the tutorial before diving into the package docs on 
this site.

The tutorial itself was built using this package - it's basically one big form. 
If you want, you can run it locally by doing:

```bash
$ cd examples
$ npm install
$ . run
```
