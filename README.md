# Elm forms

A library for making user input forms, composed from controls of any 
(non-recursive) Elm type.

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


main : Browser.Program
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
