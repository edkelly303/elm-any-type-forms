module User exposing (main)

import Control
import Browser

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