module User exposing (main)

import Browser
import Control


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
    { state :
        Control.State
            ( Control.State String
            , ( Control.State String
              , ( Control.State
                    ( Control.State ()
                    , ( Control.State
                            ( Control.State String
                            , Control.End
                            )
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
        (Control.Delta
            ( Control.Delta String
            , ( Control.Delta String
              , ( Control.Delta
                    ( Control.Delta ()
                    , ( Control.Delta
                            ( Control.Delta String
                            , Control.End
                            )
                      , Control.End
                      )
                    )
                , Control.End
                )
              )
            )
        )
    | FormSubmitted


userForm =
    Control.form
        { control = userControl
        , title = "Let's make a User"
        , onUpdate = FormUpdated
        , onSubmit = FormSubmitted
        }


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                let
                    ( state, cmd ) =
                        userForm.init
                in
                ( { state = state }, cmd )
        , view =
            \model ->
                userForm.view model.state
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
        , subscriptions =
            \_ -> Sub.none
        }
