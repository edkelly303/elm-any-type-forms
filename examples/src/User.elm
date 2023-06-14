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
        |> Control.field "Name" .name (Control.string |> Control.failIf String.isEmpty "Name is required")
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
        |> Control.tag1 "Admin" AdminLevel (Control.int |> Control.label "Security clearance level")
        |> Control.end


type alias Model =
    { formState : FormState
    }


type Msg
    = FormUpdated FormDelta
    | FormSubmitted


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


userForm =
    Control.form
        { control = userControl
        , title = "Let's create a user!"
        , onUpdate = FormUpdated
        , onSubmit = FormSubmitted
        }


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                let
                    ( formState, formCmd ) =
                        userForm.init
                in
                ( { formState = formState }
                , formCmd
                )
        , view =
            \model ->
                userForm.view model.formState
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
                            ( newFormState, Ok user ) ->
                                -- in a real app, you'd probably do something
                                -- with the `user` here.
                                ( { model | formState = newFormState }
                                , Cmd.none
                                )

                            ( newFormState, Err errors ) ->
                                -- in a real app, you might choose to do
                                -- something with the `errors` here.
                                ( { model | formState = newFormState }
                                , Cmd.none
                                )
        , subscriptions =
            \model ->
                userForm.subscriptions model.formState
        }
