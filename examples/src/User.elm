module User exposing (main)

import Browser
import Control


type alias User =
    { name : String
    , age : Int
    , role : Role
    , password : String
    }


userControl :
    Control.Control
        ( Control.State String, ( Control.State String, ( Control.State ( Control.State (), ( Control.State ( Control.State String, Control.End ), Control.End ) ), ( Control.State ( Control.State ( Control.State String, ( Control.State String, Control.End ) ), Control.End ), Control.End ) ) ) )
        ( Control.Delta String, ( Control.Delta String, ( Control.Delta ( Control.Delta (), ( Control.Delta ( Control.Delta String, Control.End ), Control.End ) ), ( Control.Delta ( Control.Delta ( Control.Delta String, ( Control.Delta String, Control.End ) ), Control.End ), Control.End ) ) ) )
        User
userControl =
    Control.record User
        |> Control.field .name nameControl
        |> Control.field .age ageControl
        |> Control.field .role roleControl
        |> Control.field .password passwordControl
        |> Control.end


ageControl : Control.Control String String Int
ageControl =
    Control.int
        |> Control.label "Age"
        |> Control.failIf (\a -> a < 0) "Age must be at least 0"
        |> Control.noteIf (\a -> a >= 100) "Gosh, that's a ripe old age!"


nameControl : Control.Control String String String
nameControl =
    Control.string
        |> Control.label "Name"
        |> Control.failIf String.isEmpty "Name is required"
        |> Control.noteIf (String.uncons >> Maybe.map (Tuple.first >> Char.isLower) >> Maybe.withDefault False) "Should your name begin with an uppercase letter?"


type Role
    = Regular
    | AdminLevel Int


roleControl :
    Control.Control
        ( Control.State (), ( Control.State ( Control.State String, Control.End ), Control.End ) )
        ( Control.Delta (), ( Control.Delta ( Control.Delta String, Control.End ), Control.End ) )
        Role
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
        |> Control.tag1 "Admin" AdminLevel levelControl
        |> Control.end
        |> Control.label "Role"


levelControl : Control.Control String String Int
levelControl =
    Control.int
        |> Control.label "Security clearance level"
        |> Control.failIf (\l -> not (List.member l [ 1, 2, 3 ])) "The only valid clearance levels are 1, 2 and 3"


passwordControl :
    Control.Control
        ( Control.State ( Control.State String, ( Control.State String, Control.End ) ), Control.End )
        ( Control.Delta ( Control.Delta String, ( Control.Delta String, Control.End ) ), Control.End )
        String
passwordControl =
    Control.record (\p1 p2 -> { password1 = p1, password2 = p2 })
        |> Control.field .password1 password1Control
        |> Control.field .password2 password2Control
        |> Control.end
        |> Control.throw
            { when = \{ password1, password2 } -> password1 /= password2
            , flag = "passwords-do-not-match"
            }
        |> Control.map { convert = .password1, revert = \p -> { password1 = p, password2 = p } }


password1Control : Control.Control String String String
password1Control =
    Control.string
        |> Control.label "Password"
        |> Control.failIf (\p -> String.length p < 12) "Password must be at least 12 characters"


password2Control : Control.Control String String String
password2Control =
    Control.string
        |> Control.label "Confirm password"
        |> Control.catch { flag = "passwords-do-not-match", fail = True, message = "Passwords must match" }


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
                , ( Control.State
                        ( Control.State String, Control.End )
                  , Control.End
                  )
                )
            , ( Control.State
                    ( Control.State
                        ( Control.State String
                        , ( Control.State String, Control.End )
                        )
                    , Control.End
                    )
              , Control.End
              )
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
            , ( Control.Delta
                    ( Control.Delta
                        ( Control.Delta String
                        , ( Control.Delta String, Control.End )
                        )
                    , Control.End
                    )
              , Control.End
              )
            )
          )
        )


userForm :
    Control.Form
        ( Control.State String, ( Control.State String, ( Control.State ( Control.State (), ( Control.State ( Control.State String, Control.End ), Control.End ) ), ( Control.State ( Control.State ( Control.State String, ( Control.State String, Control.End ) ), Control.End ), Control.End ) ) ) )
        ( Control.Delta String, ( Control.Delta String, ( Control.Delta ( Control.Delta (), ( Control.Delta ( Control.Delta String, Control.End ), Control.End ) ), ( Control.Delta ( Control.Delta ( Control.Delta String, ( Control.Delta String, Control.End ) ), Control.End ), Control.End ) ) ) )
        User
        Msg
userForm =
    Control.form
        { control = userControl
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
                                let
                                    _ = Debug.log "errors" errors
                                in
                                ( { model | formState = newFormState }
                                , Cmd.none
                                )
        , subscriptions =
            \model ->
                userForm.subscriptions model.formState
        }
