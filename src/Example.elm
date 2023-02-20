module Example exposing (main)

import Browser
import Control exposing (..)
import Form
import Html as H
import Html.Attributes as HA
import Html.Events as HE


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init () =
    ( mainForm.init
    , Cmd.none
    )


view model =
    H.div
        [ HA.style "width" "100vw"
        , HA.style "height" "100vh"
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "font-family" "sans"
        , HA.style "background-color" "gray"
        , HA.style "margin" "-8px"
        ]
        [ H.div
            [ HA.style "width" "600px"
            , HA.style "outline" "solid 1px gray"
            , HA.style "border-radius" "5px"
            , HA.style "margin" "10px"
            , HA.style "padding" "20px"
            , HA.style "background-color" "white"
            , HA.style "overflow" "scroll"
            ]
            [ mainForm.view model
            , H.button
                [ HA.style "margin-top" "10px"
                , HE.onClick Submit
                ]
                [ H.text "Submit" ]
            ]
        ]


update msg model =
    case msg of
        Submit ->
            case mainForm.submit model of
                Ok output ->
                    let
                        _ =
                            Debug.log "Success" output
                    in
                    ( model, Cmd.none )

                Err { state, errors } ->
                    let
                        _ =
                            Debug.log "Errors" errors
                    in
                    ( state, Cmd.none )

        FormMsg formMsg ->
            mainForm.update formMsg model


type Msg
    = FormMsg
        (Delta ( Delta String
        , ( Delta String
          , ( Delta (ListDelta String)
            , ( Delta
                    ( Delta ()
                    , ( Delta
                            ( Delta ( Delta String, ( Delta String, End ) ), End
                            )
                      , End
                      )
                    )
              , End
              )
            )
          )
        ))
    | Submit


mainForm =
    Form.fromControl "Create a User" FormMsg user


type alias User =
    { name : String
    , age : Int
    , interests : List String
    , role : Role
    }


user =
    record User
        |> field "Name" .name (string |> onFlag "na" "Boo!")
        |> field "Age" .age (boundedInt |> onFlag "na" "Boo!")
        |> field "Interests" .interests (list (string |> flagIf String.isEmpty "Must not be blank"))
        |> field "Role" .role role
        |> end
        |> flagIf (\{ name, age } -> name == String.fromInt age) "na"


type Role
    = Guest
    | Registered Password


role :
    Control
        ( State ()
        , ( State ( State ( State String, ( State String, End ) ), End ), End )
        )
        ( Delta ()
        , ( Delta ( Delta ( Delta String, ( Delta String, End ) ), End ), End )
        )
        Role
role =
    customType
        (\guest registered tag ->
            case tag of
                Guest ->
                    guest

                Registered pwd ->
                    registered pwd
        )
        |> tag0 "Guest" Guest
        |> tag1 "Registered" Registered password
        |> end


type alias Password =
    { password : String
    , confirmPassword : String
    }


password :
    Control
        ( State String, ( State String, End ) )
        ( Delta String, ( Delta String, End ) )
        Password
password =
    record Password
        |> field "Enter password"
            .password
            (string
                |> onFlag "password-matcher" "Must match confirm password field"
                |> failIf (\str -> str == "") "Password can't be blank"
            )
        |> field "Confirm password"
            .confirmPassword
            (string
                |> onFlag "password-matcher" "Must match password field"
            )
        |> end
        |> flagIf (\rec -> rec.password /= rec.confirmPassword) "password-matcher"


boundedInt : Control String String Int
boundedInt =
    int
        |> failIf (\x -> x <= 0) "must be greater than 0"
        |> failIf (\x -> x > 120) "maximum is 120"
