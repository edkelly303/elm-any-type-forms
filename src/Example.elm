module Example exposing (..)

import Browser
import Form exposing (..)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Item


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init () =
    ( -- userForm.init
      itemForm.initWith Item.exampleItem
    , Cmd.none
    )


view model =
    H.div
        [ HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "font-family" "sans"
        , HA.style "background-color" "gray"
        ]
        [ H.div
            [ HA.style "width" "600px"
            , HA.style "outline" "solid 1px gray"
            , HA.style "border-radius" "5px"
            , HA.style "margin" "10px"
            , HA.style "padding" "20px"
            , HA.style "background-color" "white"
            ]
            [ itemForm.view model
            , H.button [ HE.onClick Submit ] [ H.text "Submit" ]
            ]
        ]


update msg model =
    case msg of
        Submit ->
            case itemForm.submit model of
                Ok _ ->
                    ( model, Cmd.none )

                Err newModel ->
                    ( newModel, Cmd.none )

        FormMsg formMsg ->
            let
                ( newModel, cmd ) =
                    itemForm.update formMsg model
            in
            ( newModel, cmd )


type
    Msg
    --= FormMsg (Form.Delta UserDelta)
    = FormMsg (Form.Delta Item.ItemDelta)
    | Submit


itemForm =
    Item.form FormMsg



-- userForm =
--     toForm "Create a User" FormMsg user


type alias User =
    { name : String
    , age : Int
    , role : Role
    }


type alias UserDelta =
    ( Delta String
    , ( Delta String
      , ( Delta
            (CustomTypeDelta
                ( Delta ()
                , ( Delta
                        ( Delta String
                        , ( Delta String
                          , End
                          )
                        )
                  , End
                  )
                )
            )
        , End
        )
      )
    )


type Role
    = Guest
    | Registered String String


user =
    record User
        |> field .name "Name" string
        |> field .age "Age" boundedInt
        |> field .role "Role" role
        |> endRecord


role =
    customType
        |> tag0 "Guest" Guest
        |> tag2 "Registered" Registered string string
        |> endCustomType
            (\guest registered tag ->
                case tag of
                    Guest ->
                        guest

                    Registered username password ->
                        registered username password
            )


boundedInt : Control String String Int
boundedInt =
    int
        |> failIf (\x -> x <= 0) "must be greater than 0"
        |> failIf (\x -> x > 120) "maximum is 120"
