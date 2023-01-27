module Example exposing (..)

import Browser
import Form exposing (..)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE



-- main : Program () (Form.State String) Msg


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init () =
    ( mainForm.initFrom (C1 1 2)
    , Cmd.none
    )



-- view : Form.State String -> Html Msg


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
            [ mainForm.view model
            , H.button [ HE.onClick Submit ] [ H.text "Submit" ]
            ]
        ]



-- update : Msg -> Form.State String -> ( Form.State String, Cmd Msg )


update msg model =
    case msg of
        Submit ->
            case mainForm.submit model of
                Ok _ ->
                    ( model, Cmd.none )

                Err newModel ->
                    ( newModel, Cmd.none )

        FormMsg formMsg ->
            let
                ( newModel, cmd ) =
                    mainForm.update formMsg model
            in
            ( newModel, cmd )


type Msg
    = FormMsg
        (Form.Delta
            (CustomTypeDelta
                ( Delta ( Delta String, ( Delta String, End ) )
                , ( Delta ( Delta String, End ), End )
                )
            )
        )
    | Submit



-- mainForm : Form String String Int Msg


mainForm =
    test
        |> toForm "Creating an Item" FormMsg


boundedInt : Control String String Int
boundedInt =
    int
        |> failIf (\x -> x <= 0) "must be greater than 0"
        |> failIf (\x -> x > 120) "maximum is 120"
