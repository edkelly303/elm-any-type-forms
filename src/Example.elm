module Example exposing (main)

import Browser
import Field
import Form
import Html as H exposing (Html)
import Html.Events as HE


type Model
    = Editing UserFormFields
    | Completed User


type Msg
    = UserFormUpdated UserFormFields
    | Submitted
    | Back


type alias UserFormFields =
    ( Field.State String String Int, ( Field.State String String Float, ( Field.State String String String, ( Field.ListField (Maybe Pet) Pet Pet, Form.End ) ) ) )


type Pet
    = Dog
    | Cat
    | Goldfish


petToString : Pet -> String
petToString pet =
    case pet of
        Dog ->
            "dog"

        Cat ->
            "cat"

        Goldfish ->
            "goldfish"


type alias User =
    { age : Int
    , height : Float
    , name : String
    , pets : List Pet
    }


form :
    { init : ( UserFormFields, Cmd Msg )
    , update : UserFormFields -> UserFormFields -> ( UserFormFields, Cmd Msg )
    , view : UserFormFields -> List (Html Msg)
    , submit : UserFormFields -> Result UserFormFields User
    }
form =
    Form.new User UserFormUpdated
        |> Form.field Form.f0 (Field.int "Age")
        |> Form.field Form.f1 (Field.float "Height")
        |> Form.field Form.f2
            (Field.string "Name"
                |> Field.debounce 500
                |> Field.failIf (\name -> String.length name < 2) "name must be at least 2 characters"
                |> Field.infoIf (\name -> name == "e") "'E' is my favourite letter!"
            )
        |> Form.field Form.f3
            (Field.list
                petToString
                (Field.radio "Pets"
                    [ ( "dog", Dog )
                    , ( "cat", Cat )
                    , ( "goldfish", Goldfish )
                    ]
                )
            )
        |> Form.failIf2
            (\int flt -> int > round flt)
            "height must be greater than age"
            Form.f0
            Form.f1
        |> Form.failIf2
            (\int name -> String.length name < int)
            "name must be at least as long as age"
            Form.f0
            Form.f2
        |> Form.end



-- Userland program plumbing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserFormUpdated delta ->
            case model of
                Editing form_ ->
                    form.update delta form_
                        |> Tuple.mapFirst Editing

                Completed _ ->
                    ( model, Cmd.none )

        Submitted ->
            case model of
                Editing form0 ->
                    case form.submit form0 of
                        Ok user ->
                            ( Completed user, Cmd.none )

                        Err form1 ->
                            ( Editing form1, Cmd.none )

                Completed _ ->
                    ( model, Cmd.none )

        Back ->
            form.init
                |> Tuple.mapFirst Editing


view : Model -> Html Msg
view model =
    case model of
        Editing form_ ->
            H.div []
                [ H.div [] (form.view form_)
                , H.button [ HE.onClick Submitted ] [ H.text "submit" ]
                ]

        Completed user ->
            H.div []
                [ H.div [] [ H.text "Your user's data:" ]
                , H.div [] [ H.text ("Age: " ++ String.fromInt user.age) ]
                , H.div [] [ H.text ("Height: " ++ String.fromFloat user.height) ]
                , H.div [] [ H.text ("Name: " ++ user.name) ]
                , H.div []
                    [ H.text
                        ("Pets: "
                            ++ (case user.pets of
                                    [] ->
                                        "none"

                                    [ pet ] ->
                                        petToString pet

                                    _ ->
                                        "several pets"
                               )
                        )
                    ]
                , H.button [ HE.onClick Back ] [ H.text "go back" ]
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> form.init |> Tuple.mapFirst Editing
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
