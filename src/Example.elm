module Example exposing (main)

import Browser
import Field
import Form
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


type Model
    = Editing UserFormFields
    | Completed User


type Msg
    = UserFormUpdated UserFormFields
    | Submitted
    | Back


type alias UserFormFields =
    ( Field.State String String Int
    , ( Field.State String String Float
      , ( Field.State String String String
        , ( Field.MaybeField (Maybe Pet) Pet Pet
          , ( Field.ListField (Maybe Pet) Pet Pet
            , ( Field.ChoiceField String String String String Either
              , Form.End
              )
            )
          )
        )
      )
    )


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


type Either
    = Integer Int
    | Str String


type alias User =
    { age : Int
    , height : Float
    , name : String
    , pet : Maybe Pet
    , pets : List Pet
    , something : Either
    }


form :
    { init : ( UserFormFields, Cmd Msg )
    , update : UserFormFields -> UserFormFields -> ( UserFormFields, Cmd Msg )
    , view : UserFormFields -> List (Html Msg)
    , submit : UserFormFields -> Result UserFormFields User
    }
form =
    Form.new
        User
        UserFormUpdated
        |> Form.field Form.f0 (Field.int "Age")
        |> Form.field Form.f1 (Field.float "Height")
        |> Form.field Form.f2
            (Field.string "Name"
                |> Field.debounce 500
                |> Field.failIf (\name -> String.length name < 2) "must be at least 2 characters"
                |> Field.infoIf (\name -> name == "e") "'e' is my favourite letter!"
            )
        |> Form.field Form.f3
            (Field.maybe
                (Field.radio "Favourite pet"
                    [ ( "dog", Dog )
                    , ( "cat", Cat )
                    , ( "goldfish", Goldfish )
                    ]
                )
            )
        |> Form.field Form.f4
            (Field.list
                petToString
                (Field.radio "Other pets"
                    [ ( "dog", Dog )
                    , ( "cat", Cat )
                    , ( "goldfish", Goldfish )
                    ]
                )
            )
        |> Form.field Form.f5
            (Field.choice "Choose"
                ( Integer
                , \either ->
                    case either of
                        Integer i ->
                            Just i

                        _ ->
                            Nothing
                , Field.int "Int"
                )
                ( Str
                , \either ->
                    case either of
                        Str s ->
                            Just s

                        _ ->
                            Nothing
                , Field.string "String"
                )
            )
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
                , H.button
                    [ HA.style "margin-top" "30px"
                    , HE.onClick Submitted
                    ]
                    [ H.text "submit" ]
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
                , H.button
                    [ HA.style "margin-top" "30px"
                    , HE.onClick Back
                    ]
                    [ H.text "go back" ]
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> form.init |> Tuple.mapFirst Editing
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
