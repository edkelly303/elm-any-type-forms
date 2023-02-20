module Example exposing (main)

import Browser
import Control
import Html
import Html.Events

main =
    Browser.element
        { init = \() -> ( form.init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = FormUpdated FormUpdated
    | FormSubmitted


update msg model =
    case msg of
        FormUpdated delta ->
            form.update delta model

        FormSubmitted ->
            case form.submit model of
                Ok { state, output } ->
                    let
                        _ =
                            Debug.log "Success!" output
                    in
                    ( state, Cmd.none )

                Err { state, errors } ->
                    let
                        _ =
                            Debug.log "Failure!" errors
                    in
                    ( state, Cmd.none )


view model =
    Html.div []
        [ form.view model
        , Html.button
            [ Html.Events.onClick FormSubmitted ]
            [ Html.text "Submit" ]
        ]


type alias FormUpdated =
    Control.Delta
        ( Control.Delta ( Control.Delta String, Control.End )
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


form =
    Control.toForm "My Lovely Form" FormUpdated exampleControl


type Example
    = Foo String
    | Bar { baz : String, qux : Int }


exampleControl =
    Control.customType
        (\foo bar tag ->
            case tag of
                Foo str ->
                    foo str

                Bar record ->
                    bar record
        )
        |> Control.tag1 "Foo" Foo fooControl
        |> Control.tag1 "Bar" Bar barControl
        |> Control.end
        |> Control.initWith (Bar { baz = "hello", qux = 1 })


fooControl =
    Control.string
        |> Control.debounce 500
        |> Control.failIf String.isEmpty "Foo must not be blank"


barControl =
    Control.record
        (\baz qux ->
            { baz = baz
            , qux = qux
            }
        )
        |> Control.field "Baz" .baz bazControl
        |> Control.field "Qux" .qux quxControl
        |> Control.end
        |> Control.flagIf (\record -> record.baz == String.fromInt record.qux) "baz=qux"


bazControl =
    Control.string
        |> Control.debounce 500
        |> Control.onFlag "baz=qux" "Baz must not equal Qux"
        |> Control.failIf (String.contains "?") "Baz must not contain a '?'"


quxControl =
    Control.int
        |> Control.debounce 500
        |> Control.onFlag "baz=qux" "Qux must not equal Baz"
        |> Control.failIf (\n -> n < 1) "Qux must greater than zero"
