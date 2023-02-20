module Example exposing (main)

import Browser
import Control


main =
    Browser.element
        { init = \() -> ( form.init, Cmd.none )
        , view = form.view
        , update = form.update
        , subscriptions = \_ -> Sub.none
        }


form =
    Control.toForm "My Lovely Form" identity exampleControl


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
        |> Control.initWith (Bar {baz = "hello", qux = 1})


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
