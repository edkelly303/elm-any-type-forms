module Example exposing (main)

import Browser
import Control
import Html
import Html.Events



-- Defining a form for a complex Elm type with multi-field validations and debouncing


exampleForm =
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


fooControl =
    Control.string
        |> Control.debounce 500
        |> Control.failIf String.isEmpty "Foo must not be blank"
        |> Control.initWith "Hello world"


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


type CounterDelta
    = Increment
    | Decrement


quxControl =
    Control.create
        { empty = 0
        , initialise = identity
        , update =
            \delta state ->
                case delta of
                    Increment ->
                        state + 1

                    Decrement ->
                        state - 1
        , view =
            \{ state, status } ->
                Html.div []
                    [ Html.button [ Html.Events.onClick Increment ] [ Html.text "+1" ]
                    , Html.div [] [ Html.text <| String.fromInt state ]
                    , Html.button [ Html.Events.onClick Decrement ] [ Html.text "-1" ]
                    , Control.statusView status
                    ]
        , parse = Ok
        }
        |> Control.onFlag "baz=qux" "Qux must not equal Baz"
        |> Control.failIf (\n -> n < 1) "Qux must be greater than zero"



-- Initialising, updating and viewing a form


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { form : FormState }


type Msg
    = FormUpdated FormDelta
    | FormSubmitted


init : () -> ( Model, Cmd Msg )
init () =
    ( { form = exampleForm.init }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormUpdated delta ->
            let
                ( state, cmd ) =
                    exampleForm.update delta model.form
            in
            ( { form = state }, cmd )

        FormSubmitted ->
            case exampleForm.submit model.form of
                Ok { state, output } ->
                    let
                        _ =
                            Debug.log "Success!" output
                    in
                    ( { form = state }, Cmd.none )

                Err { state, errors } ->
                    let
                        _ =
                            Debug.log "Failure!" errors
                    in
                    ( { form = state }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ exampleForm.view model.form
        , Html.button
            [ Html.Events.onClick FormSubmitted ]
            [ Html.text "Submit" ]
        ]



-- The ugly secret! The internal model and msg types for forms are a bit nasty.
-- Fortunately the Elm compiler can tell us what they should be, so we don't have
-- to work them out for ourselves.


type alias FormState =
    Control.State
        ( Control.State ( Control.State String, Control.End )
        , ( Control.State
                ( Control.State
                    ( Control.State String
                    , ( Control.State Int, Control.End )
                    )
                , Control.End
                )
          , Control.End
          )
        )


type alias FormDelta =
    Control.Delta
        ( Control.Delta ( Control.Delta String, Control.End )
        , ( Control.Delta
                ( Control.Delta
                    ( Control.Delta String
                    , ( Control.Delta CounterDelta, Control.End )
                    )
                , Control.End
                )
          , Control.End
          )
        )
