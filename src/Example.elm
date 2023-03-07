module Example exposing (main)

import Browser
import Control
import Html
import Html.Events
import List.Extra



-- Here's how we define a form for a complex Elm type
-- with multi-field validations and debouncing for free


exampleForm =
    Control.toForm "My Lovely Form" FormUpdated exampleControl


type Example
    = Foo (List String)
    | Bar { baz : String, qux : Int }


exampleControl =
    Control.customType
        (\foo bar tag ->
            case tag of
                Foo list ->
                    foo list

                Bar record ->
                    bar record
        )
        |> Control.tag1 "Foo" Foo fooControl
        |> Control.tag1 "Bar" Bar barControl
        |> Control.end


fooControl =
    Control.list
        (Control.string |> Control.failIf String.isEmpty "Item cannot be blank")
        |> Control.initWith [ "Hello", "World", "" ]
        |> Control.failIf (List.Extra.allDifferent >> not) "All items in the list must be unique"


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
        |> Control.flagIf (\record -> String.toInt record.baz == Just record.qux) "baz=qux"


bazControl =
    Control.string
        |> Control.debounce 500
        |> Control.onFlag "baz=qux" "Baz must not equal Qux"
        |> Control.failIf String.isEmpty "Baz must not be blank"
        |> Control.failIf (String.contains "?") "Baz must not contain a '?'"


quxControl =
    counterControl
        |> Control.onFlag "baz=qux" "Qux must not equal Baz"
        |> Control.failIf (\n -> n < 1) "Qux must be greater than zero"



-- The package includes basic controls for all the core Elm types (Control.int,
-- Control.float, Control.string, Control.list, Control.maybe, Control.tuple, etc.)
--
-- You can also make your own custom controls, with arbitrary state (i.e. model)
-- and delta (i.e. msg) types. Here's an example that may look a bit familiar:


type CounterDelta
    = Increment
    | Decrement


counterControl =
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
            \state ->
                Html.div []
                    [ Html.button [ Html.Events.onClick Increment ] [ Html.text "+1" ]
                    , Html.div [] [ Html.text <| String.fromInt state ]
                    , Html.button [ Html.Events.onClick Decrement ] [ Html.text "-1" ]
                    ]
        , parse = Ok
        }



-- And here's how we initialise, update and view a form:


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
                ( newForm, cmd ) =
                    exampleForm.update delta model.form
            in
            ( { form = newForm }, cmd )

        FormSubmitted ->
            let
                ( newForm, result ) =
                    exampleForm.submit model.form

                _ =
                    case result of
                        Ok output ->
                            Debug.log "Success!" (Debug.toString output)

                        Err errors ->
                            Debug.log "Failure!" (Debug.toString errors)
            in
            ( { form = newForm }, Cmd.none )


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
        ( Control.State
            ( Control.State (List (Control.State String))
            , Control.End
            )
        , ( Control.State
                ( Control.State
                    ( Control.State String
                    , ( Control.State Int
                      , Control.End
                      )
                    )
                , Control.End
                )
          , Control.End
          )
        )


type alias FormDelta =
    Control.Delta
        ( Control.Delta
            ( Control.Delta (Control.ListDelta String)
            , Control.End
            )
        , ( Control.Delta
                ( Control.Delta
                    ( Control.Delta String
                    , ( Control.Delta CounterDelta
                      , Control.End
                      )
                    )
                , Control.End
                )
          , Control.End
          )
        )
