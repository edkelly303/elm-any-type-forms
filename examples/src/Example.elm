module Example exposing (main)

import Browser
import Control
import Dict
import Html
import Html.Attributes
import Html.Events



-- Here's how we define a form for a complex Elm type
-- with multi-field validations and debouncing for free


exampleForm =
    Control.toForm "My Lovely Form"
        FormUpdated
        FormSubmitted
        exampleControl


type Example
    = Foo (Dict.Dict String Int)
    | Bar { baz : String, qux : Int, xin : Bool, jyg : Char, zup : Float }


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
    Control.dict "Key" Control.string "Value" Control.int
        |> Control.initWith (Dict.fromList [ ( "Hello", 1 ), ( "World", 2 ) ])


barControl =
    Control.record
        (\baz qux xin jyg zup ->
            { baz = baz
            , qux = qux
            , xin = xin
            , jyg = jyg
            , zup = zup
            }
        )
        |> Control.field "Baz" .baz bazControl
        |> Control.field "Qux" .qux quxControl
        |> Control.field "Xin" .xin (Control.bool "" "")
        |> Control.field "Jyg" .jyg Control.char
        |> Control.field "Zup" .zup Control.float
        |> Control.end
        |> Control.throwFlagIf (\record -> String.toInt record.baz == Just record.qux) "baz=qux"


bazControl =
    Control.string
        |> Control.debounce 500
        |> Control.catchFlag "baz=qux" "Baz must not equal Qux"
        |> Control.failIf String.isEmpty "Baz must not be blank"
        |> Control.failIf (String.contains "?") "Baz must not contain a '?'"


quxControl =
    counterControl
        |> Control.catchFlag "baz=qux" "Qux must not equal Baz"
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
        { initEmpty = 0
        , initWith = identity
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
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Increment
                        ]
                        [ Html.text "+1" ]
                    , Html.div [] [ Html.text <| String.fromInt state ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Decrement
                        ]
                        [ Html.text "-1" ]
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
            in
            case result of
                Ok output ->
                    let
                        _ =
                            Debug.log "Success!" output
                    in
                    ( { form = newForm }, Cmd.none )

                Err errors ->
                    let
                        _ =
                            Debug.log "Failure!" errors
                    in
                    ( { form = newForm }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    exampleForm.view model.form



-- The ugly secret! The internal model and msg types for forms are a bit nasty.
-- Fortunately the Elm compiler can tell us what they should be, so we don't have
-- to work them out for ourselves.


type alias FormState =
    Control.State
        ( Control.State
              ( Control.State
                    ( Control.State
                          (
                          List
                              (
                              Control.State
                                  ( Control.State String
                                  , ( Control.State String, Control.End )
                                  )
                              )
                          )
                    , Control.End
                    )
              , Control.End
              )
        , ( Control.State
                ( Control.State
                      ( Control.State String
                      , ( Control.State Int
                        , ( Control.State Bool
                          , ( Control.State String
                            , ( Control.State String, Control.End )
                            )
                          )
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
              ( Control.Delta
                    ( Control.Delta
                          (
                          Control.ListDelta
                              ( Control.Delta String
                              , ( Control.Delta String, Control.End )
                              )
                          )
                    , Control.End
                    )
              , Control.End
              )
        , ( Control.Delta
                ( Control.Delta
                      ( Control.Delta String
                      , ( Control.Delta CounterDelta
                        , ( Control.Delta Bool
                          , ( Control.Delta String
                            , ( Control.Delta String, Control.End )
                            )
                          )
                        )
                      )
                , Control.End
                )
          , Control.End
          )
        )
