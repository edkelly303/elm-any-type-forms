module POC exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Time



-- USERLAND CODE


type alias Model =
    --( Int, ( Float, ( String, End ) ) )
    State Int (State Float (State String End))


type
    Msg
    --= FormMsg ( Maybe Int, ( Maybe Float, ( Maybe String, End ) ) )
    = FormMsg (Delta Int (Delta Float (Delta String End)))


type alias User =
    { age : Int, height : Float, name : String }


form =
    new User FormMsg
        |> field0 100 intUpdate intView
        |> field1 0.1 floatUpdate floatView
        |> field2 "hey" stringUpdate stringView
        |> end


intView toMsg state =
    button [ onClick (toMsg 2) ] [ text (String.fromInt state) ]


intUpdate delta state =
    state // delta


floatView toMsg state =
    button [ onClick (toMsg 2.1) ] [ text (String.fromFloat state) ]


floatUpdate delta state =
    state + delta


stringView toMsg state =
    input [ onInput toMsg ] [ text state ]


stringUpdate delta state =
    delta


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormMsg payload ->
            form.update payload model


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "User" (form.submit model)
    in
    div []
        (form.view model)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = form.init
        , view = view
        , update = update
        }



-- LIBRARY CODE


type End
    = End


type alias Delta this rest =
    ( Maybe this, rest )


type alias State this rest =
    ( this, rest )


type Field state delta output
    = Field { state : state, delta : D delta, output : Result (List String) output }


type D delta
    = Update delta
    | StartDebouncing delta Time.Posix
    | CheckDebouncingTimeout delta Time.Posix
    | ExecuteParsing


f0 =
    identity


f1 =
    Tuple.mapSecond


f2 =
    f1 >> f1


field0 =
    field f0


field1 =
    field f1


field2 =
    field f2


set x =
    Tuple.mapFirst (always (Just x))



-- STEP FUNCTIONS


combine1 next msg ( field_, fields ) =
    ( { update = field_.update
      , view = field_.view
      , toDelta = \x -> field_.setter (set x) msg
      }
    , next msg fields
    )


update1 next ( field_, fields ) ( delta, deltas ) ( state, states ) =
    ( case delta of
        Just d ->
            field_.update d state

        Nothing ->
            state
    , next fields deltas states
    )


view1 next toMsg ( field_, fields ) ( state, states ) =
    ( field_.view (\x -> toMsg (field_.toDelta x)) state, next toMsg fields states )


toList1 ( list, ( item, items ) ) =
    ( item :: list, items )


reverse1 ( s, ( fst, rest ) ) =
    ( ( fst, s ), rest )


unfurl1 ( toX, ( a, b ) ) =
    ( toX a, b )


new output toMsg =
    { unfurlX = identity
    , combineX = identity
    , updateX = identity
    , viewX = identity
    , toListX = identity
    , reverseStateX = identity
    , reverseFieldX = identity
    , fieldStates = End
    , fields = End
    , emptyMsg = End
    , toMsg = toMsg
    , output = output
    }


field fieldSetter fieldState fieldUpdate fieldView f =
    { unfurlX = f.unfurlX >> unfurl1
    , combineX = f.combineX >> combine1
    , updateX = f.updateX >> update1
    , viewX = f.viewX >> view1
    , toListX = f.toListX >> toList1
    , reverseStateX = f.reverseStateX >> reverse1
    , reverseFieldX = f.reverseFieldX >> reverse1
    , fieldStates = ( fieldState, f.fieldStates )
    , fields =
        ( { update = fieldUpdate
          , view = fieldView
          , setter = fieldSetter
          }
        , f.fields
        )
    , emptyMsg = ( Nothing, f.emptyMsg )
    , toMsg = f.toMsg
    , output = f.output
    }


end f =
    let
        fieldStates =
            f.fieldStates
                |> Tuple.pair End
                |> f.reverseStateX
                |> Tuple.first

        fields =
            f.combineX (\_ End -> End) f.emptyMsg f.fields
                |> Tuple.pair End
                |> f.reverseFieldX
                |> Tuple.first
    in
    { init = fieldStates
    , update =
        \ds ss ->
            f.updateX (\End End End -> End) fields ds ss
    , view =
        \ss ->
            f.viewX (\_ End End -> End) f.toMsg fields ss
                |> Tuple.pair []
                |> f.toListX
                |> Tuple.first
                |> List.reverse
    , submit =
        \ss ->
            ss
                |> Tuple.pair f.output
                |> f.unfurlX
                |> Tuple.first
    }
