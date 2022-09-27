module POC exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)



-- USERLAND CODE


type alias Model =
    ( Int, ( Float, ( String, () ) ) )


type Msg
    = FormMsg ( Maybe Int, ( Maybe Float, ( Maybe String, () ) ) )


type alias User =
    { age : Int, height : Float, name : String }


form =
    new User FormMsg
        |> add f0 100 intUpdate intView
        |> add f1 0.1 floatUpdate floatView
        |> add f2 "hey" stringUpdate stringView
        |> done


intView toMsg toDelta state =
    button [ onClick (toMsg (toDelta 2)) ] [ text (String.fromInt state) ]


intUpdate delta state =
    state // delta


floatView toMsg toDelta state =
    button [ onClick (toMsg (toDelta 2.1)) ] [ text (String.fromFloat state) ]


floatUpdate delta state =
    state + delta


stringView toMsg toDelta state =
    input [ onInput (toMsg << toDelta) ] [ text state ]


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


f0 =
    identity


f1 =
    Tuple.mapSecond


f2 =
    f1 >> f1


set x =
    Tuple.mapFirst (always (Just x))



-- STEP FUNCTIONS


combine1 next msg ( setter, setters ) =
    ( \x -> setter (set x) msg, next msg setters )


update1 next ( updater, updaters ) ( delta, deltas ) ( state, states ) =
    ( case delta of
        Just d ->
            updater d state

        Nothing ->
            state
    , next updaters deltas states
    )


view1 next toMsg ( fieldView, fieldViews ) ( toDelta, toDeltas ) ( state, states ) =
    ( fieldView toMsg toDelta state, next toMsg fieldViews toDeltas states )


toList1 next ( list, ( item, items ) ) =
    next ( item :: list, items )


reverse1 next ( s, ( fst, rest ) ) =
    next ( ( fst, s ), rest )


unfurl1 ( toX, ( a, b ) ) =
    ( toX a, b )


new output toMsg =
    { unfurlX = identity
    , combineX = identity
    , updateX = identity
    , viewX = identity
    , toListX = identity
    , reverseStateX = identity
    , reverseDeltaX = identity
    , reverseUpdateX = identity
    , reverseViewX = identity
    , fieldStates = ()
    , fieldUpdates = ()
    , fieldSetters = ()
    , fieldViews = ()
    , toMsg = toMsg
    , emptyMsg = ()
    , output = output
    }


add fieldSetter fieldState fieldUpdate fieldView f =
    { unfurlX = f.unfurlX >> unfurl1
    , combineX = f.combineX >> combine1
    , updateX = f.updateX >> update1
    , viewX = f.viewX >> view1
    , toListX = f.toListX >> toList1
    , reverseStateX = f.reverseStateX >> reverse1
    , reverseDeltaX = f.reverseDeltaX >> reverse1
    , reverseUpdateX = f.reverseUpdateX >> reverse1
    , reverseViewX = f.reverseViewX >> reverse1
    , fieldStates = ( fieldState, f.fieldStates )
    , fieldUpdates = ( fieldUpdate, f.fieldUpdates )
    , fieldViews = ( fieldView, f.fieldViews )
    , fieldSetters = ( fieldSetter, f.fieldSetters )
    , toMsg = f.toMsg
    , emptyMsg = ( Nothing, f.emptyMsg )
    , output = f.output
    }


done f =
    let
        reversedToDeltas =
            f.combineX (\_ () -> ()) f.emptyMsg f.fieldSetters

        fieldToDeltas =
            f.reverseDeltaX identity ( (), reversedToDeltas ) |> Tuple.first

        fieldStates =
            f.reverseStateX identity ( (), f.fieldStates ) |> Tuple.first

        fieldUpdates =
            f.reverseUpdateX identity ( (), f.fieldUpdates ) |> Tuple.first

        fieldViews =
            f.reverseViewX identity ( (), f.fieldViews ) |> Tuple.first
    in
    { init = fieldStates
    , update =
        \ds ss ->
            f.updateX (\() () () -> ()) fieldUpdates ds ss
    , view =
        \ss ->
            f.viewX (\_ () () () -> ()) f.toMsg fieldViews fieldToDeltas ss
                |> Tuple.pair []
                |> f.toListX identity
                |> Tuple.first
                |> List.reverse
    , submit =
        \ss ->
            f.unfurlX ( f.output, ss ) |> Tuple.first
    }
