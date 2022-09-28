module POC exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Time



-- USERLAND CODE
-- Userland type definitions


type alias Model =
    --( Int, ( Float, ( String, End ) ) )
    State Int (State Float (State String End))


type
    Msg
    --= FormMsg ( Maybe Int, ( Maybe Float, ( Maybe String, End ) ) )
    = FormMsg (Delta Int (Delta Float (Delta String End)))


type alias User =
    { age : Int, height : Float, name : String }



-- Userland form definitions


form =
    new User FormMsg
        |> field f0 int
        |> field f1 float
        |> field f2 string
        |> end



-- Userland widget definitions


int =
    { init = 100
    , update = \delta state -> state // delta
    , view = \toMsg state -> button [ onClick (toMsg 2) ] [ text (String.fromInt state) ]
    }


float =
    { init = 0.1
    , update = \delta state -> state + delta
    , view = \toMsg state -> button [ onClick (toMsg 2.1) ] [ text (String.fromFloat state) ]
    }


string =
    { init = ""
    , update = \delta state -> delta
    , view = \toMsg state -> input [ onInput toMsg, HA.value state ] [ text state ]
    }



-- Userland program plumbing


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
    div [] (form.view model)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = form.init
        , view = view
        , update = update
        }



-- LIBRARY CODE
--Types


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


type alias FieldBuilder state delta element msg =
    { init : state
    , update : delta -> state -> state
    , view : (delta -> msg) -> state -> element
    }



-- Field setters


f0 =
    identity


f1 =
    Tuple.mapSecond


f2 =
    f1 >> f1



-- Step functions


updater1 next ( field_, fields ) ( delta, deltas ) ( state, states ) =
    ( case delta of
        Just d ->
            field_.update d state

        Nothing ->
            state
    , next fields deltas states
    )


viewer1 next toMsg ( field_, fields ) ( state, states ) =
    ( field_.view (\x -> toMsg (field_.toDelta x)) state, next toMsg fields states )


combine combiner msg fields =
    combiner (\_ End -> End) msg fields


combiner1 next msg ( field_, fields ) =
    let
        set x =
            Tuple.mapFirst (always (Just x))
    in
    ( { update = field_.update
      , view = field_.view
      , toDelta = \x -> field_.setter (set x) msg
      }
    , next msg fields
    )


toList toLister tuple =
    toLister ( [], tuple )
        |> Tuple.first
        |> List.reverse


toLister1 ( list, ( item, items ) ) =
    ( item :: list, items )


reverse reverser tuple =
    reverser ( End, tuple )
        |> Tuple.first


reverser1 ( s, ( fst, rest ) ) =
    ( ( fst, s ), rest )


unfurl unfurler toOutput states =
    unfurler ( toOutput, states )
        |> Tuple.first


unfurler1 ( toX, ( a, b ) ) =
    ( toX a, b )



-- Form builder functions


new output toMsg =
    { unfurler = identity
    , combiner = identity
    , updater = identity
    , viewer = identity
    , toLister = identity
    , stateReverser = identity
    , fieldReverser = identity
    , fieldStates = End
    , fields = End
    , emptyMsg = End
    , toMsg = toMsg
    , output = output
    }


field fieldSetter fb f =
    { unfurler = f.unfurler >> unfurler1
    , combiner = f.combiner >> combiner1
    , updater = f.updater >> updater1
    , viewer = f.viewer >> viewer1
    , toLister = f.toLister >> toLister1
    , stateReverser = f.stateReverser >> reverser1
    , fieldReverser = f.fieldReverser >> reverser1
    , fieldStates = ( fb.init, f.fieldStates )
    , fields =
        ( { update = fb.update
          , view = fb.view
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
        inits =
            reverse f.stateReverser f.fieldStates

        fields =
            combine f.combiner f.emptyMsg f.fields
                |> reverse f.fieldReverser
    in
    { init = inits
    , update =
        \deltas states ->
            f.updater (\End End End -> End) fields deltas states
    , view =
        \states ->
            f.viewer (\_ End End -> End) f.toMsg fields states
                |> toList f.toLister
    , submit =
        \states ->
            unfurl f.unfurler f.output states
    }
