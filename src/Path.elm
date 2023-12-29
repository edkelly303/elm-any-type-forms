module Path exposing (Dict, Path, Set, add, compare, dict, root, set, toString, depth)

import Dict
import Set


type Path
    = Path (List Int)


root : Path
root =
    Path [ 1 ]


add : Int -> Path -> Path
add segment (Path path) =
    Path (segment :: path)


toString : Path -> String
toString (Path path) =
    path
        |> List.reverse
        |> List.map String.fromInt
        |> String.join "-"


compare : Path -> Path -> Basics.Order
compare (Path p1) (Path p2) =
    Basics.compare (List.reverse p1) (List.reverse p2)


depth : Path -> Int
depth (Path p) =
    List.length p


type Dict v
    = Dict (Dict.Dict (List Int) v)


dict :
    { fromList : List ( Path, v ) -> Dict v
    , get : Path -> Dict a -> Maybe a
    , toList : Dict b -> List ( Path, b )
    }
dict =
    { fromList =
        \list ->
            list
                |> List.map (Tuple.mapFirst (\(Path path) -> path))
                |> Dict.fromList
                |> Dict
    , get =
        \(Path path) (Dict dict_) ->
            Dict.get path dict_
    , toList =
        \(Dict dict_) ->
            Dict.toList dict_
                |> List.sortBy (Tuple.first >> List.reverse)
                |> List.map (Tuple.mapFirst Path)
    }


type Set
    = Set (Set.Set (List Int))


set :
    { empty : Set
    , insert : Path -> Set -> Set
    , remove : Path -> Set -> Set
    , member : Path -> Set -> Bool
    }
set =
    { empty = Set.empty |> Set
    , insert = \(Path path) (Set set_) -> Set.insert path set_ |> Set
    , remove = \(Path path) (Set set_) -> Set.remove path set_ |> Set
    , member = \(Path path) (Set set_) -> Set.member path set_
    }
