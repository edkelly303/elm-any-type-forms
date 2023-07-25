module Path exposing (Path, add, dropLast, last, root, toString)


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
        |> String.toLower


last : Path -> Int
last (Path path) =
    List.head path
        |> Maybe.withDefault 0


dropLast : Path -> Path
dropLast (Path path) =
    Path (List.drop 1 path)
