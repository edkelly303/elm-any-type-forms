module Path exposing (Path, add, root, toString)


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
