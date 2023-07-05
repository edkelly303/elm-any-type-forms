module Path exposing (Path, add, dropLast, last, root, toString)


type Path
    = Path (List String)


root : Path
root =
    Path []


add : String -> Path -> Path
add segment (Path path) =
    Path (segment :: path)


toString : Path -> String
toString (Path path) =
    path
        |> List.reverse
        -- |> List.filter (not << String.isEmpty)
        |> String.join "-"
        |> String.toLower


last : Path -> String
last (Path path) =
    List.head path
        |> Maybe.withDefault ""


dropLast : Path -> Path
dropLast (Path path) =
    Path (List.drop 1 path)
