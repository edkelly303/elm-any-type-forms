module Path exposing (Path, add, root, toString)


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
        |> List.filter (not << String.isEmpty)
        |> String.join " > "
