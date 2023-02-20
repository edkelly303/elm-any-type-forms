module Path exposing (Path, add, last, root, toString)


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


last : Path -> String
last (Path path) =
    case path of
        lastSegment :: _ ->
            lastSegment

        [] ->
            ""
