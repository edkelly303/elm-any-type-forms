module Interruptable exposing (Calc, finished, interruptAfter, processing)

import Process
import Task


type Calc a b
    = Processing a
    | Finished b


processing : a -> Calc a b
processing a =
    Processing a


finished : b -> Calc a b
finished b =
    Finished b


interruptAfter : Int -> (a -> Calc a b) -> a -> Cmd (Calc a b)
interruptAfter steps fn acc =
    Task.perform
        (\() -> loop steps fn acc)
        (Process.sleep 0)


loop : Int -> (a -> Calc a b) -> a -> Calc a b
loop steps fn acc =
    if steps == 0 then
        Processing acc

    else
        case fn acc of
            Processing acc1 ->
                loop (steps - 1) fn acc1

            Finished answer ->
                Finished answer



-- compute : ( Int, List Int ) -> Calc ( Int, List Int ) Int
-- compute ( acc, input ) =
--     case input of
--         [] ->
--             Finished acc
--         h :: t ->
--             Processing ( acc + h, t )
