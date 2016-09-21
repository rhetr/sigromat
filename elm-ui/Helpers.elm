module Helpers exposing (..)

import Array exposing (Array)
import String exposing (indices)

atos : Int -> Array String -> String
atos i array =
    Array.get i array |> Maybe.withDefault ""
    
listStringComparison : String -> String -> String -> Order
listStringComparison char a b =
    let
        charInA = indices "/" a
        charInB = indices "/" b
    in 
        compare charInA charInB
