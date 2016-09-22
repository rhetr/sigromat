module Helpers exposing (..)

import Array exposing (Array)
import String exposing (indices)

atos : Int -> Array String -> String
atos i array =
    Array.get i array |> Maybe.withDefault ""

splitCommandArgs : String -> (String, List String)
splitCommandArgs msg =
    let 
        messageList = String.split " " msg 
        cmd = List.head messageList 
            |> Maybe.withDefault "none"  
        args = List.tail messageList |> Maybe.withDefault [""]
    in 
        (cmd, args)
    
listStringComparison : String -> String -> String -> Order
listStringComparison char a b =
    let
        charInA = indices "/" a
        charInB = indices "/" b
    in 
        compare charInA charInB
