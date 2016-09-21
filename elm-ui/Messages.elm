module Messages exposing (..)

import Graph exposing (Client, Port, Connection)


type Msg
    = AddClient Client
    | RmClient Client
    | AddPort Port
    | RmPort Port
    | AddConnection Connection
    | RmConnection Connection
    | SendMessage
    | RecvMessage String
    | InputMessage String
    | NoOp
