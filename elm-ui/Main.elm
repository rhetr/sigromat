import Html exposing (Html, h2, button, div, text, input)
import Html.Attributes exposing (value)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)

import WebSocket
import String
import Array exposing (Array)
import Cmd.Extra

import Graph exposing (..)


main =
    program { init = init, view = view, update = update, subscriptions = subs }


-- MODEL

server : String
server =
      "wss://echo.websocket.org"

-- VIEW

showClient : Client -> Html msg
showClient client =
    div [] [ text (client.name ++ "\n") ]

showPort : Port -> Html msg
showPort client_port =
    div [] [ text (client_port.client.name ++ ":" ++ client_port.name ++ " - " ++ (toString client_port.portType) ++ " - " ++ (toString client_port.flow)) ]

showConnection : Connection -> Html msg
showConnection connection =
    let
        cstr = 
            connection.source.client.name ++ ":" ++ connection.source.name ++
            "->" ++ connection.sink.client.name ++ ":" ++ connection.sink.client.name
    in
        div [] [ text cstr ]

inputCmd : Graph -> Html Msg
inputCmd graph =
    div []
        [ h2 [] [ text "command" ]
        , input [ onInput InputMessage, value graph.sendMsg ] []
        , button [ onClick SendMessage ] [ text "send" ]
        ]

receivedMsgs : Graph -> Html msg
receivedMsgs graph = 
    div [] 
        [ h2 [] [ text "received messages" ]
        , div [] (List.map viewMessage (List.reverse graph.recvMsg))
        ]

viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]

view : Graph -> Html Msg
view graph =
    div []
        [ h2 [] [ text "clients" ]
        , div [] (List.map showClient graph.clients)
        , h2 [] [ text "ports" ]
        , div [] (List.map showPort graph.ports)
        , h2 [] [ text "connections" ]
        , div [] (List.map showConnection graph.connections)
        , inputCmd graph
        , receivedMsgs graph
        ]

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


update : Msg -> Graph -> (Graph, Cmd Msg)
update msg graph =
    case msg of 
        AddClient client ->
            ((addClient client graph), Cmd.none)
        RmClient client ->
            ((addClient client graph), Cmd.none)
        AddPort client_port ->
            ((addPort client_port graph), Cmd.none)
        RmPort client_port ->
            ((addPort client_port graph), Cmd.none)
        AddConnection connection ->
            ((addConnection connection graph), Cmd.none)
        RmConnection connection ->
            ((addConnection connection graph), Cmd.none)

        SendMessage ->
            ({ graph | sendMsg = "" }, WebSocket.send server graph.sendMsg )
        RecvMessage msg ->
            ({ graph | recvMsg = msg :: graph.recvMsg }, receive msg graph )
        InputMessage msg ->
            ({ graph | sendMsg = msg }, Cmd.none)

init : (Graph, Cmd Msg)
init =
  ({ clients = [], ports = [], connections = [], sendMsg = "", recvMsg = [] }, Cmd.none)

-- subscribe to server
subs : Graph -> Sub Msg
subs graph =
    WebSocket.listen server RecvMessage

-- 1 - /add/client
-- 2 - /add/port
-- 3 - /add/connection
-- 4 - /remove/client
-- 5 - /remove/port
-- 6 - /remove/connection

atos i array =
    Array.get i array |> Maybe.withDefault ""

conv : String -> Int
conv str =
    str |> String.toInt |> Result.toMaybe |> Maybe.withDefault 0
        
receive : String -> Graph -> Cmd Msg
receive msg graph = 
    let
        msgs = msg 
            |> String.split "?"
            |> List.sort
    in 
        Cmd.batch (List.map (flip processMsg graph) msgs)


makeClient : Array String -> Cmd Msg
makeClient args =
    if (Array.length args) == 2 then
        let
            client = createClient 
                ( atos 0 args |> conv )
                ( atos 1 args)
        in 
            AddClient client |> Cmd.Extra.message
    else 
        Cmd.none

makePort : Array String -> Graph -> Cmd Msg
makePort args graph =
    if (Array.length args) == 5 then
        let
            client_port = createPort 
                ( atos 0 args |> conv )
                ( atos 1 args)
                ( atos 2 args |> conv )
                ( atos 3 args |> conv )
                ( atos 4 args |> conv )
                graph
        in 
            AddPort client_port |> Cmd.Extra.message
    else 
        Cmd.none

makeConnection : Array String -> Graph -> Cmd Msg
makeConnection args graph =
    if (Array.length args) == 3 then
        let
            connection = createConnection 
                ( atos 0 args |> conv )
                ( atos 1 args |> conv )
                ( atos 2 args |> conv )
                graph
        in 
            AddConnection connection |> Cmd.Extra.message
    else 
        Cmd.none


delClient : Int -> Graph -> Cmd Msg 
delClient client_id graph =
    let
        client = getClientByID client_id graph
    in 
        RmClient client |> Cmd.Extra.message

delPort : Int -> Graph -> Cmd Msg 
delPort port_id graph =
    let 
        client_port = getPortByID port_id graph
    in 
        RmPort client_port |> Cmd.Extra.message

delConnection : Int -> Graph -> Cmd Msg 
delConnection connection_id graph =
    let 
        connection = getConnectionByID connection_id graph
    in 
        RmConnection connection |> Cmd.Extra.message


processMsg : String -> Graph -> Cmd Msg
processMsg msg graph =
    let
        messageList = String.split " " msg 
        cmd = List.head messageList 
            |> Maybe.withDefault "none"  
        args = List.tail messageList |> Maybe.withDefault [""] |> Array.fromList
    in 
        if cmd == "1" then
            makeClient args
        else if cmd == "2" then
            delClient ( atos 0 args |> conv ) graph

        else if cmd == "3" then
            makePort args graph
        else if cmd == "4" then
            delPort ( atos 0 args |> conv ) graph

        else if cmd == "5" then
            makeConnection args graph
        else if cmd == "6" then
            delConnection ( atos 0 args |> conv ) graph
        else 
            Cmd.none

