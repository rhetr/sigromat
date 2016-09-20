import Html exposing (Html, h2, button, div, text, input)
import Html.App exposing(program)
import Html.Events exposing (onClick, onInput)
import WebSocket

import Graph exposing (..)


main =
    program { init = init, view = view, update = update, subscriptions = subs }



-- MODEL

server : String
server =
      "wss://echo.websocket.org"

-- VIEW
-- matrix
--  connections are dots
-- sources
-- sinks

showClient : Client -> Html a
showClient client =
    text client.name

showPort : Port -> Html a
showPort client_port =
    text client_port.name

showConnection : Connection -> Html a
showConnection connection =
    let cstr = 
        connection.source.client.name ++ ":" ++ connection.source.name ++
        "->" ++ connection.sink.client.name ++ ":" ++ connection.sink.client.name
    in text cstr

inputCmd : Html Msg
inputCmd =
    div []
        [ h2 [] [ text "command" ]
        , input [ onInput InputMessage ] []
        , button [ onClick SendMessage ] [ text "send" ]
        ]

receivedMsgs : Graph -> Html msg
receivedMsgs graph = 
    div [] 
        [ h2 [] [ text "received messages" ]
        , div [] (List.map viewMessage graph.recvMsg)
        ]

viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]



view : Graph -> Html Msg
view graph =
    div []
        [ div [] (List.map showClient graph.clients)
        , div [] (List.map showPort graph.ports)
        , div [] (List.map showConnection graph.connections)
        , inputCmd
        , receivedMsgs graph
        ]
-- view graph =
--     div []
--         [ List.map showClient graph.clients
--         , List.map showPort graph.ports
--         , List.map showConnection graph.connections
--         , inputCmd
--         ]

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
            ({ graph | sendMsg = "" }, WebSocket.send server graph.sendMsg)
        RecvMessage msg ->
            ({ graph | recvMsg = msg :: graph.recvMsg }, Cmd.none)
        InputMessage msg ->
            ({ graph | sendMsg = msg }, Cmd.none)



init : (Graph, Cmd Msg)
init =
  ({ clients = [], ports = [], connections = [], sendMsg = "", recvMsg = [] }, Cmd.none)

-- subscribe to server
subs : Graph -> Sub Msg
subs graph =
    WebSocket.listen server RecvMessage

