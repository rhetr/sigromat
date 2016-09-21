import Html.App exposing (program)
import WebSocket
import String
import Array exposing (Array)
import Cmd.Extra

import Messages exposing (..)
import Graph exposing (..)
import DebugView exposing (..)
import Helpers exposing (..)


main =
    program { init = init, view = view, update = update, subscriptions = subs }


-- MODEL
server : String
server =
      "wss://echo.websocket.org"


-- INIT
init : (Graph, Cmd Msg)
init =
  { clients = []
  , ports = []
  , connections = []
  , sendMsg = ""
  , recvMsg = [] 
  } ! []

-- subscribe to server
subs : Graph -> Sub Msg
subs graph =
    WebSocket.listen server RecvMessage

-- UPDATE
update : Msg -> Graph -> (Graph, Cmd Msg)
update msg graph =
    case msg of 
        AddClient client ->
            addClient client graph ! []
        RmClient client ->
            removeClient client graph ! []
        AddPort client_port ->
            addPort client_port graph ! []
        RmPort client_port ->
            removePort client_port graph ! []
        AddConnection connection ->
            addConnection connection graph ! []
        RmConnection connection ->
            removeConnection connection graph ! []

        SendMessage ->
            { graph | sendMsg = "" } 
            ! [WebSocket.send server graph.sendMsg]
        RecvMessage msg ->
            { graph | recvMsg = msg :: graph.recvMsg }
            ! [receive msg graph]
        InputMessage msg ->
            { graph | sendMsg = msg }
            ! []

        NoOp ->
            graph ! []

-- splits messages
receive : String -> Graph -> Cmd Msg
receive msg graph = 
    let
        msgs = msg 
            |> String.split "?"
            |> List.sortWith (listStringComparison "/")
    in 
        msgs
            |> List.map (flip processMsg graph)
            |> Cmd.batch

processMsg : String -> Graph -> Cmd Msg
processMsg msg graph =
    let
        messageList = String.split " " msg 
        cmd = List.head messageList 
            |> Maybe.withDefault "none"  
        args = List.tail messageList |> Maybe.withDefault [""] |> Array.fromList
    in 
        case cmd  of
            "/client/add" ->
                makeClient args
            "/client/remove" ->
                delClient args graph.clients
            "/client/port/add" ->
                makePort args graph.clients graph.ports
            "/client/port/remove" ->
                delPort args graph.ports
            "/client/port/connection/add" ->
                makeConnection args graph.ports
            "/client/port/connection/remove" ->
                delConnection args graph.connections
            _ ->
                Cmd.none

makeClient : Array String -> Cmd Msg
makeClient args =
    if (Array.length args) == 1 then
        let
            client = createClient 
                ( atos 0 args )
        in 
            AddClient client |> Cmd.Extra.message
    else 
        Cmd.none

makePort : Array String -> List Client -> List Port -> Cmd Msg
makePort args clients ports =
    if (Array.length args) == 4 then
        let
            client_port = createPort 
                ( atos 1 args )
                ( atos 0 args )
                ( atos 2 args )
                ( atos 3 args )
                clients
                ports
        in 
            AddPort client_port |> Cmd.Extra.message
    else 
        Cmd.none

makeConnection : Array String -> List Port -> Cmd Msg
makeConnection args ports =
    if (Array.length args) == 2 then
        let
            connection = createConnection 
                ( atos 0 args )
                ( atos 1 args )
                ports
        in 
            AddConnection connection |> Cmd.Extra.message
    else 
        Cmd.none


delClient : Array String -> List Client -> Cmd Msg 
delClient args clients =
    let
        client_name = atos 0 args
        client = getClientByName client_name clients
    in 
        RmClient client |> Cmd.Extra.message

delPort : Array String -> List Port -> Cmd Msg 
delPort args ports =
    let 
        port_name = atos 0 args
        client_port = getPortByName port_name ports
    in 
        RmPort client_port |> Cmd.Extra.message

delConnection : Array String -> List Connection -> Cmd Msg 
delConnection args connections =
    let 
        source_name = atos 0 args
        sink_name = atos 1 args
        connection = getConnectionByNames source_name sink_name connections
    in 
        RmConnection connection |> Cmd.Extra.message

