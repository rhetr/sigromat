import Html.App exposing (program)
import WebSocket
import Task
import String
import Array exposing (Array)
import Cmd.Extra

import Messages exposing (..)
import Graph exposing (..)
import View exposing (..)
import Helpers exposing (..)


main =
    program { init = init, view = view, update = update, subscriptions = subs }


-- MODEL
server : String
server =
      "wss://echo.websocket.org"
-- server =
--     "ws://localhost:9090"


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
        NoOp ->
            graph ! []
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
        InputMessage msg ->
            { graph | sendMsg = msg }
            ! []
        RecvMessage msg ->
            { graph | recvMsg = msg :: graph.recvMsg }
            ! [receive msg graph]

        ChainMessages strings ->
            let
                chain string' (graph', cmds) =
                    let 
                        msg' = processMsg graph' string'
                        (graph'', cmds') = update msg' graph'
                    in 
                        graph'' ! [ cmds, cmds' ]
            in 
                List.foldl chain (graph ! []) strings

-- splits messages
receive : String -> Graph -> Cmd Msg
receive msg graph =
    msg |> String.split "&"
        |> List.map String.trim
        |> List.sortWith (listStringComparison "/")
        |> ChainMessages
        |> Cmd.Extra.message

processMsg : Graph -> String -> Msg
processMsg graph msg =
    let
        (cmd, argsList) = splitCommandArgs msg
        args = Array.fromList argsList
        message =
            case cmd of
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
                    Nothing
    in
        Maybe.withDefault NoOp message

makeClient : Array String -> Maybe Msg
makeClient args =
    if (Array.length args) == 1 then
        let
            client = createClient 
                ( atos 0 args )
        in 
            AddClient client |> Just
    else 
        Nothing

makePort : Array String -> List Client -> List Port -> Maybe Msg
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
            case client_port of
                Just the_port ->
                    Just (AddPort the_port)
                Nothing ->
                    Nothing
    else 
        Nothing

makeConnection : Array String -> List Port -> Maybe Msg
makeConnection args ports =
    if (Array.length args) == 2 then
        let
            connection = createConnection 
                ( atos 0 args )
                ( atos 1 args )
                ports
        in 
            case connection of
                Just the_connection ->
                    Just (AddConnection the_connection)
                Nothing ->
                    Nothing
    else 
        Nothing

delClient : Array String -> List Client -> Maybe Msg
delClient args clients =
    let
        client_name = atos 0 args
        maybe_client = getClientByName client_name clients
    in 
        case maybe_client of
            Just client ->
                Just (RmClient client)
            Nothing ->
                Nothing

delPort : Array String -> List Port -> Maybe Msg
delPort args ports =
    let 
        port_name = atos 0 args
        maybe_port = getPortByName port_name ports
    in 
        case maybe_port of
            Just client_port ->
                Just (RmPort client_port)
            Nothing ->
                Nothing

delConnection : Array String -> List Connection -> Maybe Msg
delConnection args connections =
    let 
        source_name = atos 0 args
        sink_name = atos 1 args
        maybe_connection = getConnectionByNames source_name sink_name connections
    in 
        case maybe_connection of
            Just connection ->
                Just (RmConnection connection)
            Nothing ->
                Nothing
