module Graph exposing (..)

type PortType = Audio | MIDI
type FlowType = Source | Sink

type alias Client = 
    { name : String
    }

type alias Port =
    { name : String
    , client : Client
    , portType : PortType
    , flow : FlowType
    }

type alias Connection =
    { id : String
    , source : Port
    , sink : Port
    }

type alias Graph = 
    { clients : List Client
    , ports : List Port
    , connections : List Connection
    , sendMsg : String
    , recvMsg : List String
    }


emptyClient = (Client "")
emptyAudioSource = (Port "" emptyClient Audio Source)
emptyAudioSink = (Port "" emptyClient Audio Sink)
emptyMIDISource = (Port "" emptyClient MIDI Source)
emptyMIDISink = (Port "" emptyClient MIDI Sink)
emptyConnection = (Connection "" emptyAudioSource emptyAudioSink)

-- Setters

createClient : String -> Client
createClient client_name =
    { name = client_name
    }

-- port_name includes "client:" prefix
createPort : String -> String -> String -> String -> Graph -> Port
createPort port_name port_client_name port_type_str port_flow_str graph =
    let 
        port_client = getClientByName port_client_name graph
    in
        { name = port_name
        , client = port_client
        , portType = strToPortType port_type_str
        , flow = strToFlowType port_flow_str
        }

-- create from IDs
createConnection : String -> String -> String -> Graph -> Connection
createConnection connection_id source_name sink_name graph =
    let 
        source_port = getPortByName source_name graph
        sink_port = getPortByName sink_name graph
    in 
        { id = connection_id
        , source = source_port
        , sink = sink_port
        }

addClient : Client -> Graph -> Graph
addClient client graph =
    let
        new_clients = 
            if List.member client graph.clients
            then graph.clients
            else client :: graph.clients
    in 
        { graph | clients = new_clients }

addPort : Port -> Graph -> Graph
addPort client_port graph =
    let
        new_ports = 
            if List.member client_port graph.ports
            then graph.ports
            else client_port :: graph.ports
    in 
        { graph | ports = new_ports }

addConnection : Connection -> Graph -> Graph
addConnection connection graph =
    let
        new_connections = 
            if List.member connection graph.connections
            then graph.connections
            else connection :: graph.connections
    in 
        { graph | connections = new_connections }


removeFromList : a -> List a -> List a
removeFromList a listA =
    List.filter (\c -> c /= a) listA

-- removeClientFromList : Client -> Graph -> List Client
-- removeClientFromList client graph =
--     List.filter (\c -> c /= client) graph.clients
-- 
-- removePortFromList : Port -> List Port -> List Port
-- removePortFromList client_port ports =
--     List.filter (\c -> c /= client_port) ports
-- 
-- removeConnectionFromList : Connection -> Graph -> List Connection
-- removeConnectionFromList connection graph = 
--     List.filter (\c -> c /= connection) graph.connections

removeClient : Client -> Graph -> Graph
removeClient client graph =
    let
        new_clients = removeFromList client graph.clients
        new_ports = List.foldr removeFromList graph.ports (getClientPorts client graph.ports)
        new_connections = List.foldr removeFromList graph.connections (getClientConnections client graph.ports graph.connections)
    in
        Graph new_clients new_ports new_connections graph.sendMsg graph.recvMsg


removePort : Port -> Graph -> Graph
removePort client_port graph =
    let 
        new_ports = removeFromList client_port graph.ports
        new_connections = List.foldr removeFromList graph.connections (getPortConnections client_port graph.connections)
    in
        Graph graph.clients new_ports new_connections graph.sendMsg graph.recvMsg

removeConnection : Connection -> Graph -> Graph
removeConnection connection graph =
    let
        new_connections = removeFromList connection graph.connections
    in
        { graph | connections = new_connections }

-- Getters

getClientByName : String -> Graph -> Client
getClientByName client_name graph =
    List.filter (\client -> client.name == client_name) graph.clients 
        |> List.head 
        |> Maybe.withDefault emptyClient

getPortByName : String -> Graph -> Port
getPortByName port_name graph =
    List.filter (\client_port -> client_port.name == port_name) graph.ports
        |> List.head 
        |> Maybe.withDefault emptyAudioSource

getConnectionByID : String -> Graph -> Connection
getConnectionByID connection_id graph =
    List.filter (\connection -> connection.id == connection_id) graph.connections
        |> List.head 
        |> Maybe.withDefault emptyConnection

getSourcePorts : List Port -> List Port
getSourcePorts ports =
    List.filter (\client_port -> client_port.flow == Source) ports

getSinkPorts : List Port -> List Port
getSinkPorts ports =
    List.filter (\client_port -> client_port.flow == Sink) ports

getAudioPorts : List Port -> List Port
getAudioPorts ports =
    List.filter (\client_port -> client_port.portType == Audio) ports

getMIDIPorts : List Port -> List Port
getMIDIPorts ports =
    List.filter (\client_port -> client_port.portType == MIDI) ports

getClientPorts : Client -> List Port -> List Port
getClientPorts client ports =
    List.filter (\client_port -> client_port.client == client) ports

-- need to remove dupes
getClientConnections : Client -> List Port -> List Connection -> List Connection
getClientConnections client ports connections =
    getClientPorts client ports
        |> List.map (flip getPortConnections connections)
        |> List.concat

getPortConnections : Port -> List Connection -> List Connection
getPortConnections client_port connections =
    case client_port.flow of
        Source ->
            List.filter (\connection -> connection.source == client_port) connections
        Sink ->
            List.filter (\connection -> connection.sink == client_port) connections

-- Helpers

strToPortType : String -> PortType
strToPortType portTypeStr =
    if portTypeStr == "Audio"
    then Audio
    else MIDI

strToFlowType : String -> FlowType
strToFlowType flowTypeStr = 
    if flowTypeStr == "Source"
    then Source
    else Sink
