module Graph exposing (..)

type PortType = Audio | MIDI
type FlowType = Source | Sink

type alias Client = 
    { id : Int
    , name : String
    }

type alias Port =
    { id : Int
    , name : String
    , client : Client
    , portType : PortType
    , flow : FlowType
    }

type alias Connection =
    { id : Int
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


emptyClient = (Client 0 "")
emptyAudioSource = (Port 0 "" emptyClient Audio Source)
emptyAudioSink = (Port 0 "" emptyClient Audio Sink)
emptyMIDISource = (Port 0 "" emptyClient MIDI Source)
emptyMIDISink = (Port 0 "" emptyClient MIDI Sink)
emptyConnection = (Connection 0 emptyAudioSource emptyAudioSink)

-- Setters

createClient : Int -> String -> Client
createClient client_id client_name =
    { id = client_id
    , name = client_name
    }

-- create from IDs
createPort : Int -> String -> Int -> Int -> Int -> Graph -> Port
createPort port_id port_name port_client_id port_type_int port_flow_int graph =
    let 
        port_client = getClientByID port_client_id graph
    in
        { id = port_id
        , name = port_name
        , client = port_client
        , portType = intToPortType port_type_int
        , flow = intToFlowType port_flow_int
        }

-- create from IDs
createConnection : Int -> Int -> Int -> Graph -> Connection
createConnection connection_id source_id sink_id graph =
    let 
        source_port = getPortByID source_id graph
        sink_port = getPortByID sink_id graph
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

removeClient : Client -> Graph -> Graph
removeClient client graph =
    let
        new_clients = List.filter (\c -> c /= client) graph.clients
    in
        { graph | clients = new_clients }

removePort : Port -> Graph -> Graph
removePort client_port graph =
    let
        new_ports = List.filter (\c -> c /= client_port) graph.ports
    in
        { graph | ports = new_ports }

removeConnection : Connection -> Graph -> Graph
removeConnection connection graph =
    let
        new_connections = List.filter (\c -> c /= connection) graph.connections
    in
        { graph | connections = new_connections }

-- Getters

getClientByID : Int -> Graph -> Client
getClientByID client_id graph =
    List.filter (\client -> client.id == client_id) graph.clients 
        |> List.head 
        |> Maybe.withDefault emptyClient

getPortByID : Int -> Graph -> Port
getPortByID port_id graph =
    List.filter (\client_port -> client_port.id == port_id) graph.ports
        |> List.head 
        |> Maybe.withDefault emptyAudioSource

getConnectionByID : Int -> Graph -> Connection
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

getSourceConnections : Port -> List Connection -> List Connection
getSourceConnections client_port connections =
    List.filter (\connection -> connection.source == client_port) connections

getSinkConnections : Port -> List Connection -> List Connection
getSinkConnections client_port connections =
    List.filter (\connection -> connection.sink == client_port) connections

-- Helpers

intToPortType : Int -> PortType
intToPortType portTypeInt =
    if portTypeInt == 0
    then Audio
    else MIDI

intToFlowType : Int -> FlowType
intToFlowType flowTypeInt = 
    if flowTypeInt == 0
    then Source
    else Sink
