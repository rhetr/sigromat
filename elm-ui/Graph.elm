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
    { source : Port
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
emptyConnection = (Connection emptyAudioSource emptyAudioSink)

-- Setters

createClient : String -> Client
createClient client_name =
    { name = client_name
    }

-- port_name includes "client:" prefix
createPort : String -> String -> String -> String -> List Client -> List Port -> Maybe Port
createPort port_name port_client_name port_type_str port_flow_str clients ports =
    let
        maybe_client = getClientByName port_client_name clients
        maybe_type = strToPortType port_type_str
        maybe_flow = strToFlowType port_flow_str
        all_just =
                isJust maybe_client
                && isJust maybe_type
                && isJust maybe_flow
    in
        case all_just of
            False ->
                Nothing
            True ->
                let
                    -- none of these should default
                    port_client = Maybe.withDefault emptyClient maybe_client
                    port_type = Maybe.withDefault Audio maybe_type
                    port_flow = Maybe.withDefault Source maybe_flow
                    port_exists  =
                        getClientPorts port_client ports
                            |> List.map .name
                            |> List.member port_name
                in
                    case (not port_exists)  of
                        True ->
                            Just
                            { name = port_name
                            , client = port_client
                            , portType = port_type
                            , flow = port_flow
                            }
                        False ->
                            Nothing


-- needs to connect by porttype and flowtype
createConnection : String -> String -> List Port -> Maybe Connection
createConnection source_name sink_name ports =
    let 
        source_port = getPortByName source_name ports |> Maybe.withDefault emptyAudioSource
        sink_port = getPortByName sink_name ports |> Maybe.withDefault emptyAudioSink
        validConnection = 
               source_port /= emptyAudioSource
            && sink_port /= emptyAudioSink
            && source_port.portType == sink_port.portType
            && source_port.flow == Source 
            && sink_port.flow == Sink
    in 
        case validConnection of
            True ->
                Just
                { source = source_port
                , sink = sink_port
                }
            False -> 
                Nothing

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

removeClient : Client -> Graph -> Graph
removeClient client graph =
    let
        new_clients = removeFromList client graph.clients
        new_ports = List.foldr removeFromList graph.ports (getClientPorts client graph.ports)
        new_connections = List.foldr removeFromList graph.connections (getClientConnections client graph.ports graph.connections)
    in
        { graph |
            clients  = new_clients,
            ports = new_ports,
            connections = new_connections
        }

removePort : Port -> Graph -> Graph
removePort client_port graph =
    let 
        new_ports = removeFromList client_port graph.ports
        new_connections = List.foldr removeFromList graph.connections (getPortConnections client_port graph.connections)
    in
        { graph |
            ports = new_ports,
            connections = new_connections
        }

removeConnection : Connection -> Graph -> Graph
removeConnection connection graph =
    let
        new_connections = removeFromList connection graph.connections
    in
        { graph | connections = new_connections }

-- Getters

getClientByName : String -> List Client -> Maybe Client
getClientByName client_name clients =
    List.filter (\client -> client.name == client_name) clients 
        |> List.head 

getPortByName : String -> List Port -> Maybe Port
getPortByName port_name ports =
    List.filter (\client_port -> client_port.name == port_name) ports
        |> List.head 

getConnectionByNames : String -> String -> List Connection -> Maybe Connection
getConnectionByNames source_name sink_name connections =
    List.filter (\connection -> connection.source.name == source_name && connection.sink.name == sink_name) connections
        |> List.head
        
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

getClientConnections : Client -> List Port -> List Connection -> List Connection
getClientConnections client ports connections =
    getClientPorts client ports
        |> List.map (flip getPortConnections connections)
        |> List.concat
        |> makeSetList

getPortConnections : Port -> List Connection -> List Connection
getPortConnections client_port connections =
    case client_port.flow of
        Source ->
            List.filter (\connection -> connection.source == client_port) connections
        Sink ->
            List.filter (\connection -> connection.sink == client_port) connections

getAudioConnections : List Connection -> List Connection
getAudioConnections connections =
    List.filter (\connection -> connection.source.portType == Audio) connections

getMIDIConnections : List Connection -> List Connection
getMIDIConnections connections =
    List.filter (\connection -> connection.source.portType == MIDI) connections

-- Helpers

sortConnections : Connection -> Connection -> Order
sortConnections a b =
    let
        sourceA = a.source.name
        sinkA = a.sink.name
        sourceB = b.source.name
        sinkB = b.sink.name
    in
        case compare sourceA sourceB of
            LT -> LT
            GT -> GT
            EQ -> 
                case compare sinkA sinkB of
                    LT -> LT
                    GT -> GT
                    EQ -> EQ

uniqueAdd : a -> List a -> List a
uniqueAdd a list =
    if not (List.member a list)
    then a :: list
    else list

makeSetList : List a -> List a
makeSetList list =
    List.foldr uniqueAdd [] list

-- these need to be able to fail
strToPortType : String -> Maybe PortType
strToPortType portTypeStr =
    case portTypeStr of
        "Audio" -> Just Audio
        "MIDI" -> Just MIDI
        _ -> Nothing

strToFlowType : String -> Maybe FlowType
strToFlowType flowTypeStr = 
    case flowTypeStr of
        "Source" -> Just Source
        "Sink" -> Just Sink
        _ -> Nothing

isJust : Maybe a -> Bool
isJust a =
    case a of
        Just a ->
            True
        Nothing ->
            False
