module View exposing (..)

import Html exposing (Html)
import Html exposing (h2, h3, button, div, span, text, input, param)
import Html exposing (table, tbody, tfoot, tr, th, td)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onClick, onInput, on, onMouseUp, onMouseOver, onMouseLeave, keyCode)
import Json.Decode as Json
import String

import Graph exposing (..)
import Messages exposing (..)
import Helpers exposing (..)

mainStyle =
    [ ("margin","2%")
    , ("padding", "1%")
    , ("font-family", "droid sans mono, slab-serif")
    , ("font-size", "10px")
    , ("padding", "20px")
    , ("border", "1px solid black")
    , ("float", "left")
    , ("width", "90%")
    ]

messageStyle =
    [ ("border", "1px dotted black")
    , ("width","40%")
    , ("float","right")
    , ("padding", "0 1%")
    ]


debugView : Graph -> Html Msg
debugView graph =
    div [ style mainStyle ]
        [ div [ style messageStyle ] 
            [ inputCmd graph
            , receivedMsgs graph
            , parsedMsgs graph
            ]
        , div [ style [ ("width", "60%") ] ]
            [ div [ style [ ("float", "left"), ("width","50%")] ]
                [ h2 [] [ text "clients" ]
                , div [] (List.map showClient graph.clients)
                ]
            , div [ style [ ("float", "left") ] ]
                [ h2 [] [ text "ports" ]
                , showPorts "Audio Source" (graph.ports |> getAudioPorts |> getSourcePorts)
                , showPorts "Audio Sink" (graph.ports |> getAudioPorts |> getSinkPorts)
                , showPorts "MIDI Source" (graph.ports |> getMIDIPorts |> getSourcePorts)
                , showPorts "MIDI Sink" (graph.ports |> getMIDIPorts |> getSinkPorts)
                ]
            , div [ style [ ("float", "left") ] ]
                [ h2 [] [ text "connections" ]
                , showConnections "Audio Connections" (graph.connections |> getAudioConnections)
                , showConnections "MIDI Connections" (graph.connections |> getMIDIConnections)
                ]
            ]
        ]

matrixDiv : Graph -> Html Msg
matrixDiv graph =
    let 
        audioGraph = { graph | ports = (graph.ports |> getAudioPorts) }
        midiGraph = { graph | ports = (graph.ports |> getMIDIPorts) }
    in
        div []
            [ div [ style mainStyle ]
                [ h2 [] [ text "audio" ]
                , matrixTable audioGraph
                ]
            , div [ style mainStyle ] 
                [ h2 [] [ text "midi" ]
                , matrixTable midiGraph
                ]
            ]


view : Graph -> Html Msg
view graph =
    div []
        [ matrixDiv graph 
        , debugView graph
        ]

showClient : Client -> Html msg
showClient client =
    div [] [ text (client.name ++ "\n") ]

showPort : Port -> Html msg
showPort client_port =
    div [] [ text (client_port.client.name ++ ":" ++ client_port.name) ]


showConnection : Connection -> Html msg
showConnection connection =
    let
        cstr = 
            connection.source.client.name ++ ":" ++ connection.source.name ++
            "->" ++ connection.sink.client.name ++ ":" ++ connection.sink.name
    in
        div [] [ text cstr ]

showPorts : String -> List Port -> Html msg
showPorts name ports =
    div [ style
            [ ("border","1px dotted black")
            , ("float", "left")
            , ("padding", "0 2%")
            , ("width", "20%")
            ]
        ]
        [ h3 [] [ text name ]
        , div [] (List.map showPort ports)
        ]

showConnections : String -> List Connection -> Html msg 
showConnections name connections =
    div [ style
            [ ("border","1px dotted black")
            , ("float", "left")
            , ("padding", "0 2%")
            ]
        ]
        [ h3 [] [ text name ]
        , div [] (List.map showConnection connections)
        ]

inputCmd : Graph -> Html Msg
inputCmd graph =
    div []
        [ h2 [] [ text "command" ]
        , input 
            [ onInput InputMessage
            , onEnter SendMessage
            , value graph.sendMsg
            ] []
        , button [ onClick SendMessage ] [ text "send" ]
        ]


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13
            then msg 
            else NoOp
    in
        Json.map tagger keyCode
            |> Html.Events.on "keydown"

receivedMsgs : Graph -> Html msg
receivedMsgs graph = 
    div [] 
        [ h2 [] [ text "received messages" ]
        , div [] (List.map viewMessage (List.reverse graph.recvMsg))
        ]

viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text (msg ++ " &") ]

viewMessageSpan : String -> String -> Html msg
viewMessageSpan color msg =
    span [ style [("margin-right","5px"), ("color",color)] ] [ text msg ]

parsedMsgs : Graph -> Html msg
parsedMsgs graph =
    div []
        [ h2 [] [ text "parsed messages" ]
        , div [] (List.map parsedMessage (List.reverse graph.recvMsg))
        ]


showCmdArgs : String -> List String -> Html msg
showCmdArgs cmd args =
    div []
        [ (viewMessageSpan "black" cmd)
        , (viewMessageSpan "blue" (String.join " " args))
        ]


parsedMessage : String -> Html msg
parsedMessage msg =
    let 
        (cmds, args) = msg
            |> String.split "&"
            |> List.map String.trim
            |> List.sortWith (listStringComparison "/")
            |> List.map splitCommandArgs
            |> List.unzip
    in
        div 
                                [ style [ ("border-top", "1px dotted black") ] ]
            (List.map2 showCmdArgs cmds args)


makeSourceRow : List Port -> List Connection -> Port -> Html Msg
makeSourceRow sinks connections source =
    let 
        trContent = 
            [ th [] [ div [ style [("transform", "rotate(45deg)")]] [ span [] [ text (source.client.name ++ ":" ++ source.name) ] ] ] ]
            ++ List.map (makeSinkRow connections source.name) sinks
    in 
        tr [] trContent


boxC =
    [("box-shadow","inset 0px 0px 0px 6px white"), ("background-color","black")]

makeSinkRow : List Connection -> String -> Port -> Html Msg
makeSinkRow connections source_name sink =
    let 
        connected =
            List.filter (\connection -> connection.source.name == source_name) connections
                |> List.map .sink
                |> List.member sink
        show =
            if connected then boxC else []
        message =
            if connected
            then ("/client/port/connection/remove " ++ source_name ++ " " ++ sink.name)
            else ("/client/port/connection/add " ++ source_name ++ " " ++ sink.name)
    in
        td 
            [ style ( 
                [ ("border","1px solid black")
                ] ++ show )
            ]
            [ div
                [ style 
                    [ ("width", "30px")
                    , ("height", "30px")
                    , ("color","white")
                    ]
                , onMouseOver ( InputMessage message )
                , onMouseLeave ( InputMessage "" )
                , onClick SendMessage
                , onMouseUp ( InputMessage message )
                ]
                [ text "." ]
            ]


sinkLabel =
    [ ("transform","rotate(45deg)")
    , ("transform-origin", "left bottom")
    , ("width","0")
    ]

makeSinkFootHeader : Port -> Html msg
makeSinkFootHeader sink =
    th [] [ div [ style sinkLabel ] [ span [] [ text (sink.client.name ++ ":" ++ sink.name) ] ] ]

matrixTable : Graph -> Html Msg
matrixTable graph = 
    let
        sources = getSourcePorts graph.ports
        sinks = getSinkPorts graph.ports
    in
        table 
            [ style 
                [ ("border-collapse","collapse")
                , ("table-layout", "fixed")
                , ("transform","rotate(-45deg)") 
                , ("margin", "0 auto")
                ] 
            ]
            [ tbody [] (List.map (makeSourceRow (List.reverse sinks) graph.connections) sources)
            , tfoot [] ([ th [] [] ] ++ (List.map makeSinkFootHeader (List.reverse sinks)) )
            ]
