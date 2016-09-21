module DebugView exposing (..)

import Html exposing (Html, h2, h3, button, div, text, input)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import String

import Graph exposing (..)
import Messages exposing (..)
import Helpers exposing (..)

view : Graph -> Html Msg
view graph =
    div [ style
            [ ("margin","20px")
            , ("padding", "20px")
            , ("font-family", "droid sans mono, slab-serif")
            , ("font-size", "10px")
            , ("padding", "20px")
            , ("border", "1px solid black")
            , ("float", "left")
            ]
        ]
        [ div
            [ style 
                [ ("border", "1px dotted black")
                , ("width","40%")
                , ("float","right")
                , ("padding", "0 1%")
                ]
            ] 
            [ inputCmd graph
            , receivedMsgs graph
            , parsedMsgs graph
            ]
        , div
            [ style
                [ ("width", "60%")
                ]
            ]
            [ div
                [ style
                    [ ("float", "left")
                    ]
                ]
                [ h2 [] [ text "clients" ]
                , div [] (List.map showClient graph.clients)
                ]
            , div
                [ style
                    [ ("float", "left")
                    ]
                ]
                [ h2 [] [ text "ports" ]
                , showPorts "Audio Source" (graph.ports |> getAudioPorts |> getSourcePorts)
                , showPorts "Audio Sink" (graph.ports |> getAudioPorts |> getSinkPorts)
                , showPorts "MIDI Source" (graph.ports |> getMIDIPorts |> getSourcePorts)
                , showPorts "MIDI Sink" (graph.ports |> getMIDIPorts |> getSinkPorts)
                ]
            , div 
                [ style
                    [ ("float", "left")
                    ]
                ]
                [ h2 [] [ text "connections" ]
                , showConnections "Audio Connections" (graph.connections |> getAudioConnections)
                , showConnections "MIDI Connections" (graph.connections |> getMIDIConnections)
                ]
            ]
        ]


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
        Json.Decode.map tagger Html.Events.keyCode
            |> Html.Events.on "keydown"



receivedMsgs : Graph -> Html msg
receivedMsgs graph = 
    div [] 
        [ h2 [] [ text "received messages" ]
        , div [] (List.map viewMessage (List.reverse graph.recvMsg))
        ]

viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]



parsedMsgs : Graph -> Html msg
parsedMsgs graph =
    div []
    [ h2 [] [ text "parsed messages" ]
    , div [] (List.map parsedMessage (List.reverse graph.recvMsg))
    ]

parsedMessage : String -> Html msg
parsedMessage msg =
    let msgs = msg
        |> String.split "?"
        |> List.sortWith (listStringComparison "/")
        |> List.map viewMessage
    in
        div 
            [ style
                [ ("border-top", "1px dotted black")
                ]
            ]
            msgs

