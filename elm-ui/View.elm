module View exposing (..)

import Html exposing (Html, h2, h3, button, div, text, input)
import Html.Attributes exposing (value, style)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)

import DebugView exposing (..)

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

