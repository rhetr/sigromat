module DebugView exposing (..)

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

