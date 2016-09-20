**J**ack **A**utomatable **M**atrix **P**atcher

j-a-m-pa jack automable matrix patcher

j-a-p-en jack automable patching engine

si-fl-en signal flow engine 


au-fl-en audio flow engine - awful


osc requirements -

send multiple commands in a single message
messages are prioritized



osc messages to server:
1. add connection $connection -> { out = $outport, in = $inport }
2. remove connection $connection
3. refresh (send all adds)


messages to gui:
1. add client $client -> { name = $clientname }
2. remove client $client
3. add port $port -> { name = $portname, client = $client, type = $porttype, flow = $flowtype }
4. remove port $port
5. add connection $connection -> { out = $outport, in = $inport }
6. remove connection $connection

/add
  /client
    id 		int
    name 	str
  /port
    id 		int
    name 	str
    client 	int
    type 	int
    flow 	int
  /connection
    id		int
    out 	int
    in 		int

/remove
  /client
    id 		int
  /port
    id		int
  /connection
    id		int

must have unique ID
