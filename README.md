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

must have unique ID

https://www.npmjs.com/package/osc
http://stackoverflow.com/questions/16108714/haskell-removing-duplicates-from-a-list

- delete client isn't deleting related ports and connections
- duplicates need to be removed from getClientConnections
- ports should include the prefix in their name
- things need to be renamable (from the backend) while retaining the same position
- ignore invalid ports and connections
- batch isn't working
- configure sort hierarchy


using parameters

/refresh

/client/add 
	name 
/client/remove 
	name
/client/port/add
	name client type flow
/client/port/remove
	name

/client/port/connection/add
	id source sink
/client/port/connection/remove
	id


using address as parameters
/client_name/add
/client_name/remove

/client_name/port_name/add type flow
/client_name/port_name/remove

/client_name/source_port/connection_name/add sink_port
/client_name/source_port/connection_name/remove sink_port
