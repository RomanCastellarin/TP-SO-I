% Server
-module(server).
-compile(export_all).
-define(PORT, 5423).

wait(X) -> receive after (1000*X) -> ok end.

getMinState(D) ->
	Comp = fun (Node, Load, {OldNode, OldLoad}) ->
		if Load < OldLoad -> {Node, Load};
		   true			  -> {OldNode, OldLoad} end end,
	{Node,_} = orddict:fold(Comp, {none, 1.0e10}, D),
    Node.

updateStates(D, K, V) ->
	orddict:update(K, fun (_)->V end, V, D).

% NetworkNode: 'pepe@192.168.15.05'
server(Port, NetworkNode) ->
	case net_adm:ping(NetworkNode) of
		pong -> server(Port);
		_	 -> io:format("No se pudo establecer la conexión.~n"), error
	end.

server(Port) ->
	case gen_tcp:listen(Port, [{active, false}]) of 
		{ok, ListenSocket} ->
			spawn(?MODULE, dispatcher, [ListenSocket, 0]),
			% all the other processes			
			ok;
		{error, ErrorMessage} ->
			io:format("No se pudo crear la conexión, ~p.~n", [ErrorMessage]),
			error
	end.

dispatcher(ListenSocket, N) ->
	{ok, SocketClient} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, pSocket, [SocketClient, N]),
	dispatcher(ListenSocket, N+1).
	
	
pBalance(NodesState) ->
    io:format("~p~n", [NodesState]),
	receive
		{getMin, Pid} ->
			Pid ! getMinState(NodesState),
			pBalance(NodesState);
		{notify, Node, Load} ->
			pBalance(updateStates(NodesState, Node, Load))
	end.


pStat() ->
	{_,Load} = statistics(reductions),
	lists:foreach(fun(Node) -> {balance, Node} ! {notify, node(), Load} end, [node()|nodes()]),
	wait(1),
	pStat().
	

pSocket(Socket, N) -> 
	case gen_tcp:recv(Socket, 0) of
		{ok, Message} ->
			case string:tokens(Message, " \r\n") of
				["CON", Name] -> io:format("Client[~p] se llama ~p~n", [N, Name]);
				X -> io:format("Client[~p] dice ~p~n", [N, X])
			end,
			pSocket(Socket, N);
		{error, ErrorMessage } -> 
			io:format("Client[~p] ha muerto porque ~p~n", [N, ErrorMessage])
	end,
	ok.
	
fst({A,_}) -> A.    
snd({_,B}) -> B.    
    
lowerBound(X, [H|T]) ->
    Hash = fst(H),
    if
        X =< Hash -> snd(H);
        true      -> lowerBound(X, T)
    end.
    
hashName(Name) ->   
    Nodes_list = [ node() | nodes() ],
    Nodes_hashes = lists:map(fun (X) -> {crypto:hash(md5,atom_to_list(X)), X} end, Nodes_list), 
    Ring = lists:sort(Nodes_hashes),
    Name_hash = crypto:hash(md5,Name),
    Last_hash = fst(lists:last(Ring)),
    io:format("~p ~p~n", [Name_hash, Ring]),
    if
        Last_hash < Name_hash -> snd(hd(Ring));
        true                  -> lowerBound(Name_hash, Ring)
    end.
    
    
funy() ->
	{A, B} = statistics(reductions),
	io:format("~p~n", [B]),
	funy().

funny() -> 
    register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
    spawn(?MODULE, pStat, []).
