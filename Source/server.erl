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
	
	
pBalance(NodesStates) ->
    %io:format("~p~n", [NodesState]),
	receive
		{getMin, Pid} ->
			Pid ! getMinState(NodesStates), pBalance(NodesStates);
		{notify, Node, Load} ->
			pBalance(updateStates(NodesStates, Node, Load))
	end.


pStat() ->
	{_,Load} = statistics(reductions),
	lists:foreach(fun(Node) -> {balance, Node} ! {notify, node(), Load} end, allNodes()),
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
    
-define(HASH, md5).

hash(X) ->
    if is_list(X) -> crypto:hash(?HASH,X);
       is_atom(X) -> crypto:hash(?HASH,atom_to_list(X));
       true       -> crypto:hash(?HASH,pid_to_list(X))
    end.

getNode(Name) ->
    Nodes_list = allNodes(),
    Nodes_hashes = lists:map(fun (X) -> {hash(X), X} end, Nodes_list), 
    Ring = lists:sort(Nodes_hashes),
    Name_hash = hash(Name),
    Last_hash = fst(lists:last(Ring)),
    %io:format("~p ~p~n", [Name_hash, Ring]),
    if
        Last_hash < Name_hash -> snd(hd(Ring));
        true                  -> lowerBound(Name_hash, Ring)
    end.
    

pSession(RegisteredUsers) ->   
    io:format("~p registered users~n", [RegisteredUsers]),
    receive
        {login, Username, Pid} ->
            case lists:member(Username, RegisteredUsers) of
                true  -> Pid ! error, pSession(RegisteredUsers);                
                false -> Pid ! ok, pSession([Username | RegisteredUsers])            
            end;
        {logout, Username, Pid} ->
            Pid ! ok, pSession(lists:delete(Username, RegisteredUsers)) 
    end.

% taTeTi() -> taTeTi("........." (Tablero), [Pid] (Suscriptores), Pid (Turno), Char (Simbolo de jugada) )
taTeTi() -> taTeTi().
taTeTi(Board, Subscribers, Turn, Sym) ->
    receive
        {play, Jugada, Pid} -> ok;
        {subscribe, Pid} -> ok;
        {join, Pid} -> ok;
        _ -> ok
    end.

allNodes() -> [node() | nodes()].

getAllGames(Games) ->
    AllGames = Games ++ lists:map(
        fun(Node)-> 
            {gameManager, Node} ! {retrieve, local, self()},
            receive X->X end end,
        nodes()
    ).

pGameManager(Games) ->
    receive
        {create, Pid} ->
            io:format("~p quiere crear un game~n", [Pid]),
            balance ! {getMin, self()},
            receive Node -> ok end,
            GamePid = spawn(Node, ?MODULE, taTeTi, []),
            NewGame = {hash(GamePid), GamePid},
            Pid ! NewGame,
            pGameManager([NewGame | Games]);
        {retrieve, local, Pid} ->
            Pid ! Games,
            pGameManager(Games);
        {retrieve, all, Pid} ->
            AllGames = getAllGames(Games),
            Pid ! AllGames,
            pGameManager(Games);
        {Action, GameHash, Pid} ->
            if (Action == join) or (Action == subscribe) ->
                    AllGames = getAllGames(Games),
                    case proplists:get_value(GameHash, AllGames) of
                        undefined -> Pid ! error;
                        GamePid -> GamePid ! {Action, Pid}
                    end;
               true -> Pid ! error
            end,
            pGameManager(Games)
    end.


funy() ->
	{A, B} = statistics(reductions),
	io:format("~p~n", [B]),
	funy().

funny() -> 
    register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
    register(gameManager, spawn(?MODULE, pGameManager, [[]])),
    register(session, spawn(?MODULE, pSession, [[]])),
    spawn(?MODULE, pStat, []).
