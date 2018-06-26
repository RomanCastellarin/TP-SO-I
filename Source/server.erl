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
			receive X -> X end;
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

getMinState() ->
 balance ! {getMin, self()}, receive Node -> Node end.

pHandler(Socket, Name) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Message} ->
      case string:tokens(Message, " \r\n") of
        % List available games
        ["LSG"] -> 
          Node = getMinState(),
          {gameManager, Node} ! {retrieve, all, self()}, receive GamesList -> ok end,
          gen_tcp:send(Socket, io_lib:format("List: ~p~n", [GamesList])),
          io:format("List: ~p~n", [GamesList]),
          pHandler(Socket, Name);
          
        % Create new game
        ["NEW"] -> 
          gameManager ! {create, self()}, receive {GameHash, GamePid} -> ok end,
          gen_tcp:send(Socket, io_lib:format("GameID: ~p~n", [GameHash])),
          pHandler(Socket, Name)
        %~ % Enter game
        %~ ["ACC", GameHash] ->
        %~ 
        %~ % Make a movement
        %~ ["PLA" | Name] -> 
           %~ ???
        %~ % Subscribe to game
        %~ ["OBS", Name] -> 
        %~ 
        %~ % Unsubscribe from a game
        %~ ["LEA", Name] -> 
        %~ 
        %~ % Disconnect
        %~ ["BYE", Name] -> 
      end;
    {error, ErrorMessage} ->
      %% VER QUÉ HACER
      io:format("Client ~p could not connect.~n", [Name, ErrorMessage]), error
  end.

pSocket(Socket, N) -> 
	case gen_tcp:recv(Socket, 0) of
		{ok, Message} ->
			case string:tokens(Message, " \r\n") of
				["CON", Name] -> 
          Node = getNode(Name),
          {session, Node} ! {login, Name, self()},
          receive 
            ok -> io:format("Client[~p] (~p) has logged in.~n", [N, Name]), pHandler(Socket, Name);
            error -> gen_tcp:send(Socket, "El nombre de usuario ya está registrado.~n"), error
          end;
				X -> gen_tcp:send(Socket, "Uso: 'CON username'.~n"), error
			end;
		{error, ErrorMessage } -> 
			io:format("Client[~p] could not connect.~n", [N, ErrorMessage]), error
	end.
	
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
    io:format("HASHEAR: ~p~n", [X]),
    if is_atom(X) -> crypto:hash(?HASH,atom_to_list(X));
       is_pid(X)  -> crypto:hash(?HASH,pid_to_list(X));
       true       -> crypto:hash(?HASH,X)
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
taTeTi() -> taTeTi(".........", [], false, "x").
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

pGameRegistry(RegisteredGames) ->
  receive
    {store, GameTuple} ->
      pGameRegistry([GameTuple | RegisteredGames]);
    {getPid, GameHash, Pid} ->
      case proplists:get_value(GameHash, RegisteredGames) of
          undefined -> Pid ! error;
          GamePid -> Pid ! {ok, GamePid}
      end,
      pGameRegistry(RegisteredGames)
  end.


pGameManager(Games) ->
    receive
        {create, Pid} ->
            io:format("~p quiere crear un game~n", [Pid]),
            Node = getMinState(),
            GamePid = spawn(Node, ?MODULE, taTeTi, []),
            NewGame = {hash(GamePid), GamePid},
            NodeRegistry = getNode(fst(NewGame)),
            {gameRegistry, NodeRegistry} ! {store, NewGame},
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
                NodeRegistry = getNode(GameHash),
                {gameRegistry, NodeRegistry} ! {getPid, GameHash, self()},
                receive GamePid -> ok end,
                GamePid ! {Action, Pid}
            end,
            pGameManager(Games)
    end.

startup(Port) -> 
    register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
    register(gameManager, spawn(?MODULE, pGameManager, [[]])),
    register(session, spawn(?MODULE, pSession, [[]])),
    register(gameRegistry, spawn(?MODULE, pGameRegistry, [[]])),
    spawn(?MODULE, pStat, []),
    server(Port).

startup(Port, Node) -> 
    register(balance, spawn(?MODULE, pBalance, [orddict:new()])),
    register(gameManager, spawn(?MODULE, pGameManager, [[]])),
    register(session, spawn(?MODULE, pSession, [[]])),
    register(gameRegistry, spawn(?MODULE, pGameRegistry, [[]])),
    spawn(?MODULE, pStat, []),
    server(Port, Node).
