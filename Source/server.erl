% Server
-module(server).
-import(nim,[nim/0]).
-compile(export_all).
-define(PORT, 5423).

wait(X) -> receive after (1000*X) -> ok end.

getMinState(D) ->
	Comp = fun (Node, Load, {OldNode, OldLoad}) ->
		if Load < OldLoad -> {Node, Load};
		   true			      -> {OldNode, OldLoad} end end,
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
			receive X -> X end; %% TODO: Esto es una villereada para que no termine.
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

pReplier(Socket, Name) ->
  receive 
    die -> ok;
    {format, S, A} -> gen_tcp:send(Socket, io_lib:format(S, A)), pReplier(Socket, Name);
    S -> gen_tcp:send(Socket, S), pReplier(Socket, Name)
  end.

pHandler(Socket, Name, Replier) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Message} ->
      case string:tokens(Message, " \t\r\n") of
        % List available games
        ["LSG"] -> 
          Node = getMinState(),
          {gameManager, Node} ! {retrieve, all, self()}, receive GamesList -> ok end,
          HashesList = lists:map(fun fst/1, GamesList),
          Replier ! {format, "List: ~p~n", [HashesList]},
          io:format("List: ~p~n", [HashesList]),
          pHandler(Socket, Name, Replier);
        % Create new game
        ["NEW"] -> 
          gameManager ! {create, Replier, self()}, receive {GameHash, _} -> ok end,
          Replier ! {format,"GameID: ~p~n", [GameHash]},
          pHandler(Socket, Name, Replier);
        % Enter game
        ["ACC", HashString] ->
          GameHash = list_to_binary(HashString),
          io:format("gameHash: ~p~n", [GameHash]),
          
          %%gameRegistry ! {getPid, GameHash, self()},
          %%receive
          %%  error -> io:format("ono~n", []);
          %%  {ok, X} -> io:format("osi ~p~n", [X])
          %%end,
          gameManager ! {join, GameHash, Replier, self()}, %% Por que GameHash y no X? (el Pid)
                                                  %% potential answer: borrar el receive de arriba
          receive
            ok -> Replier ! "Has sido aceptado.~n";
            error -> error %% TODO: handle this
          end,
          pHandler(Socket, Name, Replier);
        % Make a movement
        ["PLA", HashString | Move ] -> 
          GameHash = list_to_binary(HashString),
          io:format("~p quiere jugar ~p~n",[Name, Move]),
          gameManager ! {play, GameHash, Move, self()},
          receive
            ok -> io:format("Ha jugado ~p~n",[Name]);
            error -> error %% TODO: handle this
          end,
          pHandler(Socket, Name, Replier);
        % Subscribe to game
        ["OBS", HashString] ->
          GameHash = list_to_binary(HashString),
          gameManager ! {subscribe, GameHash, Replier, self()},
          receive
            ok -> Replier ! "Has sido subscripto.~n";
            error -> error %% TODO: handle this
          end,
          pHandler(Socket, Name, Replier);
        % Unsubscribe from a game
        ["LEA", HashString] ->
          GameHash = list_to_binary(HashString), 
          gameManager ! {unsubscribe, GameHash, Replier, self()},
          receive
            ok -> Replier ! "Has sido desubscripto.~n";
            error -> error %% TODO: handle this too
          end,
          pHandler(Socket, Name, Replier);
        % Disconnect
        ["BYE", Name] -> 
          Replier ! "Chau.~n",
          ok;
        % Catch-all
        _ -> pHandler(Socket, Name, Replier)
      end;
    {error, ErrorMessage} ->
      %% VER QUÉ HACER
      io:format("Client ~p could not connect: ~p.~n", [Name, ErrorMessage]), error
  end.

%% pSocket: verifica que el nombre de Usuario no haya sido registrado previamente en la red
%% en caso de disponibilidad, se TRANSFORMA en pHandler
pSocket(Socket, N) -> 
	case gen_tcp:recv(Socket, 0) of
		{ok, Message} ->
			case string:tokens(Message, " \r\n") of
				["CON", Name] -> 
          Node = getNode(Name),
          {session, Node} ! {login, Name, self()},
          receive 
            ok -> io:format("Client[~p] (~p) has logged in.~n", [N, Name]),
                  Replier = spawn(?MODULE, pReplier, [Socket, Name]),
                  pHandler(Socket, Name, Replier);
            error -> gen_tcp:send(Socket, "El nombre de usuario ya está registrado.~n"), error
          end;
				_ -> gen_tcp:send(Socket, "Uso: 'CON username'.~n"), error
			end;
		{error, ErrorMessage } -> 
			io:format("Client[~p] could not connect: ~p.~n", [N, ErrorMessage]), error
	end.
	
fst(X) -> element(1, X).    
snd(X) -> element(2, X).    
    
lowerBound(X, [H|T]) ->
    Hash = fst(H),
    if
        X =< Hash -> snd(H);
        true      -> lowerBound(X, T)
    end.
    
-define(HASH, md5).


hash(X) ->
    if is_atom(X) -> Y = atom_to_list(X);
       is_pid(X)  -> Y = pid_to_list(X);
       true       -> Y = X
    end,
    base64:encode(crypto:hash(?HASH, Y)).

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

% taTeTi() -> taTeTi("........." (Tablero), [(Pid, Socket)] (Suscriptores), Bool (Turno), Char (Simbolo de jugada) )
taTeTi() -> taTeTi(".........", [], false, "x").
taTeTi(Board, Subscribers, Turn, Sym) ->
    receive
        {play, Jugada, Pid} -> ok;
        {subscribe, Replier, Pid} -> ok;
        {unsubscribe, Replier, Pid} -> ok;
        {join, Replier, Pid} -> ok;
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
        {create, Replier, Pid} ->
            io:format("~p quiere crear un game~n", [Pid]),
            Node = getMinState(),
            GamePid = spawn(Node, nim, nim, []),
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
        {play, GameHash, Jugada, Pid} ->
            NodeRegistry = getNode(GameHash),
            {gameRegistry, NodeRegistry} ! {getPid, GameHash, self()},
            receive {ok, GamePid} -> ok end, %% TODO: PUEDE QUE TE DEN UN HASH INVALIDO
            GamePid ! {play, Jugada, Pid},
            Pid ! ok,
            pGameManager(Games);
        {Action, GameHash, Replier, Pid} ->
            if (Action == join) or (Action == subscribe) or (Action == unsubscribe) ->
               NodeRegistry = getNode(GameHash),
               {gameRegistry, NodeRegistry} ! {getPid, GameHash, self()},
               receive {ok, GamePid} -> ok end, %% TODO: PUEDE QUE TE DEN UN HASH INVALIDO
               GamePid ! {Action, Replier, Pid},
               Pid ! ok
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
