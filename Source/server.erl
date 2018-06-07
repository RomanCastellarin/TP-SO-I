% Server
-module(server).
-compile(export_all).
-define(PORT, 5423).

server(Port, NetworkNode) ->
	case net_adm:ping(NetworkNode) of
		pong -> server(Port);
		_	 -> io:format("No se pudo establecer la conexión.~n"), error
	end.

server(Port) ->
	case gen_tcp:listen(Port, [{active, false}]) of 
		{ok, ListenSocket} ->
			spawn(?MODULE, dispatcher, [ListenSocket, 0]),
			ok;
		{error, ErrorMessage} ->
			io:format("No se pudo crear la conexión, ~p.~n", [ErrorMessage]),
			error
	end.

dispatcher(ListenSocket, N) ->
	{ok, SocketClient} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, pSocket, [SocketClient, N]),
	dispatcher(ListenSocket, N+1).
	
	
pBalance() ->
	Nodes = [ node() | nodes() ],
	ok.
	
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
	

	

