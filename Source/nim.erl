% Nim
-module(nim).
-compile(export_all).
 
update(L, I, V) ->
    P = lists:sublist(L,1,I-1),
    S = lists:nthtail(I, L),
    P ++ [V] ++ S.
 
nim() ->
    State = [10+random:uniform(10), 10+random:uniform(10), 10+random:uniform(10)],
    nim(State, [], []).
 
 
%% formato de jugada "%d %d" (numero de pila, cuanto)
nim(State, Players, Subscribers) ->
    NPlayers = length(Players),      
    receive
        {play, Jugada, Pid} ->
            [Jug2, {Pid1, Replier}] = Players,
            if Pid == Pid1 ->  
               case Jugada of
                   [PilaString, NumberString] ->
                       try {list_to_integer(PilaString), list_to_integer(NumberString)} of
                           {PilaInt, NumberInt} ->
                               NewState = update(State, PilaInt, lists:nth(PilaInt, State) - NumberInt),
                                %% chequear que sea valida toda esta wea de arriba
                               lists:foreach( fun (R) -> R ! {format, "~p~n", [NewState]}, ok end, Subscribers),
                               nim(NewState, [{Pid1, Replier}, Jug2], Subscribers)
                       catch
                          _:_ -> Pid ! error, nim(State, Players, Subscribers)
                       end;
                   true -> Pid ! error, nim(State, Players, Subscribers)
               end;
               true -> Pid ! error, nim(State, Players, Subscribers)
            end;
        {join, Replier, Pid} ->            
            if  NPlayers < 2 -> 
                lists:foreach( fun (R) -> R ! {format, "~p~n", [State]}, ok end, [Replier]),                
                nim(State, [{Pid, Replier} | Players], [Replier | Subscribers]);
                true -> Pid ! error, nim(State, Players, Subscribers)
            end;            
        {subscribe, Replier, Pid} ->
            Pid ! ok, %% ja foi            
            nim(State, Players, [Replier | Subscribers]);
        {unsubscribe, Replier, Pid} ->
            Pid ! ok,            
            nim(State, Players, Subscribers -- [Replier])
    end.
