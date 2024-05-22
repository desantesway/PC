-module(lobby).
-include("server.hrl").
-export[start/0].

start() -> lobby(#{}).
% gestor de salas do jogo
lobby(Rooms) -> % only logged can create room CHANGE
    io:format("Rooms ~p~n", [Rooms]),
    receive
        {countdown_started, CountProc, Room}  ->
            {_, Lvl, Pids} = maps:get(Room, Rooms),
            NRooms = maps:put(Room, {CountProc, Lvl, Pids}, Rooms),
            lists:foreach(fun(Pid) -> ?SEND_MESSAGE(Pid, "countdown_started\n") end, Pids),
            lobby(NRooms);
        {start_game, Room, Game}  ->
            {CountProc, _, Pids} = maps:get(Room, Rooms),
            if CountProc == self() ->
                continue;
            true ->
                lists:foreach(fun(Pid) -> Pid ! {start_game, Game}, ?SEND_MESSAGE(Pid, "enter_game\n") end, Pids)
            end,
            lobby(Rooms);
        {join, User, Lobby, Room, Level, Pid} -> % jogador tenta entrar numa sala
            if Lobby == "main" ->
                if User == "Anonymous" ->
                    ?SEND_MESSAGE(Pid, "Precisas de fazer login\n"),
                    lobby(Rooms);
                true ->
                    case maps:is_key(Room, Rooms) of
                        true ->
                            {CountProc, RLevel, Pids} = maps:get(Room, Rooms),
                            if (Level == RLevel orelse Level == RLevel + 1 orelse Level == RLevel - 1) andalso (CountProc == self()) -> % nivel de dificuldade e nao comecou
                                if length(Pids) < 4 -> % maximo de jogadores
                                    NRooms = maps:put(Room, {CountProc, RLevel, [Pid | Pids]} , Rooms),
                                    ?CHANGE_STATE(Pid, {new_room, Room}),
                                    ?SEND_MESSAGE(Pid, "success\n"),
                                    if (length(Pids) + 1) == 2 -> %% start countdown
                                        ?CHANGE_STATE(Pid, {countdown, Room}); 
                                    true ->
                                        ?CHANGE_STATE(Pid, {wait}) % espera para comecar o jogo
                                    end,
                                    lobby(NRooms);
                                true ->
                                    ?SEND_MESSAGE(Pid, "Sala cheia\n"),
                                    lobby(Rooms)
                                end;
                            true ->
                                ?SEND_MESSAGE(Pid, "Nivel diferente da sala\n"),
                                lobby(Rooms)
                            end;
                        false ->
                            ?SEND_MESSAGE(Pid, "Sala nao existe\n"),
                            lobby(Rooms)
                    end
                end;
            true ->
                ?SEND_MESSAGE(Pid, "Ja estas noutra sala, sai primeiro\n"),
                lobby(Rooms)
            end;
        {create_room, User, Room, Level, Pid} -> % jogador tenta criar sala
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "Precisas de fazer login\n"),
                lobby(Rooms);
            true ->
                case maps:is_key(Room, Rooms) of
                    true ->
                        ?SEND_MESSAGE(Pid, "Esta sala ja existe\n"),
                        lobby(Rooms);
                    false ->
                        ?SEND_MESSAGE(Pid, "success\n"),
                        NRooms = maps:put(Room, {self(), Level, []}, Rooms),
                        lobby(NRooms)
                end
            end;
        {list_rooms, Level, Pid} -> % lista as salas ao jogador -- resolver
            Ver = fun(Key, Value, Acc) -> 
                {CountProc, RLevel, Pids} = Value,
                if (Level == RLevel orelse Level == RLevel + 1 orelse Level == RLevel - 1) andalso (length(Pids) < 4) andalso (CountProc == self()) ->
                    [Key | Acc];
                true ->
                    Acc
                end
            end,
            RoomsList = maps:fold(Ver, [], Rooms),
            ?SEND_MUL_MESSAGE(Pid, RoomsList),
            lobby(Rooms);
        {leave, Room, Pid} -> % jogador tenta sair da sala, se estiver numa
            if Room == "main" ->
                ?SEND_MESSAGE(Pid, "Nao estas em nenhuma sala\n"),
                lobby(Rooms);
            true->
                {CountProc, Level, Pids} = maps:get(Room, Rooms),
                if length(Pids) =< 1 ->
                    NRooms = maps:remove(Room, Rooms),
                    ?SEND_MESSAGE(Pid, "success\n"),
                    ?CHANGE_STATE(Pid, {new_room, "main"}),
                    lobby(NRooms);
                true ->
                    NRooms = maps:put(Room, {CountProc, Level, lists:delete(Pid, Pids)}, Rooms),
                    ?SEND_MESSAGE(Pid, "success\n"),
                    ?CHANGE_STATE(Pid, {new_room, "main"}),
                    lobby(NRooms)
                end
            end;
        {offline, Room, Pid} -> % jogador e eliminado das salas caso saia inesperadamente
            if Room == "main" ->
                lobby(Rooms);
            true->
                {CountProc, RLevel, Pids} = maps:get(Room, Rooms),
                if CountProc == self() ->
                    self() ! {leave, Room, Pid},
                    lobby(Rooms);
                true ->
                    if length(Pids) =< 2 -> %stop countdown/ignore i
                        NRooms = maps:put(Room, {self(), RLevel, Pids}, Rooms),
                        lists:foreach(fun(PPid) -> ?CHANGE_STATE(PPid, {unexpected_leave}), ?SEND_MESSAGE(Pid, "unexpected_leave\n") end, Pids),
                        self() ! {leave, Room, Pid},
                        lobby(NRooms);
                    true ->
                        continue
                    end
                end,
                self() ! {leave, Room, Pid},
                lobby(Rooms)
            end
    end.