-module(lobby).
-include("server.hrl").
-export[start/0].

start() -> lobby(#{}).
% falta implementar level, room => {level, Pids}, if players 2, countdown 5 seconds in another procc, start game, join room create a new process
% falta implementar list rooms
% gestor de salas do jogo
lobby(Rooms) ->
    io:format("Rooms: ~p~n", [Rooms]),
    receive
        {join, User, Lobby, Room, Level, Pid} -> % num max de jogadores, ver level % jogador tenta entrar numa sala
            if Lobby == "main" ->
                if User == "Anonymous" ->
                    ?SEND_MESSAGE(Pid, "Precisas de fazer login\n"),
                    lobby(Rooms);
                true ->
                    case maps:is_key(Room, Rooms) of
                        true ->
                            {RLevel, Pids} = maps:get(Room, Rooms),
                            if Level == RLevel orelse Level == RLevel + 1 orelse Level == RLevel - 1 -> % nivel de dificuldade
                                if length(Pids) < 4 -> % maximo de jogadores
                                    ?SEND_MESSAGE(Pid, "success\n"),
                                    NRooms = maps:put(Room, {RLevel, [Pid | Pids]} , Rooms),
                                    ?CHANGE_STATE(Pid, {new_room, Room}),
                                    ?CHANGE_STATE(Pid, {wait}), %% start game
                                    ?SEND_MESSAGE(Pid, "success\n"),
                                    io:format("Pids: ~p~n", [(length(Pids) + 1) == 2]),
                                    if (length(Pids) + 1) == 2 ->
                                        ?CHANGE_STATE(Pid, {countdown}); %% start countdown
                                    true ->
                                        ok 
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
        {create_room, Room, Level, Pid} -> % jogador tenta criar sala
            case maps:is_key(Room, Rooms) of
                true ->
                    ?SEND_MESSAGE(Pid, "Esta sala ja existe\n"),
                    lobby(Rooms);
                false ->
                    ?SEND_MESSAGE(Pid, "success\n"),
                    NRooms = maps:put(Room, {Level, []}, Rooms),
                    lobby(NRooms)
            end;
        {list_rooms, Pid} -> % lista as salas ao jogador
            ?SEND_MESSAGE(Pid, "rooms:\n"),
            lists:foreach(fun(Room) -> ?SEND_MESSAGE(Pid, Room) end, maps:keys(Rooms)),
            lobby(Rooms);
        {leave, Room, Pid} -> % jogador tenta sair da sala, se estiver numa
            if Room == "main" ->
                ?SEND_MESSAGE(Pid, "Nao estas em nenhuma sala\n"),
                lobby(Rooms);
            true->
                {Level, Pids} = maps:get(Room, Rooms),
                if length(Pids) =< 1 ->
                    NRooms = maps:remove(Room, Rooms);
                true ->
                    NRooms = maps:put(Room, {Level, lists:delete(Pid, Pids)}, Rooms)
                end,
                ?SEND_MESSAGE(Pid, "success\n"),
                ?CHANGE_STATE(Pid, {new_room, "main"}),
                ?CHANGE_STATE(Pid, {leave}), %% start game
                lobby(NRooms)
            end;
        {offline, Room, Pid} -> % jogador e eliminado das salas caso saia inesperadamente
            self() ! {leave, Room, Pid},
            lobby(Rooms)
    end.