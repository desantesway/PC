-module(lobby).
-export[start/0].

start() -> lobby(#{}).

lobby(Rooms) -> %% sala -> pid, sala elimina se caso nao haja jogadores dentro
    receive
        {join, User, Lobby, Room, Pid} -> % num max de jogadores e nao duplicados (caso tenha fechado o jogo)
            if Lobby == "main" ->
                if User == "Anonymous" ->
                    send_message(Pid, "Precisas de fazer login\n"),
                    lobby(Rooms);
                true ->
                    case maps:is_key(Room, Rooms) of
                        true ->
                            Pids = maps:get(Room, Rooms),
                            NRooms = maps:put(Room, Pids, Rooms),
                            change_state(Pid, {new_room, Room}),
                            change_state(Pid, {wait}), %% start game
                            send_message(Pid, "Entraste na sala\n"),
                            lobby(NRooms);
                        false ->
                            send_message(Pid, "Sala nao existe\n"),
                            lobby(Rooms)
                    end
                end;
            true ->
                send_message(Pid, "Ja estas noutra sala, sai primeiro\n"),
                lobby(Rooms)
            end;
        {create_room, Room, Pid} ->
            case maps:is_key(Room, Rooms) of
                true ->
                    send_message(Pid, "Esta sala ja existe\n"),
                    lobby(Rooms);
                false ->
                    send_message(Pid, "Sala criada\n"),
                    NRooms = maps:put(Room, [], Rooms),
                    lobby(NRooms)
            end;
        {browse_rooms, Pid} ->
            send_message(Pid, "rooms:\n"),
            lists:foreach(fun(Room) -> send_message(Pid, Room) end, maps:keys(Rooms)),
            lobby(Rooms);
        {leave, Room, Pid} ->
            if Room == "main" ->
                send_message(Pid, "Nao estas em nenhuma sala\n"),
                lobby(Rooms);
            true->
                Pids = maps:get(Room, Rooms),
                if length(Pids) =< 1 ->
                    NRooms = maps:remove(Room, Rooms);
                true ->
                    NRooms = maps:put(Room, Pids:delete(Pid), Rooms)
                end,
                change_state(Pid, {leave}), %% start game
                change_state(Pid, {new_room, "main"}),
                send_message(Pid, "Sai da sala\n"),
                lobby(NRooms)
            end
    end.

change_state(Pid, Data) -> Pid ! {fpieces, Data}.

send_message(Socket, Message) ->
    Socket ! {broadcast, Message}.