-module(game_sim).
-include("server.hrl").
-export([start/3]).

start(GameProc, Sock, UserAuth) -> %User = {{Name, Level, Lobby, XP}, BoostLeft, {Pos, Velocity, Angle}, Buttons}
    Keys = #{ % fazer telicas default aqui
        "A" => false
        },
    gameSim(GameProc, Sock, {UserAuth, 100, {0, 0, 0}, Keys}). 

gameSim(GameProc, Sock, User) ->
    receive
        {broadcast, Data} ->
            ?SEND_BROADCAST(Sock, Data),
            gameSim(GameProc, Sock, User);
        {broadcast_list, Data} ->
            ?SEND_BROADCAST_LIST(Sock, Data),
            gameSim(GameProc, Sock, User);
        {fpieces, Data} ->
            case Data of
                {send_pid} ->
                    GameProc ! {new_pid, self()},
                    gameSim(GameProc, Sock, User);
                {end_game} ->
                    {{Name, Level, _, XP}, _, _, _} = User,
                    server:userAuth(Sock, {Name, Level, "main", XP})
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?USERS_INFO>>, _] -> %sum about requesting user info
                    gameSim(GameProc, Sock, User);
                [<<?NEW_KEY>>, _] -> %sum about new button pressed (?)
                    gameSim(GameProc, Sock, User)
            end;
        {tcp_closed, _} ->
            {{_, _, Lobby, _}, _, _, _} = User,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {end_game, self()};
        {tcp_error, _, _} ->
            {{_, _, Lobby, _}, _, _, _} = User,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {end_game, self()};
        Other ->
            io:fwrite("ERROR ~p ~p\n", [self(), Other]),
            gameSim(GameProc, Sock, User)
    end.