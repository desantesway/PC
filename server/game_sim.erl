-module(game_sim).
-include("server.hrl").
-export([start/3]).

start(GameProc, Sock, UserAuth) -> % CAHNGE THIS - User = {{Name, Level, XP}, BoostLeft, {Pos, Velocity, Angle}, Buttons}, fazer telicas default aqui
    gameSim(GameProc, Sock, {UserAuth, 100, {0, 0, 0}, #{}}). 

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
                    {{Name, Level, XP}, _, _, _} = User,
                    server:userAuth(Sock, {Name, Level, "main", XP})
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?USERS_INFO>>, _] -> %sum about requesting user info
                    gameSim(GameProc, Sock, User);
                [<<?NEW_KEY>>, _] -> %sum about new button pressed (?)
                    gameSim(GameProc, Sock, User)
            end;
        {tcp_closed, _} -> %%remove from game and if game < 2 players, end game
            %{_, _, Lobby} = User,
            accsProc ! {offline, self()},
            GameProc ! {end_game, self()};
            %lobbyProc ! {offline, Lobby, self()};
        {tcp_error, _, _} ->
            %{_, _, Lobby} = User,
            accsProc ! {offline, self()},
            GameProc ! {end_game, self()};
            %lobbyProc ! {offline, Lobby, self()};
        Other ->
            io:fwrite("ERROR ~p ~p\n", [self(), Other]),
            gameSim(GameProc, Sock, User)
    end.