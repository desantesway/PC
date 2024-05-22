-module(game_player).
-include("server.hrl").
-export([start/3]).

start(GameProc, Sock, UserAuth) -> %User = {{Name, Level, Lobby, XP}, BoostLeft, {Pos, Velocity, Angle}, Buttons}
    io:format("game proc pid ~p\n", [GameProc]),
    {Name, _, _, _} = UserAuth,
    GameProc ! {new_pid, Name, self()},
    Keys = #{ % fazer telicas default aqui
        "UP" => false,
        "LEFT" => false,
        "RIGHT" => false
        },
    gamePlayer(GameProc, Sock, {UserAuth, 100, {0, 0, 0}, Keys}). 

gamePlayer(GameProc, Sock, User) ->
    receive
        {broadcast, Data} ->
            ?SEND_BROADCAST(Sock, Data),
            gamePlayer(GameProc, Sock, User);
        {broadcast_list, Data} ->
            ?SEND_BROADCAST_LIST(Sock, Data),
            gamePlayer(GameProc, Sock, User);
        {fpieces, Data} ->
            case Data of
                {died} ->
                    io:format("Died ~p\n", [self()]),
                    ?SEND_MESSAGE(Sock, "you_died\n"),
                    gamePlayer(GameProc, Sock, User);
                {unexpected_leave} ->
                    self() ! {fpieces, {end_game}},
                    gamePlayer(GameProc, Sock, User);
                {won} ->
                    {{Name, Lvl, Lobby, XP}, Boost, Pos, But} = User,
                    if XP < 1 ->
                        NXP = 1;
                    true ->
                        NXP = XP + 1
                    end,
                    if Lvl == NXP ->
                        NLvl = Lvl + 1;
                    true ->
                        NLvl = Lvl
                    end,
                    gamePlayer(GameProc, Sock, {{Name, NLvl, Lobby, NXP}, Boost, Pos, But});
                {lost} ->
                    {{Name, Lvl, Lobby, XP}, Boost, Pos, But} = User,
                    if XP > - 1 ->
                        NXP = - 1;
                    true ->
                        NXP = XP - 1
                    end,
                    if Lvl/2 == - NXP ->
                        if Lvl == 1 ->
                            NLvl = 1;
                        true ->
                            NLvl = Lvl - 1
                        end;
                    true ->
                        NLvl = Lvl
                    end,
                    gamePlayer(GameProc, Sock, {{Name, NLvl, Lobby, NXP}, Boost, Pos, But});
                {end_game} ->
                    {{Name, Level, _, XP}, _, _, _} = User,
                    accsProc ! {update_lvl, self(), Level, XP},
                    String = "lvl@@@" ++ erlang:integer_to_list(Level) ++ "@@@XP@@@" ++ erlang:integer_to_list(XP) ++ "\n",
                    io:fwrite("Sending ~p\n", [String]),
                    ?SEND_MESSAGE(self(), String),
                    server:userAuth(Sock, {Name, Level, "main", XP})
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CHAT_MESSAGE>>, Message] -> % sends a chat message
                    GameProc ! {send_message, 
                        self(), 
                        string:trim(binary_to_list(Message), trailing)},
                    gamePlayer(GameProc, Sock, User);
                [<<?LEAVE_CHAT>>, _] -> % sends a chat message
                    GameProc ! {leave_chat, self()},
                    gamePlayer(GameProc, Sock, User);
                [<<?UP_KEY>>, _] -> % sum about new button pressed (?)
                    % as if the calculations made the player die;
                    %GameProc ! {died, self()}, % !!!! ONLY FOR TESTING DEATH/XP this is supposed to be handled by the game_sim, not the player
                    gamePlayer(GameProc, Sock, User);
                [<<?RIGHT_KEY>>, _] -> % sum about new button pressed (?)
                    gamePlayer(GameProc, Sock, User);
                [<<?LEFT_KEY>>, _] -> % sum about new button pressed (?)
                    gamePlayer(GameProc, Sock, User)
            end;
        {tcp_closed, _} ->
            {{_, _, Lobby, _}, _, _, _} = User,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {interrupt_game, self()};
        {tcp_error, _, _} ->
            {{_, _, Lobby, _}, _, _, _} = User,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {interrupt_game, self()};
        Other ->
            io:fwrite("ERROR ~p ~p\n", [self(), Other]),
            gamePlayer(GameProc, Sock, User)
    end.