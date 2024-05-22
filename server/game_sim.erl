-module(game_sim).
-include("server.hrl").
-export([start/1]).

start(Name) -> 
    % cria um ticker
    GameProc = self(),
    Chat = spawn(fun() -> gameChat(GameProc, maps:new()) end),
    spawn(fun() -> gameTick(GameProc) end),
    gameSim(Chat, Name, #{}, false).

gameTick(GameProc) ->
    %do sum
    timer:sleep(1000),
    gameTick(GameProc).

gameChat(GameProc, OnChat) ->
    receive
        {new_message, Pid, Message} -> % sends message to all players
            ToSend = "!-CHAT-!@@@" ++ maps:get(Pid, OnChat) ++ "@@@sent:@@@" ++ Message ++ "\n",
            lists:foreach(fun(Key) -> ?SEND_MESSAGE(Key, ToSend) end, maps:keys(OnChat)),
            gameChat(GameProc, OnChat);
        {pid_left, Pid} -> % removes a pid from the chat
            Username = maps:get(Pid, OnChat),
            ToSend = "!-CHAT-!@@@" ++ Username ++ "@@@left_chat\n",
            NewOnChat = maps:remove(Pid, OnChat),
            lists:foreach(fun(Key) -> ?SEND_MESSAGE(Key, ToSend) end, NewOnChat),
            gameChat(GameProc, NewOnChat);
        {new_pid, Pid, Username} -> % adds a new pid to the chat
            gameChat(GameProc, maps:put(Pid, Username, OnChat))
    end.

afterGame(Chat, Name, Pids) ->
    receive
        {send_message, Pid, Message} -> % sends message to all players
            Chat ! {new_message, Pid, Message},
            afterGame(Chat, Name, Pids);
        {leave_chat, Pid} ->
            Chat ! {pid_left, Pid},
            ?SEND_MESSAGE(Pid, "end_game\n"),
            ?CHANGE_STATE(Pid, {end_game}),
            lobbyProc ! {leave, Name, Pid}
    end.

gameSim(Chat, Name, Pids, Countdown) -> %pid => {alive?, username} IMPLEMENTAR COUNTDOWN
    io:format("Game ~p\n", [Pids]),
    receive
        {ticket_request} -> % request and sends info of the current to all players
            gameSim(Chat, Name, Pids, Countdown);
        {countdown_started} ->
            lists:foreach(fun(Key) -> % sends to all players that last player countdown started
                ?SEND_MESSAGE(Key, "game_countdown_started\n")
            end, maps:keys(Pids)),
            gameSim(Chat, Name, Pids, true);
        {countdown_ended} ->
            lists:foreach(fun(Key) -> % sends to all players that last player countdown ended
                ?SEND_MESSAGE(Key, "game_countdown_ended\n")
            end, maps:keys(Pids)),
            gameSim(Chat, Name, Pids, false);
        {new_pid, Username, Pid} -> % add a new pid to the game
            gameSim(Chat, Name, maps:put(Pid, {true, Username}, Pids), Countdown);
        {send_message, Pid, Message} -> % sends message to all players
            Chat ! {new_message, Pid, Message},
            gameSim(Chat, Name, Pids, Countdown);
        {died, Pid} ->
            {_, Username} = maps:get(Pid, Pids),
            Chat ! {new_pid, Pid, Username},
            Chat ! {new_message, Pid, Username ++ " died\n"},
            NewAlives = lists:foldl( % gets the alive pids and 
                fun(Key, AccAlives) ->
                    case maps:get(Key, Pids) of
                        {Alive, _} when Alive == true ->
                            [Key | AccAlives];
                        _ ->
                            AccAlives
                    end
                end, [], maps:keys(Pids)),
            {_, Username} = maps:get(Pid, Pids),
            lists:foreach(fun(Key) -> % sends to all players the pid that died
                String = erlang:pid_to_list(Pid) ++ "@@@died\n",
                ?SEND_MESSAGE(Key, String)
            end, maps:keys(Pids)),
            NewPids = maps:put(Pid, {false, Username}, Pids),
            case length(NewAlives) == 2 of
                true -> % start countdown on another proccess
                    GameProc = self(),
                    spawn(fun() -> countdown(GameProc) end),
                    gameSim(Chat, Name, NewPids, true);
                false ->
                    case length(NewAlives) == 1 of
                        true ->
                            case NewAlives of
                                [LastAlive] -> % last alive wins
                                    self() ! {start_end_game, LastAlive},
                                    gameSim(Chat, Name, NewPids, Countdown)
                            end;
                        false ->
                            gameSim(Chat, Name, NewPids, Countdown)
                    end
            end;
        {start_end_game, LastAlive} -> 
            if Countdown -> % countdown still active - todos perdem, send lost to all por causa do xp para por a 0
                lists:foreach(fun(Pid) -> 
                    ?SEND_MESSAGE(Pid, "lost_game\n"),
                    ?CHANGE_STATE(Pid, {lost})
                end, maps:keys(Pids)),
                afterGame(Chat, Name, Pids);
            true -> % todos menos lastalive perdem
                lists:foreach(fun(Pid) -> 
                    if Pid == LastAlive -> 
                        ?CHANGE_STATE(Pid, {won}),
                        ?SEND_MESSAGE(Pid, "won_game\n");
                    true ->
                        ?CHANGE_STATE(Pid, {lost}),
                        ?SEND_MESSAGE(Pid, "lost_game\n")
                    end
                end, maps:keys(Pids)),
                self() ! {end_game},
                afterGame(Chat, Name, Pids)
            end;
        {interrupt_game, RIP} -> % ends the game removing all pids
            NewPids = maps:remove(RIP, Pids),
            lists:foreach(fun(Pid) -> 
                ?SEND_MESSAGE(Pid, "interrupt_game\n"),
                lobbyProc ! {offline, Name, Pid}
            end, maps:keys(NewPids));
        Data ->
            io:format("Unexpected data ~p\n", [Data]),
            gameSim(Chat, Name, Pids, Countdown)
    end.

countdown(GameProc)-> 
    io:format("Countdown started ~p\n", [GameProc]),
    GameProc !  {countdown_started},
    timer:sleep(5000),
    io:format("Countdown ended\n"),
    GameProc !  {countdown_ended}.