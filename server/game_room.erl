-module(game_room).
-include("server.hrl").
-export([start/1]).

start(Name) -> gameRoom(Name, #{}).

gameRoom(Name, Pids) -> %pid => alive?
    receive
        {get_info, Pid} -> % get the info in game from all the pid, not fully done
            Pid ! {info, Pids},
            gameRoom(Name, Pids);
        {new_pid, Pid} -> % add a new pid to the game
            gameRoom(Name, maps:put(Pid, true, Pids));
        {end_game, RIP} -> % ends the game removing all pids
            NewPids = maps:remove(RIP, Pids),
            lists:foreach(fun(Pid) -> 
                ?SEND_MESSAGE(Pid, "end_game\n"),
                ?CHANGE_STATE(Pid, {end_game}),
                lobbyProc ! {leave, Name, Pid}
            end, maps:keys(NewPids))
    end.


%gameState([]) -> ok.
%gameState([FirstPlayer | Rest]) ->
%    FirstPlayer ! {positions},
%    gameState(Rest);
