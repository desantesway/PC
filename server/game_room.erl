-module(game_room).
-include("server.hrl").
-export([start/0]).

start() -> gameRoom(#{}).

gameRoom(Pids) -> %add pids, pid => alive?
    receive
        {get_info, Pid} -> % get the info in game from all the pid, not fully done
            Pid ! {info, Pids},
            gameRoom(Pids);
        {new_pid, Pid} -> % add a new pid to the game
            gameRoom(maps:put(Pid, true, Pids));
        {end_game, RIP} -> % remove a pid from the game
            NewPids = maps:remove(RIP, Pids),
            lists:foreach(fun(Pid) -> 
                ?SEND_MESSAGE(Pid, "end_game\n"),
                ?CHANGE_STATE(Pid, {end_game})
            end, maps:keys(NewPids))
    end.


%gameState([]) -> ok.
%gameState([FirstPlayer | Rest]) ->
%    FirstPlayer ! {positions},
%    gameState(Rest);
