-module(accounts).
-include("server.hrl").
-export([start/2]).

start(Socket, Accs) -> accounts(Socket, Accs).

accounts(Socket, Accs) ->
    receive
        {shutdown, Pid} -> 
            Pid ! {full_save, Accs};
        {online, Pid} ->
            NewPids = maps:put(Pid, {"Anonymous", "main"}, Socket),
            %?SEND_MESSAGE(Pid, "Bem vindo\n"),
            accounts(NewPids, Accs);
        {create_account, Username, Password, Pid, Off} ->
            case maps:is_key(Username, Accs) of
                true ->
                    ?SEND_MESSAGE(Pid, "Nome ja existe\n"),
                    accounts(Socket, Accs);
                false ->
                    NAccs = maps:put(Username, Password, Accs),
                    {_, Lobby} = maps:get(Pid, Socket),
                    NewPids = maps:put(Pid, {Username, Lobby}, Socket),
                    Off ! {full_save, NAccs},
                    ?SEND_MESSAGE(Pid, "success\n"),
                    accounts(NewPids, NAccs)
            end;
        {login, Username, Password, Pid} ->
            case maps:is_key(Username, Accs) of
                true ->
                    RealPass = maps:get(Username, Accs),
                    if RealPass == Password ->
                        {_, Lobby} = maps:get(Pid, Socket),
                        NewPids = maps:put(Pid, {Username, Lobby}, Socket),
                        ?CHANGE_STATE(Pid, {new_name, Username}),
                        ?SEND_MESSAGE(Pid, "success\n"),
                        accounts(NewPids, Accs);
                    true ->
                        ?SEND_MESSAGE(Pid, "Password incorreta\n"),
                        accounts(Socket, Accs)
                    end;
                false ->
                    ?SEND_MESSAGE(Pid, "Username nao existe\n"),
                    accounts(Socket, Accs)
            end;
        {logout, Pid} ->
            {User, _} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "Logout ja feito\n"),
                accounts(Socket, Accs);
            true->
                NewPids = maps:put(Pid, {"Anonymous", "main"}, Socket),
                ?CHANGE_STATE(Pid, {new_room, "main"}),
                ?CHANGE_STATE(Pid, {new_name, "Anonymous"}),
                ?SEND_MESSAGE(Pid, "Logout feito\n"),
                accounts(NewPids, Accs)
            end;
        {change_name, Name, Pid, Off} -> 
            {User, Room} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "Precisas de fazer login\n"),
                accounts(Socket, Accs);
            true ->
                case maps:is_key(Name, Accs) of
                    true ->
                        ?SEND_MESSAGE(Pid, "Nome ja existe\n"),
                        accounts(Socket, Accs);
                    false ->
                        ?CHANGE_STATE(Pid, {new_name, Name}),
                        NSocket = maps:put(Pid, {Name, Room}, Socket),
                        Pass = maps:get(User, Accs),
                        NAccs = maps:put(Name, Pass, maps:remove(User, Accs)),
                        Off ! {full_save, NAccs},
                        ?SEND_MESSAGE(Pid, "Nome Mudado\n"),
                        accounts(NSocket, NAccs)
                end
            end;
        {change_pass, Pass, Pid, Off} -> 
            {User, _} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "Precisas de fazer login\n"),
                accounts(Socket, Accs);
            true ->
                NAccs = maps:put(User, Pass, Accs),
                Off ! {full_save, NAccs},
                ?SEND_MESSAGE(Pid, "Pass Mudada\n"),
                accounts(Socket, NAccs)
            end;
        {remove_account, Pid, Off} ->
            {User, _} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "Precisas de fazer login\n"),
                accounts(Socket, Accs);
            true ->
                NAccs = maps:remove(User, Accs),
                NSocket = maps:put(Pid, {"Anonymous", "main"}, Socket),
                Off ! {full_save, NAccs},
                ?CHANGE_STATE(Pid, {new_name, "Anonymous"}),
                ?CHANGE_STATE(Pid, {new_room, "main"}),
                ?SEND_MESSAGE(Pid, "Conta Removida\n"),
                accounts(NSocket, NAccs)
            end;
        {offline, Pid} ->
            {User, Room} = maps:get(Pid, Socket),
            NewPids = maps:remove(Pid, Socket),
            io:format("~p left ~p game ~n", [User, Room]),
            accounts(NewPids, Accs)
    end.