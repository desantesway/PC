-module(accounts).
-include("server.hrl").
-export([management/2]).

management(Socket, Accs) ->
    receive
        {shutdown, Pid} -> 
            Pid ! {full_save, Accs};
        {online, Pid} ->
            NewPids = maps:put(Pid, {"Anonymous", "main"}, Socket),
            send_message(Pid, "Bem vindo\n"),
            management(NewPids, Accs);
        {create_account, Username, Password, Pid, Off} ->
            case maps:is_key(Username, Accs) of
                true ->
                    send_message(Pid, "Nome ja existe\n"),
                    management(Socket, Accs);
                false ->
                    NAccs = maps:put(Username, Password, Accs),
                    {_, Lobby} = maps:get(Pid, Socket),
                    NewPids = maps:put(Pid, {Username, Lobby}, Socket),
                    Off ! {full_save, NAccs},
                    send_message(Pid, "Conta criada\n"),
                    management(NewPids, NAccs)
            end;
        {login, Username, Password, Pid} ->
            case maps:is_key(Username, Accs) of
                true ->
                    RealPass = maps:get(Username, Accs),
                    if RealPass == Password ->
                        {_, Lobby} = maps:get(Pid, Socket),
                        NewPids = maps:put(Pid, {Username, Lobby}, Socket),
                        change_state(Pid, {new_name, Username}),
                        send_message(Pid, "Login feito\n"),
                        management(NewPids, Accs);
                    true ->
                        send_message(Pid, "Password incorreta\n"),
                        management(Socket, Accs)
                    end;
                false ->
                    send_message(Pid, "Username nao existe\n"),
                    management(Socket, Accs)
            end;
        {join, Room, Pid} ->
            {User, Lobby} = maps:get(Pid, Socket),
            if Lobby == "main" ->
                if User == "Anonymous" ->
                    send_message(Pid, "Precisas de fazer login\n"),
                    management(Socket, Accs);
                true ->
                    NewPids = maps:put(Pid, {User, Room}, Socket),
                    change_state(Pid, {new_room, Room}),
                    send_message(Pid, "Entraste na sala\n"),
                    management(NewPids, Accs)
                end;
            true ->
                send_message(Pid, "Ja estas noutra sala, sai primeiro\n"),
                management(Socket, Accs)
            end;
        {logout, Pid} ->
            {User, _} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                send_message(Pid, "Logout ja feito\n"),
                management(Socket, Accs);
            true->
                NewPids = maps:put(Pid, {"Anonymous", "main"}, Socket),
                change_state(Pid, {new_room, "main"}),
                change_state(Pid, {new_name, "Anonymous"}),
                send_message(Pid, "Logout feito\n"),
                management(NewPids, Accs)
            end;
        {leave, Pid} ->
            {User, Lobby} = maps:get(Pid, Socket),
            if Lobby == "main" ->
                send_message(Pid, "Nao estas em nenhuma sala\n"),
                management(Socket, Accs);
            true->
                NewPids = maps:put(Pid, {User, "main"}, Socket),
                change_state(Pid, {new_room, "main"}),
                send_message(Pid, "Sai da sala\n"),
                management(NewPids, Accs)
            end;
        {change_name, Name, Pid, Off} -> 
            {User, Room} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                send_message(Pid, "Precisas de fazer login\n"),
                management(Socket, Accs);
            true ->
                case maps:is_key(Name, Accs) of
                    true ->
                        send_message(Pid, "Nome ja existe\n"),
                        management(Socket, Accs);
                    false ->
                        change_state(Pid, {new_name, Name}),
                        NSocket = maps:put(Pid, {Name, Room}, Socket),
                        Pass = maps:get(User, Accs),
                        NAccs = maps:put(Name, Pass, maps:remove(User, Accs)),
                        Off ! {full_save, NAccs},
                        send_message(Pid, "Nome Mudado\n"),
                        management(NSocket, NAccs)
                end
            end;
        {change_pass, Pass, Pid, Off} -> 
            {User, _} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                send_message(Pid, "Precisas de fazer login\n"),
                management(Socket, Accs);
            true ->
                NAccs = maps:put(User, Pass, Accs),
                Off ! {full_save, NAccs},
                send_message(Pid, "Pass Mudada\n"),
                management(Socket, NAccs)
            end;
        {remove_account, Pid, Off} ->
            {User, _} = maps:get(Pid, Socket),
            if User == "Anonymous" ->
                send_message(Pid, "Precisas de fazer login\n"),
                management(Socket, Accs);
            true ->
                NAccs = maps:remove(User, Accs),
                NSocket = maps:put(Pid, {"Anonymous", "main"}, Socket),
                Off ! {full_save, NAccs},
                change_state(Pid, {new_name, "Anonymous"}),
                change_state(Pid, {new_room, "main"}),
                send_message(Pid, "Conta Removida\n"),
                management(NSocket, NAccs)
            end;
        {offline, Pid} ->
            {User, Room} = maps:get(Pid, Socket),
            NewPids = maps:remove(Pid, Socket),
            io:format("~p left ~p game ~n", [User, Room]),
            management(NewPids, Accs)
    end.

change_state(Pid, Data) -> Pid ! {accounts, Data}.

send_message(Socket, Message) ->
    Socket ! {broadcast, Message}.

    
    
    


