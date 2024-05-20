-module(server).
-include("server.hrl").
-export([start/0, server/1, stop/1]).

start() -> server(12345).

stop(Server) -> 
    Server ! stop,
    shutdown.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    case Result of
        {ok, LSock} ->
            register(offProc, spawn(fun() -> offline:start() end)),
            Pid = spawn(fun() -> init(LSock) end),
            offProc ! {load, Pid},
            Pid;
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.

init(Sock)->
    receive
        {loaded, Data} ->
            io:fwrite("Loaded~p\n", [Data]),
            register(accsProc, spawn(fun()->accounts:start(maps:new(), Data) end)),
            register(lobbyProc, spawn(fun()->lobby:start() end)),
            spawn(fun() -> acceptor(Sock) end),
            init(Sock);
        {error, Reason} ->
            io:fwrite("Error loading accounts~p\n", [Reason]),
            init(Sock);
        stop ->
            gen_tcp:close(Sock),
            accsProc ! {shutdown, offProc},
            offProc ! off
    end.

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of 
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock) end),
            accsProc ! {online, self()},
            userAuth(Socket, {"Anonymous", 0, "main"});
        {error, closed} ->
            io:fwrite("Closed socket");
        {error, timeout} ->
            io:fwrite("Timeout");
        {error, _} ->
            io:fwrite("Error listening to socket")
        end.

    userAuth(Sock, User) -> % add level
        receive
        {broadcast, Data} ->
            gen_tcp:send(Sock, Data),
            gen_tcp:send(Sock, "!-SVDONE-!\n"),
            userAuth(Sock, User);
        {broadcast_list, Data} ->
            lists:foreach(fun(Message) ->
            io:fwrite("Message: ~p\n", [Message]),
            gen_tcp:send(Sock, Message ++ "\n")
            end, Data),
            gen_tcp:send(Sock, "!-SVDONE-!\n"),
            userAuth(Sock, User);
        {fpieces, Data} ->
            case Data of
                {new_name, Name} ->
                    {_, Level, Lobby} = User,
                    userAuth(Sock, {Name, Level, Lobby});
                {new_room, Room} ->
                    {UserN, Level, _} = User,
                    userAuth(Sock, {UserN, Level, Room});
                {start} ->
                    userAuth(Sock, User); % send this pid to play and wait for the game, this line is only to compile
                {leave} ->
                    userAuth(Sock, User); % leave room while waiting game, this line is only to compile
                {countdown} ->
                    userAuth(Sock, User); % starts a process that counts 5 sec, for x room, when done sends to lobby, where notifies all players in x room, this line is only to compile
                {wait} ->
                    userAuth(Sock, User) % start waiting game, this line is only to compile
            end;
        {tcp, _, Data} -> % falta criacao de salas e checkar se existe
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CREATE_ACCOUNT>>, UserName, Password] ->
                    accsProc ! {create_account, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self(), offProc},
                    userAuth(Sock, User);
                [<<?LOGIN_ACCOUNT>>, UserName, Password] ->
                    accsProc ! {login, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self()},
                    userAuth(Sock, User);
                [<<?LOGOUT_ACCOUNT>>, _] ->
                    accsProc ! {logout, self()},
                    userAuth(Sock, User);
                [<<?JOIN_ROOM>>, Room] ->
                    {UserN, Level, Lobby} = User,
                    lobbyProc ! {join, 
                        UserN, 
                        Lobby, 
                        string:trim(binary_to_list(Room), trailing), 
                        Level,
                        self()},
                    userAuth(Sock, User);
                [<<?LEAVE_ROOM>>, _] ->
                    {_, _, Lobby} = User,
                    lobbyProc ! {leave, Lobby, self()},
                    userAuth(Sock, User);
                [<<?CHANGE_NAME>>, Name] ->
                    accsProc ! {change_name, 
                        string:trim(binary_to_list(Name), trailing), 
                    self(), offProc},
                    userAuth(Sock, User);
                [<<?CHANGE_PASS>>, Pass] ->
                    accsProc ! {change_pass, 
                        string:trim(binary_to_list(Pass), trailing), 
                        self(), offProc},
                    userAuth(Sock, User);
                [<<?REMOVE_ACCOUNT>>, _] ->
                    accsProc ! {remove_account, self(), offProc},
                    userAuth(Sock, User);
                [<<?CREATE_ROOM>>, Room] ->
                    {_, Level, _} = User,
                    lobbyProc ! {create_room, 
                        string:trim(binary_to_list(Room), trailing), 
                        Level,
                        self()},
                    userAuth(Sock, User);
                [<<?LIST_ROOMS>>, _] ->
                    {_, Level, _} = User,
                    lobbyProc ! {list_rooms, Level, self()},
                    userAuth(Sock, User);
                _ ->
                    gen_tcp:send(Sock, "Error: Incorrect syntax.\n"),
                    gen_tcp:send(Sock, "!-SVDONE-!\n"),
                    userAuth(Sock, User)
            end;
        {tcp_closed, _} -> %%remove lobby
            {_, _, Lobby} = User,
            accsProc ! {offline, self()},
            lobbyProc ! {offline, Lobby, self()};
        {tcp_error, _, _} ->
            {_, _, Lobby} = User,
            accsProc ! {offline, self()},
            lobbyProc ! {offline, Lobby, self()};
        _ ->
            io:fwrite("ERROR\n"),
            userAuth(Sock, User)
    end.


