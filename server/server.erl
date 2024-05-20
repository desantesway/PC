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
            register(accsProc, spawn(fun()->accounts:management(maps:new(), Data) end)),
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
            userAuth(Socket, "main", "Anonymous");
        {error, closed} ->
            io:fwrite("Closed socket");
        {error, timeout} ->
            io:fwrite("Timeout");
        {error, _} ->
            io:fwrite("Error listening to socket")
    end.

userAuth(Sock, Lobby, User) ->
    receive
        {broadcast, Data} ->
            gen_tcp:send(Sock, Data),
            gen_tcp:send(Sock, "!-SVDONE-!\n"),
            userAuth(Sock, Lobby, User);
        {accounts, Data} ->
            case Data of
                {new_name, Name} ->
                    userAuth(Sock, Lobby, Name);
                {new_room, Room} ->
                    userAuth(Sock, Room, User);
                {start} ->
                    userAuth(Sock, Lobby, User) % send this pid to play and wait for the game, this line is only to compile
            end;
        {tcp, _, Data} -> % falta criacao de salas e checkar se existe
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CREATE_ACCOUNT>>, UserName, Password] ->
                    accsProc ! {create_account, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self(), offProc},
                    userAuth(Sock, Lobby, User);
                [<<?LOGIN_ACCOUNT>>, UserName, Password] ->
                    accsProc ! {login, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self()},
                    userAuth(Sock, Lobby, UserName);
                [<<?LOGOUT_ACCOUNT>>, _] ->
                    accsProc ! {logout, self()},
                    userAuth(Sock, Lobby, User);
                [<<?JOIN_ROOM>>, Room] ->
                    accsProc ! {join, Room, self()},
                    userAuth(Sock, Lobby, User);
                [<<?LEAVE_ROOM>>, _] ->
                    accsProc ! {leave, self()},
                    userAuth(Sock, Lobby, User);
                [<<?CHANGE_NAME>>, Name] ->
                    accsProc ! {change_name, 
                        string:trim(binary_to_list(Name), trailing), 
                    self(), offProc},
                    userAuth(Sock, Lobby, User);
                [<<?CHANGE_PASS>>, Pass] ->
                    accsProc ! {change_pass, 
                        string:trim(binary_to_list(Pass), trailing), 
                        self(), offProc},
                    userAuth(Sock, Lobby, User);
                [<<?REMOVE_ACCOUNT>>, _] ->
                    accsProc ! {remove_account, self(), offProc},
                    userAuth(Sock, Lobby, User);
                _ ->
                    gen_tcp:send(Sock, "Error: Incorrect syntax.\n"),
                    gen_tcp:send(Sock, "!-SVDONE-!\n"),
                    userAuth(Sock, Lobby, User)
            end;
        {tcp_closed, _} ->
            accsProc ! {offline, self()};
        {tcp_error, _, _} ->
            accsProc ! {offline, self()};
        _ ->
            io:fwrite("ERROR\n"),
            userAuth(Sock, Lobby, User)
    end.


