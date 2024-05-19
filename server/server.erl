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
            OfflineProc = spawn(fun() -> offline:start() end),
            io:fwrite("~p\n", [OfflineProc]),
            Pid = spawn(fun() -> init(LSock, OfflineProc, ok) end),
            OfflineProc ! {load, Pid},
            Pid;
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.

init(Sock, OfflineProc, AProc)->
    receive
        {loaded, Data} ->
            io:fwrite("Loaded~p\n", [Data]),
            AccsProc = spawn(fun()->accounts:management(maps:new(), Data) end),
            spawn(fun() -> acceptor(Sock, OfflineProc, AccsProc) end),
            init(Sock, OfflineProc, AccsProc);
        {error, Reason} ->
            io:fwrite("Error loading accounts~p\n", [Reason]),
            init(Sock, OfflineProc, AProc);
        stop -> 
            gen_tcp:close(Sock),
            AProc ! {shutdown, OfflineProc},
            OfflineProc ! off
    end.

acceptor(LSock, OfflineProc, AccsProc) ->
    case gen_tcp:accept(LSock) of 
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock, OfflineProc, AccsProc) end),
            AccsProc ! {online, self()},
            userAuth(Socket, OfflineProc, AccsProc, "main", "Anonymous");
        {error, closed} ->
            io:fwrite("Closed socket");
        {error, timeout} ->
            io:fwrite("Timeout");
        {error, _} ->
            io:fwrite("Error listening to socket")
    end.

userAuth(Sock, OfflineProc, AccsProc, Lobby, User) ->
    receive
        {broadcast, Data} ->
            gen_tcp:send(Sock, Data),
            gen_tcp:send(Sock, "!-SVDONE-!\n"),
            userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
        {accounts, Data} ->
            case Data of
                {new_name, Name} ->
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, Name);
                {new_room, Room} ->
                    userAuth(Sock, OfflineProc, AccsProc, Room, User);
                {start} ->
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User) % send this pid to play and wait for the game, this line is only to compile
            end;
        {tcp, _, Data} -> % falta criacao de salas e checkar se existe
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CREATE_ACCOUNT>>, UserName, Password] ->
                    AccsProc ! {create_account, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self(), OfflineProc},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                [<<?LOGIN_ACCOUNT>>, UserName, Password] ->
                    AccsProc ! {login, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self()},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, UserName);
                [<<?LOGOUT_ACCOUNT>>, _] ->
                    AccsProc ! {logout, self()},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                [<<?JOIN_ROOM>>, Room] ->
                    AccsProc ! {join, Room, self()},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                [<<?LEAVE_ROOM>>, _] ->
                    AccsProc ! {leave, self()},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                [<<?CHANGE_NAME>>, Name] ->
                    AccsProc ! {change_name, 
                        string:trim(binary_to_list(Name), trailing), 
                    self(), OfflineProc},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                [<<?CHANGE_PASS>>, Pass] ->
                    AccsProc ! {change_pass, 
                        string:trim(binary_to_list(Pass), trailing), 
                        self(), OfflineProc},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                [<<?REMOVE_ACCOUNT>>, _] ->
                    AccsProc ! {remove_account, self(), OfflineProc},
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User);
                _ ->
                    gen_tcp:send(Sock, "Error: Incorrect syntax.\n"),
                    gen_tcp:send(Sock, "!-SVDONE-!\n"),
                    userAuth(Sock, OfflineProc, AccsProc, Lobby, User)
            end;
        {tcp_closed, _} ->
            AccsProc ! {offline, self()};
        {tcp_error, _, _} ->
            AccsProc ! {offline, self()};
        _ ->
            io:fwrite("ERROR\n"),
            userAuth(Sock, OfflineProc, AccsProc, Lobby, User)
    end.


