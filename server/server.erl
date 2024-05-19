-module(server).
-include("server.hrl").
-export([start/0, server/1, stop/1]).

start() -> server(12345).

stop(Server) -> Server ! stop.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    case Result of
        {ok, LSock} ->
            Accs = spawn(fun()->accounts:management(maps:new(), #{"Anonymous" => "admin"}) end),
            spawn(fun() -> acceptor(LSock, Accs) end),
            receive
                stop -> ok
            end;
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.

acceptor(LSock, Accs) ->
    case gen_tcp:accept(LSock) of 
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock, Accs) end),
            Accs ! {online, self()},
            userAuth(Socket, Accs, "main", "Anonymous");
        {error, closed} ->
            io:fwrite("Closed socket");
        {error, timeout} ->
            io:fwrite("Timeout");
        {error, _} ->
            io:fwrite("Error listening to socket")
    end.

userAuth(Sock, Accs, Lobby, User) ->
    receive
        {broadcast, Data} ->
            gen_tcp:send(Sock, Data),
            gen_tcp:send(Sock, "!-SVDONE-!\n"),
            userAuth(Sock, Accs, Lobby, User);
        {accounts, Data} ->
            case Data of
                {new_name, Name} ->
                    userAuth(Sock, Accs, Lobby, Name);
                {new_room, Room} ->
                    userAuth(Sock, Accs, Room, User);
                {start} ->
                    userAuth(Sock, Accs, Lobby, User) % send this pid to play the game, this line is only to compile
            end;
        {tcp, _, Data} -> % falta mudar de nome, pass, remover conta, criacao de salas e checkar se existe
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CREATE_ACCOUNT>>, UserName, Password] ->
                    Accs ! {create_account, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self()},
                    userAuth(Sock, Accs, Lobby, User);
                [<<?LOGIN_ACCOUNT>>, UserName, Password] ->
                    Accs ! {login, 
                    string:trim(binary_to_list(UserName), trailing), 
                    string:trim(binary_to_list(Password), trailing), 
                    self()},
                    userAuth(Sock, Accs, Lobby, UserName);
                [<<?LOGOUT_ACCOUNT>>, _] ->
                    Accs ! {logout, self()},
                    userAuth(Sock, Accs, Lobby, "Anonymous");
                [<<?JOIN_ROOM>>, Room] ->
                    Accs ! {join, Room, self()},
                    userAuth(Sock, Accs, Lobby, User);
                [<<?LEAVE_ROOM>>, _] ->
                    Accs ! {leave, self()},
                    userAuth(Sock, Accs, "main", User);
                _ ->
                    gen_tcp:send(Sock, "Error: Incorrect syntax.\n"),
                    gen_tcp:send(Sock, "!-SVDONE-!\n"),
                    userAuth(Sock, Accs, Lobby, User)
            end;
        {tcp_closed, _} ->
            Accs ! {offline, self()};
        {tcp_error, _, _} ->
            Accs ! {offline, self()};
        _ ->
            io:fwrite("ERROR\n"),
            userAuth(Sock, Accs, Lobby, User)
    end.


