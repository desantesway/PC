-module(server).
-export([start/1, server/1, stop/1]).


start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.


server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            acceptor(LSock);
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.


acceptor(LSock) ->
    case gen_tcp:accept(LSock) of 
        {ok, _} ->
            spawn(fun() -> acceptor(LSock) end),
            accounts_manager:start();
        {error, closed} ->
            io:fwrite("Closed socket");
        {error, timeout} ->
            io:fwrite("Timeout");
        {error, _} ->
            io:fwrite("Error listening to socket")
    end.

request({Request, User, Pwd}) -> 
    accounts_manager ! {Request, self(), User, Pwd},
    receive
        {Result, accounts_manager} ->
            Result
    end.


userAuth(Sock) ->
    receive
        % Receive message from client
        {tcp, _, Data} ->
            % Remove newlines and carriage returns from the data
            CleanData = re:replace(Data, "\\n|\\r", "", [global, {return, list}]),
            % Split the cleandata into a list of information
            Info = string:split(CleanData, ",", all),
            % Pattern match on the split information to handle different cases
            case Info of
                ["create_account", User, Pwd] ->
                    % Handle the create account request
                    Result = request({create_account, User, Pwd}),
                    case Result of
                        success ->
                            gen_tcp:send(Sock, "Account created\n")
                    end,
                    userAuth(Sock); % Continue receiving messages
                ["login", User, Pwd] ->
                    % Handle the login request
                    Result = request({login, User, Pwd}),
                    case Result of 
                        success -> 
                            gen_tcp:send(Sock, "Login successful\n")
                    end,                    
                    userAuth(Sock); % Continue receiving messages
                ["logout", User, Pwd] ->
                    % Handle the logout request
                    Result = request({logout, User, Pwd}),
                    case Result of
                        success ->
                            gen_tcp:send(Sock, "Logout successful\n")
                    end;
                _ ->
                    % Handle unknown commands
                    gen_tcp:send(Sock, {error, "Unknown command"}),
                    userAuth(Sock) % Continue receiving messages
            end;
        {tcp_closed, _} ->
            % Handle TCP connection closed
            ok;
        {tcp_error, _, Reason} ->
            % Handle TCP connection error
            io:format("TCP error: ~p~n", [Reason]),
            ok
    end.


