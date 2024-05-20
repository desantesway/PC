-module(offline).
-export([start/0]).

start() ->
    offline(false).

offline(Saved) ->
    receive %save, full_save, load
        {load, Pid} ->  
            case file:read_file("accounts.bin") of
                {ok, Binary} ->
                    Data = binary_to_term(Binary),
                    Pid ! {loaded, Data};
                {error, Reason} ->
                    if Reason == enoent ->
                        Pid ! {loaded, #{"Anonymous" => "admin"}}; %empty map
                    true ->
                        Pid ! {error, Reason},
                        io:format("Error reading file")
                    end
            end,
            offline(false);   
        {full_save, Accs} ->
            io:format("Saving ~p\n", [Accs]),
            Binary = term_to_binary(Accs),
            Ret = file:write_file("accounts.bin", Binary),
            io:format("Saved ~p\n", [Ret]),
            offline(true);
        off -> 
            if Saved -> 
                ok;
            true ->
                offline(Saved)
            end
    end.