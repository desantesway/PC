-define(CHANGE_STATE(Pid, Data), Pid ! {fpieces, Data}).
-define(SEND_MESSAGE(Socket, Message), Socket ! {broadcast, Message}).
-define(SEND_MUL_MESSAGE(Socket, List), Socket ! {broadcast_list, List}).
-define(SEND_BROADCAST(Sock, Data),
    gen_tcp:send(Sock, Data),
    gen_tcp:send(Sock, "!-SVDONE-!\n")
).

-define(SEND_BROADCAST_LIST(Sock, Data),
    lists:foreach(fun(Message) ->
        gen_tcp:send(Sock, Message ++ "\n")
    end, Data),
    gen_tcp:send(Sock, "!-SVDONE-!\n")
).

-define(CREATE_ACCOUNT, "1").
-define(LOGIN_ACCOUNT, "2").
-define(LOGOUT_ACCOUNT, "3").
-define(JOIN_ROOM, "4").
-define(LEAVE_ROOM, "5").
-define(CHANGE_NAME, "6").
-define(CHANGE_PASS, "7").
-define(REMOVE_ACCOUNT, "8").
-define(CREATE_ROOM, "9").
-define(LIST_ROOMS, "10").
-define(USERS_INFO, "11").
-define(NEW_KEY, "12").