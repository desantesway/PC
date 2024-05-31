-define(CHANGE_STATE(Pid, Data), Pid ! {fpieces, Data}).
-define(SEND_MESSAGE(Socket, Message), Socket ! {broadcast, Message}).
-define(SEND_MUL_MESSAGE(Socket, List), Socket ! {broadcast_list, List}).
-define(SEND_BROADCAST(Sock, Data),
    gen_tcp:send(Sock, Data)
).

-define(SEND_STATES(Socket,Data),
    lists:foreach(fun(Message) -> 
        {Alive,Username,{_, Boost,{Pos, Velocity, Accel, Angle}, _}} = Message,
        RelevantData = [Username, Boost],
        Socket ! {broadcast, RelevantData}
    end, Data),
).

-define(SEND_BROADCAST_LIST(Sock, Data),
    gen_tcp:send(Sock, "res@@@"),
    lists:foreach(fun(Message) ->
        gen_tcp:send(Sock, Message ++ "@@@")
    end, Data),
    gen_tcp:send(Sock, "\n")
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
-define(UP_KEY, "11").
-define(RIGHT_KEY, "12").
-define(LEFT_KEY, "13").
-define(DOWN_KEY, "14").
-define(CHAT_MESSAGE, "15").
-define(LEAVE_CHAT, "16").
-define(RANKING, "17").
-define(GO, "18").