-module(game_player).
-export([start/4]).
-include("server.hrl").
-include("pvectors.hrl").

-define(START_POS, [#pvector{x=100,y=100},#pvector{x=1500,y=1000}, #pvector{x=100,y=1000}, #pvector{x=1500,y=100}]). 
-define(SUN_RADIUS, 135). % Radius of sun
-define(SUN_POS, #pvector{x=1920/2,y=1080/2}). % Position of sun
-define(TOP_SPEED, 12). % Maximum speed of player
-define(ACCEL_MAG, 0.06). % Magnitude of acceleration
-define(PLAYER_RADIUS, 25).  % Radius of player

start(GameProc, Sock, UserAuth, PlayerNum) -> %PlayerState = {{Name, Level, Lobby, XP}, BoostLeft, {Pos, Velocity, Angle}, Buttons}
    {Name, _, _, _} = UserAuth,
    Keys = {false, false, false,false},
    Me = self(),
    GameProc ! {new_pid, Name, Me, PlayerNum},
    gamePlayer(GameProc, Sock, {UserAuth, 10000, {#pvector{x=0,y=0}, #pvector{x=0,y=0},#pvector{x=0,y=0},0}, Keys},0). 

gamePlayer(GameProc, Sock, PlayerState, PlayerIndex) ->
    receive
        {broadcast, Data} ->
            ?SEND_BROADCAST(Sock, Data),
            gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
        {broadcast_list, Data} ->
            ?SEND_BROADCAST_LIST(Sock, Data),
            gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
        {start_pos, Index} -> % só é chamado uma vez para a posiçao inicial
            NewState = setupPlayerState(PlayerState, Index), 
            %io:format("Starting state ~p\n", [NewState]),
            GameProc ! {starting_pos, self(), NewState, Index},
            %{{Name,_,_,_}, Boost, {VecPos, _,_, _}, _} = NewState,
            % Mensagem pode ser imediatamente enviada ao jogador. maybe. or wait until all player states are set up
            %?SEND_MESSAGE(Sock, "state\n" ++ NewState ++ "\n"),
            gamePlayer(GameProc, Sock, NewState, Index);
        {player_state} ->
            io:format("Player ~p => state ~p\n", [self(), PlayerState]),
            NewPos = getNextPos(PlayerState),
            %io:format("Player ~p => state ~p\n", [self()], NewPos),
            Bool = checkCollision(NewPos),
            case Bool of 
                true -> % if the player collides with the sun or the screen
                    Me = self(),
                    GameProc ! {died, Me},
                    io:format("Position of death: ~p\n", [NewPos]);
                false -> % if the player does not collide with the sun or the screen
                    GameProc ! {player_state, self(), NewPos, PlayerIndex}
            end,
            gamePlayer(GameProc, Sock, NewPos, PlayerIndex);
        {updated_state, NewState} ->
            {_,_,_,Keys} = PlayerState,
            {UsAuth, Boost, {Pos, Vel, Acc, Angle}, _} = NewState,
            gamePlayer(GameProc, Sock, {UsAuth, Boost, {Pos,Vel,Acc,Angle}, Keys}, PlayerIndex);
        {fpieces, Data} ->
            case Data of
                {died} ->
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                {unexpected_leave} ->
                    self() ! {fpieces, {end_game}},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                {won} ->
                    {{Name, Lvl, Lobby, XP}, Boost, Pos, But} = PlayerState,
                    if XP < 1 ->
                        NXP = 1;
                    true ->
                        NXP = XP + 1
                    end,
                    if Lvl == NXP ->
                        NLvl = Lvl + 1;
                    true ->
                        NLvl = Lvl
                    end,
                    gamePlayer(GameProc, Sock, {{Name, NLvl, Lobby, NXP}, Boost, Pos, But}, PlayerIndex);
                {lost} ->
                    {{Name, Lvl, Lobby, XP}, Boost, Pos, But} = PlayerState,
                    if XP > - 1 ->
                        NXP = - 1;
                    true ->
                        NXP = XP - 1
                    end,
                    if Lvl/2 == - NXP ->
                        if Lvl == 1 ->
                            NLvl = 1;
                        true ->
                            NLvl = Lvl - 1
                        end;
                    true ->
                        NLvl = Lvl
                    end,
                    gamePlayer(GameProc, Sock, {{Name, NLvl, Lobby, NXP}, Boost, Pos, But}, PlayerIndex);
                {end_game} ->
                    io:format("Player ~p leaving game\n", [self()]),
                    {{Name, Level, _, XP}, _, _, _} = PlayerState,
                    accsProc ! {update_lvl, self(), Level, XP},
                    server:userAuth(Sock, {Name, Level, "main", XP})
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CHAT_MESSAGE>>, Message] -> % sends a chat message
                    GameProc ! {send_message, 
                        self(), 
                        string:trim(binary_to_list(Message), trailing)},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                [<<?LEAVE_CHAT>>, _] -> % sends a chat message
                    io:fwrite("Player ~p leaving chat\n", [self()]),
                    GameProc ! {leave_chat, self()},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                [<<?GO>>, _] ->
                    GameProc ! {go},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                [<<?UP_KEY>>, _] -> % sum about new button pressed (?)
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, DOWN, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {toggle(UP), DOWN, LEFT, RIGHT},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    % as if the calculations made the player die;
                    %GameProc ! {died, self()}, % !!!! ONLY FOR TESTING DEATH/XP this is supposed to be handled by the game_sim, not the player
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex);
                [<<?RIGHT_KEY>>, _] -> % sum about new button pressed (?)
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, DOWN, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {UP, DOWN, LEFT, toggle(RIGHT)},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex);
                [<<?LEFT_KEY>>, _] -> % sum about new button pressed (?)
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, DOWN, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {UP, DOWN, toggle(LEFT), RIGHT},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex);
                [<<?DOWN_KEY>>, _] -> 
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, DOWN, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {UP, toggle(DOWN), LEFT, RIGHT},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex)
            end;
        {tcp_closed, _} ->
            {{_, _, Lobby, _}, _, _, _} = PlayerState,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {interrupt_game, self()};
        {tcp_error, _, _} ->
            {{_, _, Lobby, _}, _, _, _} = PlayerState,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {interrupt_game, self()};
        Other ->
            io:fwrite("ERROR ~p ~p\n", [self(), Other]),
            gamePlayer(GameProc, Sock, PlayerState, PlayerIndex)
    end.


% helper function to toggle a boolean value - why does erlang not have a ! operator???
toggle(true) -> false;
toggle(false) -> true.


%% Initialize a player state with a given index Playerpos
setupPlayerState(PlayerState, Playerpos) -> % I need the player position. Player 1 gets position #1 ... Player 4 gets position #4
    io:format("Attempting to get playerState ~p\n", [PlayerState]),
    InitialPos = lists:nth(Playerpos, ?START_POS),
    Vec = #pvector{x = 0.0, y = 0.0},
    KeyMap = {false, false, false,false}, %default keymap {up,left,right}
    {Name, _, _, _} = PlayerState,
    PlayerState1 = {Name, 10000,   {InitialPos,   Vec,        Vec,       0},   KeyMap},
    %             {Username?, Boost,  {{PX,PY},   {VX,VY}  ,{AccX,AccY}, Angle}, Keys}
    PlayerState1.


getNextPos(PlayerState) ->
    Sunpos = ?SUN_POS,
    {Name, Boost, {Pos, Vel, _, _}, {UP,DOWN,LEFT,RIGHT} = KeyMap} = PlayerState,
    AccMag = ?ACCEL_MAG,
    Topspeed = ?TOP_SPEED,
    Accel = pvector_sub(Sunpos, Pos), % Get the vector from the player to the sun
    Accel1 = set_magnitude(Accel, AccMag), % Limit the magnitude of the acceleration vector to 0.1
    KeyAccel = #pvector{x=0,y=0},
    case Boost of
        0 -> 
            Acc____ = KeyAccel,
            Boost____ = 0;
        _ -> 
            case UP of
                true -> 
                    Mov = #pvector{x=0, y = -0.2},  % Calculate velocity vector for a given angle
                    Acc_ = pvector_add(KeyAccel, Mov), % Add the key acceleration to the angle movement
                    Boost_ = Boost - 1;                                                   
                _ -> 
                    Acc_ = KeyAccel,
                    Boost_ = Boost
                    
            end,
            case DOWN of 
                true -> 
                    Mov3 = #pvector{x=0, y = 0.2},  % Calculate velocity vector for a given angle
                    Acc__ = pvector_add(Acc_, Mov3), % Add the key acceleration to the angle movement
                    Boost__ = Boost_ - 1;
                _ -> 
                    Acc__ = Acc_,
                    Boost__ = Boost_
            end,
            case LEFT of
                true ->
                    % Calculate velocity vector for a given angle
                    %AngleMovement1 = #pvector{x=math:cos(Angle + math:pi()/2) * 0.1, y = math:sin(Angle + math:pi()/2) * 0.1},
                    %Acc__ = pvector_add(Acc, AngleMovement1), % Add the key acceleration to the angle movement
                    %Boost__ = Boost_ - 1;
                    Mov1 = #pvector{x=-0.2, y = 0},
                    Boost___ = Boost__ - 1,
                    Acc___ = pvector_add(Acc__, Mov1);
                _ ->
                    Acc___ = Acc__,
                    Boost___ = Boost__
            end,
            case RIGHT of
                true ->
                    % Calculate velocity vector for a given angle
                    %AngleMovement2 = #pvector{x=math:cos(Angle - math:pi()/2) + 0.1, y = math:sin(Angle - math:pi()/2) + 0.1},
                    %Acc___= pvector_add(Acc, AngleMovement2), % Add the key acceleration to the angle movement
                    %Boost___ = Boost__ - 1;
                    Mov2 = #pvector{x=0.2, y = 0},
                    Acc____ = pvector_add(Acc___, Mov2),
                    Boost____ = Boost___ - 1;
                _ ->
                    Acc____ = Acc___,
                    Boost____ = Boost___
            end
    end,
    
    FinalAccel = pvector_add(Accel1, Acc____), % Add the key acceleration to the angle movement
    Velocity = pvector_add(FinalAccel, Vel), % Finally, add accel to velocity
    LimitVel = pvector_limit(Velocity, Topspeed), % Limit the velocity to the top speed
    NewPos = pvector_add(LimitVel, Pos), % Add velocity to position

    NewPlayerState = {Name, Boost____, {NewPos, LimitVel, FinalAccel, 0}, KeyMap},
    NewPlayerState.


checkCollision(PlayerState) ->
    {_, _, {Pos, _, _, _}, _} = PlayerState,
    Sunpos = ?SUN_POS,
    Sunrad = ?SUN_RADIUS,
    Playrad = ?PLAYER_RADIUS,
    SunDist = pvector_dist(Pos, Sunpos),
    SunCollide = SunDist < Playrad + (Sunrad - 10), %% Give the player some leeway man

    % Check for margin collisions with the screen
    %% these values are for 1920x1080 screen... I should probably ask the client for displayWidth/displayHeight
    %% or just force the client to be 1920x1080 :)
    CollideX = case Pos#pvector.x of 
                   X when X < -10 ->  
                       true;
                   X when X > 1930 ->
                       true;
                   _ ->
                       false
               end,
    CollideY = case Pos#pvector.y of 
                   Y when Y < -10 -> 
                       true;
                   Y when Y > 1090 ->
                       true;
                   _ ->
                       false
               end,
    
    % Check for collision with the sun or the screen
    SunCollide or CollideX or CollideY.