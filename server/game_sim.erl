-module(game_sim).
-include("server.hrl").
-include("pvectors.hrl").
-export([start/3]).


-define(START_POS, [#pvector{x=100,y=100},#pvector{x=1500,y=1000}, #pvector{x=100,y=1000}, #pvector{x=1500,y=100}]). % All possible starting positions for players
-define(DELTA_SPEED, 0.1). % Change in speed
-define(SUN_RADIUS, 135). % Radius of sun
-define(PLANET_RADIUS, 100). % Radius of planets
-define(SUN_POS, #pvector{x=1920/2,y=1080/2}). % Position of sun
-define(TOP_SPEED, 10). % Maximum speed of player
-define(ACCEL_MAG, 0.08). % Magnitude of acceleration
-define(PLAYER_RADIUS, 25).  % Radius of player


start(GameProc, Sock, UserAuth) -> % CAHNGE THIS - User = {{Name, Level, XP}, Boost, {{PX,PY}, {VX,VY}, Angle}, Buttons}, fazer teclas default aqui
    {Name, _, _} = UserAuth,
    PlayerState = setupPlayerState(Name),
    gameSim(GameProc, Sock, PlayerState). 

gameSim(GameProc, Sock, User) ->
    receive
        {broadcast, Data} ->
            ?SEND_BROADCAST(Sock, Data),
            gameSim(GameProc, Sock, User);
        {broadcast_list, Data} ->
            ?SEND_BROADCAST_LIST(Sock, Data),
            gameSim(GameProc, Sock, User);
        {fpieces, Data} ->
            case Data of
                {send_pid} ->
                    GameProc ! {new_pid, self()},
                    gameSim(GameProc, Sock, User);
                {end_game} ->
                    {{Name, Level, _, XP}, _, _, _} = User,
                    server:userAuth(Sock, {Name, Level, "main", XP})
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?USERS_INFO>>, _] -> %sum about requesting user info
                    gameSim(GameProc, Sock, User);
                [<<?NEW_KEY>>, _] -> %sum about new button pressed (?)
                    gameSim(GameProc, Sock, User)
            end;
        {tcp_closed, _} ->
            {{_, _, Lobby, _}, _, _, _} = User,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {end_game, self()};
        {tcp_error, _, _} ->
            {{_, _, Lobby, _}, _, _, _} = User,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {end_game, self()};
        Other ->
            io:fwrite("ERROR ~p ~p\n", [self(), Other]),
            gameSim(GameProc, Sock, User)
    end.



%% Initialize a player state with a given index Playerpos
setupPlayerState(User, Playerpos) -> % I need the player position. Player 1 gets position #1 ... Player 4 gets position #4
    InitialPos = lists:get(Playerpos, ?START_POS),
    Vec = #pvector{x = 0.0, y = 0.0},
    KeyMap = #{false, false, false}, %default keymap {up,left,right}
    PlayerState = {User, 100,   {InitialPos,   Vec,        Vec,       0},   KeyMap},
    %             {Username?, Boost,  {{PX,PY},   {VX,VY}  ,{AccX,AccY}, Angle}, Buttons}
    PlayerState.

    
getNextPos(PlayerState) ->
    Sunpos = ?SUN_POS,
    {User, Boost, {Pos, Vel, Acc, Angle}, {UP,LEFT,RIGHT} = KeyMap} = PlayerState,
    AccMag = ?ACCEL_MAG,
    Topspeed = ?TOP_SPEED,
    
    
    Accel = pvector_sub(Sunpos, Pos), % Get the vector from the player to the sun

    Accel1 = set_magnitude(Accel, 0.1), % Limit the magnitude of the acceleration vector to 0.1
    KeyAccel = #pvector{x=0,y=0},

    case UP of ->
        true -> 
            AngleMovement = #pvector{x=math:cos(Angle) * Pos#pvector.x, y = math:sin(Angle) * Pos#pvector.y}  % Calculate velocity vector for a given angle
            Acc_ = pvector_add(KeyAccel, AngleMovement), % Add the key acceleration to the angle movement
        _ -> 
            Acc_ = KeyAccel
    end,
    case LEFT of ->
        true ->
            % Calculate velocity vector for a given angle
            AngleMovement = #pvector{x=math:cos(Angle + math:pi/2) * Pos#pvector.x, y = math:sin(Angle + math:pi/2) * Pos#pvector.y}  
            Acc__ = pvector_add(Acc, AngleMovement), % Add the key acceleration to the angle movement
        false ->
            Acc__ = Acc_
    end,
    case RIGHT of ->
        true ->
            % Calculate velocity vector for a given angle
            AngleMovement = #pvector{x=math:cos(Angle - math:pi/2) * Pos#pvector.x, y = math:sin(Angle - math:pi/2) * Pos#pvector.y}  
            Acc___= pvector_add(Acc, AngleMovement), % Add the key acceleration to the angle movement
        false ->
            Acc___ = Acc__
    end,
    FinalAccel = pvector_add(Accel1, Acc___), % Add the key acceleration to the angle movement
    Velocity = pvector_add(FinalAccel, Vel), % Finally, add accel to velocity
    LimitVel = pvector_limit(Velocity, Topspeed), % Limit the velocity to the top speed
    NewPos = pvector_add(LimitVel, Pos), % Add velocity to position

    NewPlayerState = {User, Boost, {NewPos, LimitVel, FinalAccel, Angle}, KeyMap},
    NewPlayerState.


checkCollision(PlayerState) ->
    {_, _, {Pos, Vel, Acc, Angle}, _} = PlayerState,
    Sunpos = ?SUN_POS,
    Sunrad = ?SUN_RADIUS,
    Playrad = ?PLAYER_RADIUS,
    SunDist = pvector_dist(Pos, Sunpos),

    %% Check for margin collisions with the screen
    CollideX = case Pos#pvector.x of 
                   X when X < 0 -> 
                       true;
                   X when X > 1920 ->
                       true;
                   _ ->
                       false
               end,
    CollideY = case Pos#pvector.y of 
                   Y when Y < 0 -> 
                       true;
                   Y when Y > 1080 ->
                       true;
                   _ ->
                       false
               end,

    %% Check for collision with the sun or the screen
    SunDist < Sunrad + Playrad or CollideX or CollideY.

  