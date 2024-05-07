%% PVectors module for vector math functions in Erlang
-module(pvectors).
-export([pvector_dist/2, pvector_sub/2, pvector_add/2]).

%% Define the PVector record for vector math
-record(pvector, {x :: float(), y :: float()}).

%% Calculate the Euclidean distance between two PVectors
-spec pvector_dist(pvector(), pvector()) -> float().
pvector_dist(Vec1, Vec2) ->
    DistanceX = Vec1#pvector.x - Vec2#pvector.x,
    DistanceY = Vec1#pvector.y - Vec2#pvector.y,
    math:sqrt(DistanceX * DistanceX + DistanceY * DistanceY).

%% Subtract two PVectors
-spec pvector_sub(pvector(), pvector()) -> pvector().
pvector_subt(Vec1, Vec2) ->
    #pvector{x = Vec1#pvector.x - Vec2#pvector.x,
             y = Vec1#pvector.y - Vec2#pvector.y}.

%% Add two PVectors
-spec pvector_add(pvector(), pvector()) -> pvector().
pvector_add(Vec1, Vec2) ->
    #pvector{x = Vec1#pvector.x + Vec2#pvector.x,
             y = Vec1#pvector.y + Vec2#pvector.y}.
