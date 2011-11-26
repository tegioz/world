-module(world).
-include("world_types.hrl").
-behaviour(application).
-export([start/2, stop/1]).


start(_Type, _Args) ->
    {ok, Pid} = world_mainsup:start_link(),
    world_mainsup:populate(#world_info{}),
    {ok, Pid}.


stop(_State) ->
    ok.
