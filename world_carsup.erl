-module(world_carsup).
-include("world_types.hrl").
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([createCarrots/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    gen_event:notify(eventhandler, {world_carsup, init}),
    {ok, {{one_for_one, 100, 1}, []}}.


%-------------------------------------------------------------------------------------


createCarrots(WorldInfo) ->
    newCarrot(0, WorldInfo#world_info.carrots, WorldInfo).


newCarrot(Created, Total, WorldInfo) when Created < Total ->
    Carrot = {{carrot, Created+1}, {world_carrot, start_link, [WorldInfo]}, permanent, brutal_kill, worker, [world_carrot]},
    supervisor:start_child(world_carsup, Carrot),
    newCarrot(Created+1, Total, WorldInfo);
newCarrot(_Created, _Total, _WorldInfo) ->
    done.
