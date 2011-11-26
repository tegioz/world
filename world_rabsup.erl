-module(world_rabsup).
-include("world_types.hrl").
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([createRabbits/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    gen_event:notify(eventhandler, {world_rabsup, init}),
    {ok, {{one_for_one, 1, 60}, []}}.


%-------------------------------------------------------------------------------------


createRabbits(WorldInfo) ->
    newRabbit(0, WorldInfo#world_info.rabbits, WorldInfo).


newRabbit(Created, Total, WorldInfo) when Created < Total ->
    Rabbit = {{rabbit, Created+1}, {world_rabbit, start_link, [{WorldInfo, random}]}, temporary, brutal_kill, worker, [world_rabbit]},
    supervisor:start_child(world_rabsup, Rabbit),
    newRabbit(Created+1, Total, WorldInfo);
newRabbit(_Created, _Total, _WorldInfo) ->
    done.
