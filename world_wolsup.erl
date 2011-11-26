-module(world_wolsup).
-include("world_types.hrl").
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([createWolves/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    gen_event:notify(eventhandler, {world_wolsup, init}),
    {ok, {{one_for_one, 1, 60}, []}}.


%-------------------------------------------------------------------------------------


createWolves(WorldInfo) ->
    newWolf(0, WorldInfo#world_info.wolves, WorldInfo).


newWolf(Created, Total, WorldInfo) when Created < Total ->
    Wolf = {{wolf, Created+1}, {world_wolf, start_link, [{WorldInfo, random}]}, temporary, brutal_kill, worker, [world_wolf]},
    supervisor:start_child(world_wolsup, Wolf),
    newWolf(Created+1, Total, WorldInfo);
newWolf(_Created, _Total, _WorldInfo) ->
    done.
