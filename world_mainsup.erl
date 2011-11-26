-module(world_mainsup).
-include("world_types.hrl").
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([populate/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    gen_event:start_link({local, eventhandler}),
    gen_event:add_handler(eventhandler, world_screenlogger, []),
    %%gen_event:add_handler(eventhandler, world_udptransmitter, {?RECEIVER_IP,?RECEIVER_PORT}),
    gen_event:notify(eventhandler, {world_mainsup, init}),
    CarrotSup = {carrotsup, {world_carsup, start_link, []}, permanent, brutal_kill, supervisor, [world_carsup]},
    RabbitSup = {rabbitsup, {world_rabsup, start_link, []}, permanent, brutal_kill, supervisor, [world_rabsup]},
    WolfSup = {wolfsup, {world_wolsup, start_link, []}, permanent, brutal_kill, supervisor, [world_wolsup]},
    {ok, {{one_for_all, 1, 60}, [CarrotSup, RabbitSup, WolfSup]}}.


%-------------------------------------------------------------------------------------


populate(WorldInfo) ->
    world_carsup:createCarrots(WorldInfo),
    world_rabsup:createRabbits(WorldInfo),
    world_wolsup:createWolves(WorldInfo),
    done.
