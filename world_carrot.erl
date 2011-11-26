-module(world_carrot).
-include("world_types.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2]).


start_link(WorldInfo) ->
    gen_server:start_link(?MODULE, WorldInfo, []).


init(WorldInfo) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    {X, Y} = {random:uniform(WorldInfo#world_info.max_x-1), random:uniform(WorldInfo#world_info.max_y-1)},
    State = #carrot_state{pid=self(),world_info=WorldInfo,position=#position{x=X,y=Y}},
    gen_event:notify(eventhandler, {carrot, new, State}),
    {ok, State}.


handle_call({areYouIn, Position}, _From, State) ->
    {X, Y} = {(State#carrot_state.position)#position.x, (State#carrot_state.position)#position.y},
    Result = case {Position#position.x, Position#position.y} of
        {X, Y} -> true;
        _      -> false
    end,
    {reply, Result, State};
handle_call(eatPart, _From, State) ->
    case State#carrot_state.quantity of
        0 -> {stop, normal, {error, carrotFinished}, State};
        _ -> NewQuantity = State#carrot_state.quantity - ?CARROT_UNIT,
             NewState = State#carrot_state{quantity=NewQuantity},
             gen_event:notify(eventhandler, {carrot, beingEaten, NewState}),
             {reply, {ok, partEaten}, NewState}
    end.


terminate(_, State) -> 
    gen_event:notify(eventhandler, {carrot, remove, State}),
    ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
