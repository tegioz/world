-module(world_rabbit).
-include("world_types.hrl").
-behaviour(gen_fsm).
-export([start_link/1, init/1, handle_sync_event/4, terminate/3, handle_event/3, handle_info/3, code_change/4]).
-export([running/2, eating/2, splitting/2]).


start_link(WorldInfo) ->
    gen_fsm:start_link(?MODULE, WorldInfo, []).
 

init({WorldInfo, Position}) ->
    case Position of 
        random -> 
            <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
            random:seed({A,B,C}),
            {X, Y} = {random:uniform(WorldInfo#world_info.max_x-1), random:uniform(WorldInfo#world_info.max_y-1)};
        {position, X, Y} ->
            {X, Y}
    end,
    State = #rabbit_state{pid=self(),position=#position{x=X, y=Y},direction=world_common:newDirection()},
    gen_event:notify(eventhandler, {rabbit, new, State}),
    {ok, running, State, ?TIMEOUT}.


handle_sync_event({areYouNear, Position}, _From, StateName, StateData) ->
    if 
        abs((StateData#rabbit_state.position)#position.x - Position#position.x) =< ?NOTIFY_RATIO, 
        abs((StateData#rabbit_state.position)#position.y - Position#position.y) =< ?NOTIFY_RATIO  ->
            Reply = true;
        true ->
            Reply = false
    end,
    {reply, Reply, StateName, StateData, ?QUICK};
handle_sync_event({areYouIn, Position}, _From, StateName, StateData) ->
    Reply = case StateData#rabbit_state.position of
        Position -> true;
        _        -> false
    end,
    {reply, Reply, StateName, StateData, ?QUICK};
handle_sync_event(whereAreYou, _From, StateName, StateData) ->
    Reply = StateData#rabbit_state.position,
    {reply, Reply, StateName, StateData, ?QUICK};
handle_sync_event(youHaveBeenEaten, _From, _StateName, StateData) -> 
    {stop, normal, {ok, imDeadNow}, StateData}.


handle_event({wolfAround, WolfPosition}, _StateName, StateData) -> 
    if 
        abs((StateData#rabbit_state.position)#position.x - WolfPosition#position.x) =< ?NOTIFY_RATIO, 
        abs((StateData#rabbit_state.position)#position.y - WolfPosition#position.y) =< ?NOTIFY_RATIO  ->
            NewState = StateData#rabbit_state{wolf_around=true};
        true ->
            NewState = StateData
    end,
    {next_state, running, NewState, ?QUICK};
handle_event({imChasingYou, WolfPosition}, _StateName, StateData) -> 
    NewState = StateData#rabbit_state{wolf_around=true},
    notifyWolfToRabbits(WolfPosition),
    {next_state, running, NewState, ?QUICK}.


terminate(_, _StateName, StateData) -> 
    gen_event:notify(eventhandler, {rabbit, remove, StateData}),
    ok.


handle_info(_Info, StateName, StateData) -> {next_state, StateName, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


%-------------------------------------------------------------------------------------


running({carrotAround, CarrotPosition}, State) when State#rabbit_state.wolf_around == false ->
    case State#rabbit_state.target of
        {target, undefined, undefined} -> 
            if 
                abs((State#rabbit_state.position)#position.x - CarrotPosition#position.x) =< ?NOTIFY_RATIO, 
                abs((State#rabbit_state.position)#position.y - CarrotPosition#position.y) =< ?NOTIFY_RATIO  ->
                    {X, Y} = {CarrotPosition#position.x, CarrotPosition#position.y},
                    NewState = State#rabbit_state{target=#target{x=X, y=Y}},
                    gen_event:notify(eventhandler, {rabbit, hasNewTarget, NewState});
                true ->
                    NewState = State
            end;
        {target, _Tx, _Ty} ->
            NewState = State
    end,
    {next_state, running, NewState, ?TIMEOUT};
running({carrotAround, _CarrotPosition}, State) ->
    {next_state, running, State, ?TIMEOUT};
running(timeout, State) when State#rabbit_state.wolf_around == true ->
    {NewPosition, NewDirection} = world_common:nextPosition(State#rabbit_state.world_info, State#rabbit_state.position, State#rabbit_state.direction, false),
    State2 = State#rabbit_state{position=NewPosition, direction=NewDirection},
    gen_event:notify(eventhandler, {rabbit, move, State2}),
    if
        State2#rabbit_state.time_without_food < ?MAX_TIME_WITHOUT_FOOD_RABBIT,    
        State2#rabbit_state.time_escaping =< ?MAX_TIME_ESCAPING ->    
            NewState = State2#rabbit_state{time_without_food=State2#rabbit_state.time_without_food+?TIMEOUT, time_escaping=State2#rabbit_state.time_escaping+?TIMEOUT},
            {next_state, running, NewState, ?TIMEOUT};            
        State2#rabbit_state.time_without_food < ?MAX_TIME_WITHOUT_FOOD_RABBIT ->    
            NewState = State2#rabbit_state{wolf_around=false, time_without_food=State#rabbit_state.time_without_food+?TIMEOUT, time_escaping=0},
            {next_state, running, NewState, ?TIMEOUT};
        true ->
            NewState = State2,
            {stop, normal, NewState}
    end;
running(timeout, State) ->
    {NewPosition, NewDirection} = case State#rabbit_state.target of
        {target, undefined, undefined} -> 
            TargetReached = false,
            world_common:nextPosition(State#rabbit_state.world_info, State#rabbit_state.position, State#rabbit_state.direction, false);
        {target, X, Y} when X==(State#rabbit_state.position)#position.x, Y==(State#rabbit_state.position)#position.y ->
            TargetReached = true,
            world_common:nextPosition(State#rabbit_state.world_info, State#rabbit_state.position, State#rabbit_state.direction, false);
        {target, _X, _Y} -> 
            TargetReached = false,
            world_common:nextPositionWithTarget(State#rabbit_state.position, State#rabbit_state.target)
    end, 
    NewState = case TargetReached of
        true  -> State#rabbit_state{position=NewPosition, direction=NewDirection, target=#target{x=undefined,y=undefined}, time_without_food=State#rabbit_state.time_without_food+?TIMEOUT};
        false -> State#rabbit_state{position=NewPosition, direction=NewDirection, time_without_food=State#rabbit_state.time_without_food+?TIMEOUT}
    end,
    gen_event:notify(eventhandler, {rabbit, move, NewState}),
    CarrotInPosition = firstCarrotInPosition(NewPosition),
    case length(CarrotInPosition) of
        0 when NewState#rabbit_state.time_without_food < ?MAX_TIME_WITHOUT_FOOD_RABBIT ->
            {next_state, running, NewState, ?TIMEOUT};
        0 -> 
            {stop, normal, NewState};
        1 when NewState#rabbit_state.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_RABBIT -> 
            notifyCarrotToRabbits(NewPosition),
            NewStateEating = NewState#rabbit_state{target=#target{x=undefined,y=undefined},carrot_being_eaten=lists:nth(1, CarrotInPosition)},
            gen_event:notify(eventhandler, {rabbit, eating, NewStateEating}),
            {next_state, eating, NewStateEating, ?TIMEOUT};
        1 -> 
            {stop, normal, NewState}
    end.


eating({carrotAround, _CarrotPosition}, State) ->
    {next_state, eating, State, ?TIMEOUT};
eating(timeout, State) ->
    try gen_server:call(State#rabbit_state.carrot_being_eaten, eatPart) of
        {ok, _}     -> NewState = State#rabbit_state{carrots_eaten=State#rabbit_state.carrots_eaten+?CARROT_UNIT, time_without_food=0},
                       case NewState#rabbit_state.carrots_eaten of
                           ?CARROTS_TO_SPLIT -> NewStateSplitting = NewState#rabbit_state{carrots_eaten=0, carrot_being_eaten=undefined},
                                                {next_state, splitting, NewStateSplitting, ?QUICK};
                           _                 -> gen_event:notify(eventhandler, {rabbit, eating, NewState}),
                                                {next_state, eating, NewState, ?TIMEOUT}
                       end;
        {error, _}  -> NewState = State#rabbit_state{carrot_being_eaten=undefined},
                       {next_state, running, NewState, ?TIMEOUT}
    catch
        exit:_Reason ->
            NewState = State#rabbit_state{carrot_being_eaten=undefined}, 
            {next_state, running, NewState, ?TIMEOUT}
    end.


splitting({carrotAround, _CarrotPosition}, State) ->
    {next_state, splitting, State, ?QUICK};
splitting(timeout, State) ->
    NewRabbit = {{rabbit, erlang:now()}, {world_rabbit, start_link, [{State#rabbit_state.world_info, State#rabbit_state.position}]}, temporary, brutal_kill, worker, [world_rabbit]},
    supervisor:start_child(world_rabsup, NewRabbit),
    {next_state, running, State, ?QUICK}.


%-------------------------------------------------------------------------------------


notifyCarrotToRabbits(CarrotPosition) ->
    gen_event:notify(eventhandler, {rabbit, self(), notifyingPresenceOfCarrot, CarrotPosition}),
    AllRabbits = supervisor:which_children(world_rabsup),
    notifyCarrotToRabbits(CarrotPosition, AllRabbits).
notifyCarrotToRabbits(_CarrotPosition, []) ->
    done;
notifyCarrotToRabbits(CarrotPosition, [{_Id,RabbitPid,_Type,_Modules}]) ->
    if 
        RabbitPid /= self() -> gen_fsm:send_event(RabbitPid, {carrotAround, CarrotPosition});
        true                -> notNotifyingToMyself
    end;
notifyCarrotToRabbits(CarrotPosition, [{_Id,RabbitPid,_Type,_Modules}|MoreRabbits]) ->
    if 
        RabbitPid /= self() -> gen_fsm:send_event(RabbitPid, {carrotAround, CarrotPosition});
        true                -> notNotifyingToMyself 
    end,
    notifyCarrotToRabbits(CarrotPosition, MoreRabbits).


notifyWolfToRabbits(WolfPosition) ->
    gen_event:notify(eventhandler, {rabbit, self(), notifyingPresenceOfWolf, WolfPosition}),
    AllRabbits = supervisor:which_children(world_rabsup),
    notifyWolfToRabbits(WolfPosition, AllRabbits).
notifyWolfToRabbits(_WolfPosition, []) ->
    done;
notifyWolfToRabbits(WolfPosition, [{_Id,RabbitPid,_Type,_Modules}]) ->
    if 
        RabbitPid /= self() -> gen_fsm:send_all_state_event(RabbitPid, {wolfAround, WolfPosition});
        true                -> notNotifyingToMyself
    end;
notifyWolfToRabbits(WolfPosition, [{_Id,RabbitPid,_Type,_Modules}|MoreRabbits]) ->
    if 
        RabbitPid /= self() -> gen_fsm:send_all_state_event(RabbitPid, {wolfAround, WolfPosition});
        true                -> notNotifyingToMyself
    end,
    notifyWolfToRabbits(WolfPosition, MoreRabbits).


firstCarrotInPosition(Position) ->
    AllCarrots = supervisor:which_children(world_carsup),
    firstCarrotInPosition(Position, AllCarrots).
firstCarrotInPosition(_Position, []) ->
    []; 
firstCarrotInPosition(Position, [{_Id, CarrotPid, _Type, _Modules}|MoreCarrots]) ->
    try gen_server:call(CarrotPid, {areYouIn, Position}) of
        true         -> firstCarrotInPosition(Position, CarrotPid);
        false        -> firstCarrotInPosition(Position, MoreCarrots)
    catch
        exit:_Reason -> firstCarrotInPosition(Position, MoreCarrots)
    end;
firstCarrotInPosition(_Position, CarrotPid) ->
    [CarrotPid].
