-module(world_wolf).
-include("world_types.hrl").
-behaviour(gen_fsm).
-export([start_link/1, init/1, handle_sync_event/4, terminate/3, handle_event/3, handle_info/3, code_change/4]).
-export([running/2, chasing/2, splitting/2]).
 

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
    State = #wolf_state{pid=self(),position=#position{x=X, y=Y},direction=world_common:newDirection()},
    gen_event:notify(eventhandler, {wolf, new, State}),
    {ok, running, State, ?TIMEOUT}.


terminate(_, _StateName, StateData) -> 
    gen_event:notify(eventhandler, {wolf, remove, StateData}),
    ok. 


handle_event(_Event, StateName, StateData) -> {next_state,StateName,StateData}.
handle_sync_event(_Event, _From, StateName, StateData) -> {next_state,StateName,StateData}.
handle_info(_Info, StateName, StateData) -> {next_state,StateName,StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.


%-------------------------------------------------------------------------------------


running({rabbitToChase, RabbitPid, NotifierPosition}, State) ->
    if  
        abs((State#wolf_state.position)#position.x - NotifierPosition#position.x) =< ?NOTIFY_RATIO, 
        abs((State#wolf_state.position)#position.y - NotifierPosition#position.y) =< ?NOTIFY_RATIO  ->  
            NewState = State#wolf_state{rabbit_being_chased=RabbitPid},
            gen_event:notify(eventhandler, {wolf, hasNewTarget, NewState}),
            {next_state, chasing, NewState, ?QUICK};
        true ->
            {next_state, running, State, ?QUICK}
    end;
running(timeout, State) ->
    {NewPosition, NewDirection} = world_common:nextPosition(State#wolf_state.world_info, State#wolf_state.position, State#wolf_state.direction, false),
    NewState = State#wolf_state{position=NewPosition, direction=NewDirection, time_without_food=State#wolf_state.time_without_food+?TIMEOUT},
    gen_event:notify(eventhandler, {wolf, move, NewState}),
    RabbitsInPosition = rabbitsInPosition(NewPosition),
    case length(RabbitsInPosition) of
        0 when NewState#wolf_state.time_without_food < ?MAX_TIME_WITHOUT_FOOD_WOLF ->
            RabbitsAround = rabbitsAround(NewPosition),
            case length(RabbitsAround) of
                0 -> {next_state, running, NewState, ?TIMEOUT};
                _ -> <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
                     random:seed({A,B,C}),
                     RabbitChasedIndex = random:uniform(length(RabbitsAround)),
                     RabbitChased = lists:nth(RabbitChasedIndex, RabbitsAround),
                     try gen_fsm:send_all_state_event(RabbitChased,{imChasingYou, NewPosition}) of
                        ok ->
                            NewStateChasing = NewState#wolf_state{rabbit_being_chased=RabbitChased},
                            gen_event:notify(eventhandler, {wolf, self(), isChasingNowRabbit, RabbitChased}),
                            notifyRabbitToWolves({RabbitChased, NewPosition}),
                            {next_state, chasing, NewStateChasing, ?TIMEOUT}
                     catch
                         exit:_Reason -> {next_state, running, NewState, ?TIMEOUT}
                     end
            end;
        0 -> 
            {stop, normal, NewState};
        _ when NewState#wolf_state.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF -> 
            RabbitsEaten = eatRabbits(RabbitsInPosition),
            NewStateEating = NewState#wolf_state{target=#target{x=undefined,y=undefined},
                                                 rabbits_eaten=State#wolf_state.rabbits_eaten+RabbitsEaten, 
                                                 time_without_food=0},
            case NewStateEating#wolf_state.rabbits_eaten of
                ?RABBITS_TO_SPLIT -> NewStateSplitting = NewStateEating#wolf_state{rabbits_eaten=0},
                                     {next_state, splitting, NewStateSplitting, ?QUICK};
                _                 -> {next_state, running, NewStateEating, ?TIMEOUT}
            end;
        _ -> 
            {stop, normal, NewState}
    end.


chasing({rabbitToChase, _RabbitPid, _NotifierPosition}, State) ->
    {next_state, chasing, State, ?QUICK};
chasing(timeout, State) ->
    try gen_fsm:sync_send_all_state_event(State#wolf_state.rabbit_being_chased,whereAreYou) of
        {position, Xbef, Ybef} -> 
            State2 = State#wolf_state{target=#target{x=Xbef, y=Ybef}},
            {NewPosition, NewDirection} = world_common:nextPositionWithTarget(State2#wolf_state.position, State2#wolf_state.target),
            gen_event:notify(eventhandler, {wolf, move, State2}),
            State3 = State2#wolf_state{position=NewPosition, direction=NewDirection},
            try gen_fsm:sync_send_all_state_event(State3#wolf_state.rabbit_being_chased,whereAreYou) of
                {position, Xaft, Yaft} ->
                    if
                        NewPosition == #position{x=Xaft, y=Yaft}, 
                        State3#wolf_state.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF,
                        State3#wolf_state.time_chasing =< ?MAX_TIME_CHASING ->
                            RabbitsEaten = eatRabbits([State3#wolf_state.rabbit_being_chased]),
                            State4 = State3#wolf_state{rabbits_eaten=State3#wolf_state.rabbits_eaten+RabbitsEaten, 
                                                       time_without_food=0, rabbit_being_chased=undefined, target=#target{x=undefined,y=undefined}},
                            case State4#wolf_state.rabbits_eaten of
                                ?RABBITS_TO_SPLIT -> NewState = State4#wolf_state{rabbits_eaten=0, time_without_food=0},
                                                     {next_state, splitting, NewState, ?QUICK};
                                _                 -> NewState = State4#wolf_state{time_chasing=0, time_without_food=0},
                                                     {next_state, running, NewState, ?TIMEOUT}
                            end;
                        State3#wolf_state.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF,
                        State3#wolf_state.time_chasing =< ?MAX_TIME_CHASING ->
                            NewState = State3#wolf_state{time_chasing=State3#wolf_state.time_chasing+?TIMEOUT},
                            {next_state, chasing, NewState, ?TIMEOUT};
                        State3#wolf_state.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF ->
                            NewState = State3#wolf_state{time_chasing=0, rabbit_being_chased=undefined, target=#target{x=undefined,y=undefined}},
                            {next_state, running, NewState, ?TIMEOUT};
                        true ->
                            NewState = State3,
                            {stop, normal, NewState}
                    end
            catch
                exit:_Reason -> NewState = State3#wolf_state{time_chasing=0, rabbit_being_chased=undefined, target=#target{x=undefined,y=undefined}},
                                {next_state, running, NewState, ?TIMEOUT}
            end
    catch
        exit:_Reason -> NewState = State#wolf_state{time_chasing=0, rabbit_being_chased=undefined, target=#target{x=undefined,y=undefined}},
                        {next_state, running, NewState, ?TIMEOUT}
    end.


splitting({rabbitToChase, _RabbitPid, _NotifierPosition}, State) ->
    {next_state, splitting, State, ?QUICK};
splitting(timeout, State) ->
    NewWolf = {{wolf, erlang:now()}, {world_wolf, start_link, [{State#wolf_state.world_info, State#wolf_state.position}]}, temporary, brutal_kill, worker, [world_wolf]},
    supervisor:start_child(world_wolsup, NewWolf),
    {next_state, running, State, ?QUICK}.


%-------------------------------------------------------------------------------------


notifyRabbitToWolves({RabbitPid, NotifierPosition}) ->
    gen_event:notify(eventhandler, {wolf, self(), notifyingPresenceOfRabbit, RabbitPid}),
    AllWolves = supervisor:which_children(world_wolsup),
    notifyRabbitToWolves({RabbitPid, NotifierPosition}, AllWolves).
notifyRabbitToWolves({_RabbitPid, _NotifierPosition}, []) ->
    done;
notifyRabbitToWolves({RabbitPid, NotifierPosition}, [{_Id,WolfPid,_Type,_Modules}]) ->
    if 
        WolfPid /= self() -> gen_fsm:send_event(WolfPid, {rabbitToChase, RabbitPid, NotifierPosition});
        true              -> notNotifyingToMyself
    end;
notifyRabbitToWolves({RabbitPid, NotifierPosition}, [{_Id,WolfPid,_Type,_Modules}|MoreWolvesToNotify]) ->
    if 
        WolfPid /= self() -> gen_fsm:send_event(WolfPid, {rabbitToChase, RabbitPid, NotifierPosition});
        true              -> notNotifyingToMyself
    end,
    notifyRabbitToWolves({RabbitPid, NotifierPosition}, MoreWolvesToNotify).


rabbitsInPosition(Position) ->
    AllRabbits = supervisor:which_children(world_rabsup),
    rabbitsInPosition(Position, AllRabbits, []).
rabbitsInPosition(_Position, [], []) ->
    []; 
rabbitsInPosition(_Position, [], RabbitsInPosition) ->
    RabbitsInPosition;
rabbitsInPosition(Position, [{_Id, RabbitPid, _Type, _Modules}|MoreRabbits], RabbitsInPosition) ->
    try gen_fsm:sync_send_all_state_event(RabbitPid,{areYouIn, Position}) of
        true         -> rabbitsInPosition(Position, MoreRabbits, [RabbitPid|RabbitsInPosition]);
        false        -> rabbitsInPosition(Position, MoreRabbits, RabbitsInPosition)
    catch
        exit:_Reason -> rabbitsInPosition(Position, MoreRabbits, RabbitsInPosition)
    end.


rabbitsAround(Position) ->
    AllRabbits = supervisor:which_children(world_rabsup),
    rabbitsAround(Position, AllRabbits, []).
rabbitsAround(_Position, [], []) ->
    []; 
rabbitsAround(_Position, [], RabbitsAround) ->
    RabbitsAround;
rabbitsAround(Position, [{_Id, RabbitPid, _Type, _Modules}|MoreRabbits], RabbitsAround) ->
    try gen_fsm:sync_send_all_state_event(RabbitPid,{areYouNear, Position}) of
        true         -> rabbitsAround(Position, MoreRabbits, [RabbitPid|RabbitsAround]);
        false        -> rabbitsAround(Position, MoreRabbits, RabbitsAround)
    catch
        exit:_Reason -> rabbitsAround(Position, MoreRabbits, RabbitsAround)
    end.


eatRabbits([]) ->
    0;
eatRabbits([RabbitPid|OtherRabbitsPids]) ->
    eatRabbits([RabbitPid|OtherRabbitsPids], 0).
eatRabbits([], RabbitsEaten) ->
    RabbitsEaten;
eatRabbits([RabbitPid|OtherRabbitsPids], RabbitsEaten) ->
    try gen_fsm:sync_send_all_state_event(RabbitPid,youHaveBeenEaten) of
        {ok,_}       -> RabbitsEatenNow = RabbitsEaten+1,
                        gen_event:notify(eventhandler, {wolf, self(), rabbitEaten, RabbitPid}),
                        eatRabbits(OtherRabbitsPids, RabbitsEatenNow)
    catch
        exit:_Reason -> eatRabbits(OtherRabbitsPids, RabbitsEaten)
    end.
