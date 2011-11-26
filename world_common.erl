-module(world_common).
-include("world_types.hrl").
-export([nextPositionWithTarget/2, nextPosition/4, newDirection/0]).


nextPositionWithTarget(Position, Target) ->
    {MovX, MovY} = {Target#target.x - Position#position.x, Target#target.y - Position#position.y},
    PossibleDirections = if  
        MovX>0, MovY>0                       -> [n,e];
        MovX>0, MovY>0, abs(MovX)==abs(MovY) -> [ne];
        MovX>0, MovY<0                       -> [s,e];
        MovX>0, MovY<0, abs(MovX)==abs(MovY) -> [se];
        MovX>0, MovY==0                      -> [e];
        MovX<0, MovY==0                      -> [w];
        MovX==0, MovY>0                      -> [n];
        MovX==0, MovY<0                      -> [s];
        MovX<0, MovY>0                       -> [n,w];
        MovX<0, MovY>0, abs(MovX)==abs(MovY) -> [nw];
        MovX<0, MovY<0                       -> [s,w];
        MovX<0, MovY<0, abs(MovX)==abs(MovY) -> [sw];
        MovX==0, MovY==0                     -> [n,s,e,w,nw,ne,sw,se]
    end,
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    NewDirectionIndex = random:uniform(length(PossibleDirections)),
    NewDirection = lists:nth(NewDirectionIndex, PossibleDirections),
    NewPosition = nextPosition(Position, NewDirection),
    {NewPosition, NewDirection}.


nextPosition(WorldInfo, Position, Direction, NewPositionValid) when not NewPositionValid ->
    NewPosition = nextPosition(Position, Direction),
    {WestLimit, SouthLimit, EastLimit, NorthLimit} = {0, 0, WorldInfo#world_info.max_x, WorldInfo#world_info.max_y},
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    case {NewPosition#position.x, NewPosition#position.y} of
        {WestLimit, _}  -> nextPosition(WorldInfo, Position, newDirection(WorldInfo, Position, Direction), false);
        {_, SouthLimit} -> nextPosition(WorldInfo, Position, newDirection(WorldInfo, Position, Direction), false);
        {EastLimit, _}  -> nextPosition(WorldInfo, Position, newDirection(WorldInfo, Position, Direction), false);
        {_, NorthLimit} -> nextPosition(WorldInfo, Position, newDirection(WorldInfo, Position, Direction), false);
        _               -> nextPosition(WorldInfo, NewPosition, Direction, true)
    end;
nextPosition(_WorldInfo, Position, Direction, _NewPositionValid) ->
    {Position, Direction}.
nextPosition(Position, Direction) ->
    {X, Y} = {Position#position.x, Position#position.y}, 
    case Direction of
        n  -> #position{x=X, y=Y+1};
        s  -> #position{x=X, y=Y-1};
        e  -> #position{x=X+1, y=Y};
        w  -> #position{x=X-1, y=Y};
        nw -> #position{x=X-1, y=Y+1};
        ne -> #position{x=X+1, y=Y+1};
        sw -> #position{x=X-1, y=Y-1};
        se -> #position{x=X+1, y=Y-1}
    end.


newDirection(WorldInfo, Position, Direction) ->
    {West, South, East, North} = {1, 1, WorldInfo#world_info.max_x-1, WorldInfo#world_info.max_y-1},
    PossibleDirections = case {Direction, Position#position.x, Position#position.y} of
        {n,West,_}      -> [s,e,se];
        {n,East,_}      -> [s,w,sw];
        {n,_,_}         -> [s,e,w,se,sw];
        {s,West,_}      -> [n,e,ne];
        {s,East,_}      -> [n,w,nw];
        {s,_,_}         -> [n,e,w,ne,nw];
        {e,_,South}     -> [n,w,nw];
        {e,_,North}     -> [s,w,sw];
        {e,_,_}         -> [n,s,w,nw,sw];
        {w,_,South}     -> [n,e,ne];
        {w,_,North}     -> [s,e,se];
        {w,_,_}         -> [n,s,e,ne,se];
        {nw,West,North} -> [s,e,se];
        {nw,West,_}     -> [n,s,e,ne,se];
        {nw,_,North}    -> [s,e,w,se,sw];
        {ne,East,North} -> [s,w,sw];
        {ne,East,_}     -> [n,s,w,nw,sw];
        {ne,_,North}    -> [s,e,w,se,sw];
        {sw,West,South} -> [n,e,ne];
        {sw,West,_}     -> [n,s,e,ne,se];
        {sw,_,South}    -> [n,e,w,ne,nw];
        {se,East,South} -> [n,w,nw];
        {se,East,_}     -> [n,s,w,nw,sw];
        {se,_,South}    -> [n,e,w,ne,nw]
    end,
    NewDirectionIndex = random:uniform(length(PossibleDirections)),
    lists:nth(NewDirectionIndex, PossibleDirections).
newDirection() ->
    PossibleDirections = [n,s,e,w,nw,ne,sw,se],
    NewDirectionIndex = random:uniform(length(PossibleDirections)),
    lists:nth(NewDirectionIndex, PossibleDirections).
