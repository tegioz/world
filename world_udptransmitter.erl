-module(world_udptransmitter).
-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


init({IP, Port}) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active,true}]),
    {ok, {Socket, IP, Port}}.


handle_event(Msg, {Socket, IP, Port}) ->
    gen_udp:send(Socket, IP, Port, term_to_binary(Msg)),
    {ok, {Socket, IP, Port}}.


terminate(_Args, {Socket, _IP, _Port}) ->
    gen_udp:close(Socket),
    ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, State) -> {ok,empty,State}. 
handle_info(_Info, State) -> {ok, State}.
