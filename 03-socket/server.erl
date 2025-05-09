%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, Erlang Solutions Ltd.
%%% @doc Calling start() will create an echo socket server which will do nothing
%%% but echo the incoming data till it disconnects.
%%% See client.erl for the problem description.
%%% @end
%%%-------------------------------------------------------------------
-module(server).

-export([start/0, server/1, handle_messages/1]).

-define(LISTEN_PORT, 11111).

start() ->
    spawn(?MODULE, server, [?LISTEN_PORT]).

server(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{packet, line}]),
    listen(Socket).

listen(Socket) ->
    {ok, Active_socket} = gen_tcp:accept(Socket),
    Handler = erlang:spawn(?MODULE, handle_messages, [Active_socket]),
    ok = gen_tcp:controlling_process(Active_socket, Handler),
    listen(Socket).

handle_messages(Socket) ->
    receive
        {tcp, error, closed} ->
            done;
        {tcp, Socket, Data} ->
            gen_tcp:send(Socket, Data),
            ?MODULE:handle_messages(Socket);
        _Other ->
            unexpected
    end.
