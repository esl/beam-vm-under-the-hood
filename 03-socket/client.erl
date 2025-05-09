%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, Erlang Solutions Ltd.
%%% @doc Calling start() on the socket problem will create a socket client
%%% which will create 100 connections to an echo server (in separate file) and
%%% load them in an uneven random manner. Find the busiest one.
%%% @end
%%%-------------------------------------------------------------------
-module(client).

%% API
-export([start/0]).

-define(CONNECT_PORT, 11111).

start() ->
    [spawn_client() || _I <- lists:seq(1, 10)].

spawn_client() ->
    DataSize = rand:uniform(1000) * 10, % 10b-10kb random data
    Sleep = rand:uniform(100) + 50, % sleep 50..150 ms
    erlang:spawn(fun() -> start_client(DataSize, Sleep) end).

%% @doc Runs in a process, chooses random number and loads the socket
%% accordingly to that number.
start_client(DataSize, Sleep) ->
    {ok, S} = gen_tcp:connect({127, 0, 0, 1}, ?CONNECT_PORT,
                              [binary, {active, true}]),
    client_loop(DataSize, Sleep, S).

%% @doc Send random fixed amount of data, and sleep, flush incoming data
client_loop(DataSize, Sleep, Socket) ->
    % io:format("~p -> ~p bytes~n", [self(), DataSize]),
    gen_tcp:send(Socket, <<0:(DataSize)/unit:8>>),
    case flush() of
        done ->
            %% Disconnected
            ok;
        not_done ->
            %% continue sending and receiving more
            timer:sleep(Sleep),
            client_loop(DataSize, Sleep, Socket)
    end.

%% @doc Receive everything that's arrived
flush() ->
    receive
        {tcp, error, closed} ->
            done;
        {tcp, _Socket, _Data} ->
            % io:format("~p <- ~p bytes~n", [self(), byte_size(Data)]),
            flush();
        _Other ->
            flush()
    after 0 ->
        not_done
    end.
