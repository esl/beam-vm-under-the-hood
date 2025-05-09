%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, Erlang Solutions Ltd.
%%% @doc Create a long running process which consumes CPU periodically
%%% @end
%%%-------------------------------------------------------------------
-module(problem_cpu).

%% API
-export([start/0]).

start() ->
  %% Create a process which sleeps a random (fixed for this process) amount of time.
  %% Run start() a few times to create more processes with different sleep times.
  erlang:spawn(fun() -> process_loop(random:uniform(100) + 50) end).

process_loop(T) ->
  counter(100_000),
  receive after T -> ok end,
  process_loop(T).

counter(0) -> ok;
counter(N) -> counter(N - 1).

