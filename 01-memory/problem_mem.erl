%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, Erlang Solutions Ltd.
%%% @doc Create a long running process which consumes 1 GB of memory
%%% @end
%%%-------------------------------------------------------------------
-module(problem_mem).

%% API
-export([start/0]).

start() ->
  erlang:spawn(fun process_loop/0).

process_loop() ->
  BigData = [big_data(1048576) || _ <- lists:seq(1, 1024)],
  erlang:garbage_collect(),
  %% There is a bug here: infinite receive is considered end of program by the optimizing compiler.
  receive after infinity -> ok end,
  BigData.

big_data(N) -> <<0:(N)/unit:8>>.
