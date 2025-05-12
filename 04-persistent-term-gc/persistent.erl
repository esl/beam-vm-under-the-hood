%%% @doc NOTE: Uses new tracer API available in OTP 27.
%%% Define a persistent term from one process and then have 5 processes which also have accessed it.
%%% Then modify the persistent term, the other processes should each do a major GC.
%%% Trace will witness the GCs and print messages showing that they have been collected.
%%% In Erlang shell:
%%% 1> c(persistent), l(persistent), persistent:start().
%%% @end

-module(persistent).

-export([start/0]).

create_value() -> lists:duplicate(100, erlang:monotonic_time()).

trace_print({trace, Pid, gc_minor_end, _Args}) -> io:format("Minor garbage collection finished ~p~n", [Pid]);
trace_print({trace, Pid, gc_major_end, _Args}) -> io:format("MAJOR garbage collection finished ~p~n", [Pid]);
trace_print(_M) -> ok.

start() ->
    persistent_term:put(value1, create_value()),

    Tracer = spawn(fun F() -> receive M -> trace_print(M), F() end end),
    Session = trace:session_create(persistent, Tracer, []),
    trace:process(Session, new, true, [garbage_collection]),

    %% Spawn processes which also have accessed the term by key and now "have seen it".
    Started = [spawn(fun() ->
        io:format("Process ~p starting (and getting the value1)~n", [self()]),
        T1 = persistent_term:get(value1),
        timer:sleep(5_000),
        io:format("Process ~p terminating~n", [self()]),
        T1 % do something un-optimizable to keep the value alive
    end) || _ <- lists:seq(1, 5)],
    io:format("Spawned processes which all have seen the value1: ~0p~n", [Started]),
    timer:sleep(1_000), % Give processes time to schedule in and see the value

    %% Modify the term and wait for the processes to be garbage collected.
    io:format("Modifying the term and waiting for all processes to be garbage collected...~n"),
    persistent_term:put(value1, create_value()),

    io:format("Main process sleeping 5 s and waiting for all processes to be garbage collected...~n"),
    timer:sleep(5_000),

    trace:session_destroy(Session),
    io:format("Done, tracing done too.~n").
