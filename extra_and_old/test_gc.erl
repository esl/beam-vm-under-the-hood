-module(test_gc).

-export([start_link/1, stuff/2, test/0, test/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

test() ->
    test(3, [ 1000, 10, 2000, 10, 5000, 10, 10000, 10, 40000, 10, 1000, 10]).

test(Max, Seq) when is_integer(Max) ->
    {ok,P} = start_link(Max),
    pinfo(P),
    test(P, Max, Seq).

test(P, Max, [ NBins | More ]) ->
    io:format("~nSending ~p binaries ~p times~n",[NBins, Max+1]),
    [ begin
          Bins = [ rand_str(50) || _ <- lists:seq(1,NBins) ],
          stuff(P, Bins)
      end || _ <- lists:seq(1,Max+1) ],
    pinfo(P),
    test(P, Max, More);
test(P, _, []) ->
    timer:sleep(1000),
    pinfo(P),
    bye(P).

pinfo(P) ->
    [{heap_size,Heap}, {total_heap_size,Tot},
     {memory,Mem}, {garbage_collection,GC}, {binary, Binary}] =
        erlang:process_info(P,[heap_size,total_heap_size,
                               memory,garbage_collection,
                               binary]),
    HeapKBytes = 8 * Heap / 1000,
    TotKBytes = 8 * Tot / 1000,
    MemKBytes = Mem / 1000,
    BinaryLength = length(Binary),
    BinaryKBytes = erlang:memory(binary) / 1000,
    io:format("Heap: ~p of ~p ; Mem: ~p; Minor GCs: ~p binaries: ~p, TotBin:~p~n",
              [HeapKBytes, TotKBytes, MemKBytes, proplists:get_value(minor_gcs, GC),
               BinaryLength, BinaryKBytes]).


start_link(Max) ->
    %% SpawnOpt = [{spawn_opt,[{fullsweep_after, 0}]}],
    %% SpawnOpt = [{hibernate_after, 100}],
    SpawnOpt = [],
    gen_server:start_link(?MODULE, [Max], SpawnOpt).

stuff(P, Stuff) ->
    gen_server:call(P, {stuff, Stuff}).

bye(P) ->
    gen_server:cast(P, bye).

rand_str(Len) ->
    Str0 = [ rand:uniform($~ - $!) + $! || _ <- lists:seq(1,Len) ],
    unicode:characters_to_binary(Str0).

init([Max]) ->
    io:format("Started with max ~p~n",[Max]),
    {ok, {queue:new(),0,Max}}.

handle_call(Call, From, {Q, Size, Max}) when Size > Max ->
    {_,Q1} = queue:out(Q),
    io:format("Removed item from Q..  Total size is ~p~n",
              [size(term_to_binary(Q1))]),
    handle_call(Call, From, {Q1, Size - 1, Max});
handle_call({stuff, X}, _From, St) when not is_list(X) ->
    io:format("Ignoring non-list stuff~n"),
    {reply, ignored, St};
handle_call({stuff, X}, _From, {Q, Size, Max}) ->
    io:format("Got stuff with ~p items~n",[length(X)]),
    Q1 = queue:in(X,Q),
    {reply, ok, {Q1, Size + 1, Max}}.

handle_cast(bye, St) ->
    %% erlang:garbage_collect(self()),
    timer:sleep(10000),
    io:format("Byeeee!~n"),
    {stop, normal, St};
handle_cast(_, St) ->
    {noreply, St}.

%% {fullsweep_after, 0}
%% Heap: 13818.888 of 13818.888 ; Mem: 13819.844; Minor GCs: 0 binaries: 164059, TotBin:37334.504
%% Heap: 600.904 of 600.904 ; Mem: 601.86; Minor GCs: 0 binaries: 3042, TotBin:551.496
%%
%% {fullsweep_after, 1}
%% Heap: 2545.496 of 16364.384 ; Mem: 16365.34; Minor GCs: 1 binaries: 164048, TotBin:30733.28
%% Heap: 2545.496 of 16364.384 ; Mem: 16365.34; Minor GCs: 1 binaries: 164092, TotBin:31068.504
