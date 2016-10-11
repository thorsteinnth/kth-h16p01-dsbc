-module(test).

-compile(export_all).

-define(Timeout, 1000).


%% Starting up a set of nodes is made easier using this function.

start(Module) ->
    Id = key:generate(), 
    apply(Module, start, [Id]).


start(Module, P) ->
    Id = key:generate(), 
    apply(Module, start, [Id,P]).    

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N-1, P).

%% The functions add and lookup can be used to test if a DHT works.

add(Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive 
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    Node ! {lookup, Key, Q, self()},
    receive 
	{Q, Value} ->
	    Value
    after ?Timeout ->
	    {error, "timeout"}
    end.


%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, gurka, P) end, Keys).

check(Keys, P) ->
    T1 = now(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
    io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).


check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key,P) of
	{Key, _} -> 
	    check(Keys, P, Failed, Timeout);
	{error, _} -> 
	    check(Keys, P, Failed, Timeout+1);
	false ->
	    check(Keys, P, Failed+1, Timeout)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS ADDED BY ME

test1() ->
  % Start first node
  FirstNode = start(node1),
  % Start more nodes
  start(node1, 5, FirstNode),
  % Sleep to let the ring stabilize
  timer:sleep(10000),
  % Send a probe around the ring
  FirstNode ! probe,
  timer:sleep(10000),
  % Send another probe around the ring
  FirstNode ! probe.

test2() ->
  % Start first node
  FirstNode = start(node1),
  % Start more nodes
  start(node1, 5, FirstNode),
  % Start more nodes (do it like this to get a references to them)
  Node7 = start(node1, FirstNode),
  Node8 = start(node1, FirstNode),
  % Sleep to let the ring stabilize
  timer:sleep(10000),
  % Send a probe around the ring
  FirstNode ! probe,
  timer:sleep(10000),
  % Send another probe around the ring
  FirstNode ! probe.

testStore1() ->
  FirstNode = start(node2),
  Node2 = start(node2, FirstNode),
  Node3 = start(node2, FirstNode),
  Node4 = start(node2, FirstNode),
  Node5 = start(node2, FirstNode),
  Node6 = start(node2, FirstNode),
  Node7 = start(node2, FirstNode),
  Node8 = start(node2, FirstNode),
  timer:sleep(10000),
  FirstNode ! probe,
  timer:sleep(1000),
  add(key:generate(), value1, FirstNode),
  add(key:generate(), value2, FirstNode),
  add(key:generate(), value3, FirstNode),
  add(key:generate(), value4, FirstNode),
  FirstNode ! printstore,
  Node2 ! printstore,
  Node3 ! printstore,
  Node4 ! printstore,
  Node5 ! printstore,
  Node6 ! printstore,
  Node7 ! printstore,
  Node8 ! printstore,
  timer:sleep(1000).

% Start nodes, add values, start more nodes
testStore2() ->
  FirstNode = start(node2),
  Node2 = start(node2, FirstNode),
  Node3 = start(node2, FirstNode),
  timer:sleep(10000),
  FirstNode ! probe,
  timer:sleep(10000),
  Value1Key = key:generate(),
  Value2Key = key:generate(),
  Value3Key = key:generate(),
  Value4Key = key:generate(),
  add(Value1Key, value1, FirstNode),
  add(Value2Key, value2, FirstNode),
  add(Value3Key, value3, FirstNode),
  add(Value4Key, value4, FirstNode),
  FirstNode ! printstore,
  Node2 ! printstore,
  Node3 ! printstore,
  timer:sleep(10000),
  Node4 = start(node2, FirstNode),
  Node5 = start(node2, FirstNode),
  Node6 = start(node2, FirstNode),
  Node7 = start(node2, FirstNode),
  Node8 = start(node2, FirstNode),
  timer:sleep(10000),
  FirstNode ! probe,
  timer:sleep(10000),
  FirstNode ! printstore,
  Node2 ! printstore,
  Node3 ! printstore,
  Node4 ! printstore,
  Node5 ! printstore,
  Node6 ! printstore,
  Node7 ! printstore,
  Node8 ! printstore,
  timer:sleep(10000),
  Lookup1 = lookup(Value1Key, FirstNode),
  Lookup2 = lookup(Value2Key, FirstNode),
  Lookup3 = lookup(Value3Key, FirstNode),
  Lookup4 = lookup(Value4Key, FirstNode),
  io:format("LOOKUP RESULTS: ~p ~p ~p ~p~n", [Lookup1, Lookup2, Lookup3, Lookup4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    








