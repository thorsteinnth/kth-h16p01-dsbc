%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2016 11:52
%%%-------------------------------------------------------------------
-module(routy).
-author("tts").

%% API
-export([start/2, stop/1]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = interfaces:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      % Add node to interfaces
      Ref = erlang:monitor(process, Pid),
      Intf1 = interfaces:add(Node, Ref, Pid, Intf),
      printInterfaces(Name, Intf1), % TODO Remove
      router(Name, N, Hist, Intf1, Table, Map);
    {remove, Node} ->
      % Remove node from interface
      {ok, Ref} = interfaces:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = interfaces:remove(Node, Intf),
      printInterfaces(Name, Intf1), % TODO Remove
      router(Name, N, Hist, Intf1, Table, Map);
    {'DOWN', Ref, process, _, _} ->
      % Monitor has told us that a node is down,
      % remove from interfaces
      {ok, Down} = interfaces:name(Ref,Intf),
      io:format("~w: exit received from ~w~n", [Name, Down]),
      Intf1 = interfaces:remove(Down, Intf),
      printInterfaces(Name, Intf1), % TODO Remove
      router(Name, N, Hist, Intf1, Table, Map);
    {status, From} ->
      % Received a status request message from From
      % Let's reply with our status
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);
    {links, Node, R, Links} ->
      % Received link state message
      % Update map if new message
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          % Broadcast message to all my interfaces
          interfaces:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          % This was an old message, do nothing
          router(Name, N, Hist, Intf, Table, Map)
      end;
    update ->
      % Make router update its routing table
      Table1 = dijkstra:table(interfaces:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);
    broadcast ->
      % Make router broadcast a link state message
      Message = {links, Name, N, interfaces:list(Intf)},
      interfaces:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);
    stop ->
      ok
  end.

printInterfaces(Name, Intf) ->
  io:format("~p (~w): Interfaces: ~p~n", [self(), Name, Intf]).

getProcessStatus(Pid) ->
  Pid ! {status, self()},
  receive
    {status, Reply} ->
      io:format("~p: Received status reply from ~p: ~p~n", [self(), Pid, Reply])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testAddRemoveInterfaces() ->
  % Start two routers and connect them
  start(r1, london),
  start(r2, berlin),
  start(r3, stockholm),
  r1 ! {add, berlin, r2},
  r1 ! {add, stockholm, r3},
  r2 ! {add, london, r1},
  r3 ! {add, berlin, r2},
  r3 ! {add, london, r1},
  timer:sleep(5000),
  io:format("Removing stockholm from london~n", []),
  r1 ! {remove, stockholm},
  timer:sleep(5000),
  io:format("Stopping berlin~n", []),
  stop(r2),
  ok.

testGetProcessStatus() ->
  start(r1, london),
  start(r2, berlin),
  r1 ! {add, berlin, r2},
  r2 ! {add, london, r1},
  getProcessStatus(r2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%