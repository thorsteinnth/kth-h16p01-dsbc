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
-export([start/2, stop/1, getProcessStatus/1, test/0]).

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
      % Update my map to show that I am directly linked to my interfaces
      % NOTE: Not part of assignment document, adding this myself
      Map1 = map:update(Name, interfaces:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
    {remove, Node} ->
      % Remove node from interface
      {ok, Ref} = interfaces:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = interfaces:remove(Node, Intf),
      printInterfaces(Name, Intf1), % TODO Remove
      % Update my map to show that I am directly linked to my interfaces
      % NOTE: Not part of assignment document, adding this myself
      Map1 = map:update(Name, interfaces:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
    {'DOWN', Ref, process, _, _} ->
      % Monitor has told us that a node is down,
      % remove from interfaces
      {ok, Down} = interfaces:name(Ref,Intf),
      io:format("~w: exit received from ~w~n", [Name, Down]),
      Intf1 = interfaces:remove(Down, Intf),
      printInterfaces(Name, Intf1), % TODO Remove
      % Update my map to show that I am directly linked to my interfaces
      % NOTE: Not part of assignment document, adding this myself
      Map1 = map:update(Name, interfaces:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
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
          % Update the sending node in my map
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
    {route, Name, From, Message} ->
      % Message has arrived at final destination
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);
    {route, To, From, Message} ->
      % Received a message not meant for us, route it onwards
      io:format("~w: routing message (~w)", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case interfaces:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);
    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);
    stop ->
      ok
  end.

printInterfaces(Name, Intf) ->
  io:format("~p (~w): Interfaces: ~p~n", [self(), Name, Intf]).

getProcessStatus(Pid) ->
  Pid ! {status, self()},
  receive
    {status, Reply} ->
      {Name, N, Hist, Intf, Table, Map} = Reply,
      io:format(
        "~p: Received status reply from ~p:
        Name: ~p
        N: ~p
        Hist: ~p
        Intf: ~p
        Table: ~p
        Map: ~p~n",
        [self(), Pid, Name, N, Hist, Intf, Table, Map])
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

test() ->
  start(r1, stockholm),
  start(r2, lund),
  start(r3, uppsala),
  start(r4, malmo),
  r2 ! {add, stockholm, {r1, 'sweden@192.168.1.8'}},
  timer:sleep(500),
  r1 ! {add, lund, {r2, 'sweden@192.168.1.8'}},
  timer:sleep(500),
  r2 ! {add, uppsala, {r3, 'sweden@192.168.1.8'}},
  timer:sleep(500),
  r3 ! {add, malmo, {r4, 'sweden@192.168.1.8'}},
  timer:sleep(500),
  r1 ! broadcast,
  timer:sleep(500),
  r2 ! broadcast,
  timer:sleep(500),
  r3 ! broadcast,
  timer:sleep(500),
  r4 ! broadcast,
  timer:sleep(500),
  r1 ! update,
  timer:sleep(500),
  r2 ! update,
  timer:sleep(500),
  r3 ! update,
  timer:sleep(500),
  r4 ! update,
  timer:sleep(500),
  getProcessStatus(r1),
  getProcessStatus(r2),
  getProcessStatus(r3),
  getProcessStatus(r4),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%