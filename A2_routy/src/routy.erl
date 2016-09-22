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
-export([start/2, stop/1,
  getProcessStatus/1, test/0,
  testSetupSweden/1,
  testSetupIceland/1, testSetupIceland2/1,
  testBroadcastAndUpdateRouters/0, testStopRouterProcesses/0,
  testSweden/1, testIceland/1,
  testSetupStockholmToRifCommunicationSweden/1, testSetupStockholmToRifCommunicationIceland/1]).

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
      %printInterfaces(Name, Intf1), % TODO Remove
      % Update my map to show that I am directly linked to my interfaces
      % NOTE: Not part of assignment document, adding this myself
      Map1 = map:update(Name, interfaces:list(Intf1), Map),
      router(Name, N, Hist, Intf1, Table, Map1);
    {remove, Node} ->
      % Remove node from interface
      {ok, Ref} = interfaces:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = interfaces:remove(Node, Intf),
      %printInterfaces(Name, Intf1), % TODO Remove
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
      %printInterfaces(Name, Intf1), % TODO Remove
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
      printMyStatus(Name, N, Hist, Intf, Table1, Map),
      router(Name, N, Hist, Intf, Table1, Map);
    broadcast ->
      % Make router broadcast a link state message
      Message = {links, Name, N, interfaces:list(Intf)},
      interfaces:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);
    {route, Name, From, Message} ->
      % Message has arrived at final destination
      io:format("~w: received message (~s) ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);
    {route, To, From, Message} ->
      % Received a message not meant for us, route it onwards
      io:format("~w: routing message (~s)~n", [Name, Message]),
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

printMyStatus(Name, N, Hist, Intf, Table, Map) ->
  io:format(
    "~p: STATUS:
    Name: ~p
    N: ~p
    Hist: ~p
    Intf: ~p
    Table: ~p
    Map: ~p~n",
    [self(), Name, N, Hist, Intf, Table, Map]).

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

% NodeDescriptor e.g. sweden@192.168.1.8
% routy:testSetupSweden('sweden@192.168.1.8').
testSetupSweden(NodeDescriptor) ->
  % stockholm-lund-uppsala-malmo
  start(r1, stockholm),
  start(r2, lund),
  start(r3, uppsala),
  start(r4, malmo),
  % Add halmstad, not connected to anything, just adding it here to have 5 routers in all tests
  start(r5, halmstad),
  % Connect r1 and r2
  r1 ! {add, lund, {r2, NodeDescriptor}},
  r2 ! {add, stockholm, {r1, NodeDescriptor}},
  % Connect r2 and r3
  r2 ! {add, uppsala, {r3, NodeDescriptor}},
  r3 ! {add, lund, {r2, NodeDescriptor}},
  % Connect r3 and r4
  r3 ! {add, malmo, {r4, NodeDescriptor}},
  r4 ! {add, uppsala, {r3, NodeDescriptor}},
  % Let's now connect r1 to r3, should then skip r2
  % Connect r1 and r3
  r1 ! {add, uppsala, {r3, NodeDescriptor}},
  r3 ! {add, stockholm, {r1, NodeDescriptor}},
  % Let's now connect r1 to r4, should then skip r2, r3
  % Connect r1 and r4
  %r1 ! {add, malmo, {r4, NodeDescriptor}},
  %r4 ! {add, stockholm, {r1, NodeDescriptor}},
  timer:sleep(500).

% NodeDescriptor e.g. sweden@192.168.1.8
% routy:testSetupSweden('iceland@192.168.1.8').
testSetupIceland(NodeDescriptor) ->
  start(r1, reykjavik),
  start(r2, akureyri),
  start(r3, isafjordur),
  start(r4, rif),
  % Add keflavik, not connected to anything, just adding it here to have 5 routers in all tests
  start(r5, keflavik),
  % Connect r1 and r2
  r1 ! {add, akureyri, {r2, NodeDescriptor}},
  r2 ! {add, reykjavik, {r1, NodeDescriptor}},
  % Connect r2 and r3
  r2 ! {add, isafjordur, {r3, NodeDescriptor}},
  r3 ! {add, akureyri, {r2, NodeDescriptor}},
  % Connect r3 and r4
  r3 ! {add, rif, {r4, NodeDescriptor}},
  r4 ! {add, isafjordur, {r3, NodeDescriptor}},
  timer:sleep(500).

testSetupIceland2(NodeDescriptor) ->
  % New connection, reykjavik-keflavik,rif
  start(r1, reykjavik),
  start(r2, akureyri),
  start(r3, isafjordur),
  start(r4, rif),
  start(r5, keflavik),
  % Connect r1 and r2
  r1 ! {add, akureyri, {r2, NodeDescriptor}},
  r2 ! {add, reykjavik, {r1, NodeDescriptor}},
  % Connect r2 and r3
  r2 ! {add, isafjordur, {r3, NodeDescriptor}},
  r3 ! {add, akureyri, {r2, NodeDescriptor}},
  % Connect r3 and r4
  r3 ! {add, rif, {r4, NodeDescriptor}},
  r4 ! {add, isafjordur, {r3, NodeDescriptor}},
  % Connect r1 and r5
  r1 ! {add, keflavik, {r5, NodeDescriptor}},
  r5 ! {add, reykjavik, {r1, NodeDescriptor}},
  % Connect r5 and r4
  r4 ! {add, keflavik, {r5, NodeDescriptor}},
  r5 ! {add, rif, {r4, NodeDescriptor}},
  timer:sleep(500).

% We have to call broadcast on all routers to make them send their links out into the network
% Once that has happened we have to call update on all routers to make them update their routing tables
% with the new information.
testBroadcastAndUpdateRouters() ->
  r1 ! broadcast,
  timer:sleep(500),
  r2 ! broadcast,
  timer:sleep(500),
  r3 ! broadcast,
  timer:sleep(500),
  r4 ! broadcast,
  timer:sleep(500),
  r5 ! broadcast,
  timer:sleep(500),
  r1 ! update,
  timer:sleep(500),
  r2 ! update,
  timer:sleep(500),
  r3 ! update,
  timer:sleep(500),
  r4 ! update,
  timer:sleep(500),
  r5 ! update,
  timer:sleep(500).

testStopRouterProcesses() ->
  io:format("Will stop all router processes in shell~n", []),
  stop(r1),
  stop(r2),
  stop(r3),
  stop(r4),
  stop(r5).

testSweden(NodeDescriptor) ->
  testSetupSweden(NodeDescriptor),
  testBroadcastAndUpdateRouters(),
  r1 ! {send, malmo, "Message from Stockholm to Malmo"},
  timer:sleep(5000),
  testStopRouterProcesses().

testIceland(NodeDescriptor) ->
  testSetupIceland(NodeDescriptor),
  testBroadcastAndUpdateRouters(),
  r1 ! {send, rif, "Message from Reykjavik to Rif"},
  timer:sleep(5000),
  testStopRouterProcesses().

testSetupStockholmToRifCommunicationSweden(IcelandNodeDescriptor) ->
  % Link malmo to reykjavik
  r4 ! {add, reykjavik, {r1, IcelandNodeDescriptor}},
  timer:sleep(5000),
  testBroadcastAndUpdateRouters().

testSetupStockholmToRifCommunicationIceland(SwedenNodeDescriptor) ->
  % Link reykjavik to malmo
  r1 ! {add, malmo, {r4, SwedenNodeDescriptor}},
  timer:sleep(5000),
  testBroadcastAndUpdateRouters().

% TEST SCRIPT

% Start shells
% erl -name sweden@192.168.1.8 -setcookie routy -connect_all false
% erl -name iceland@192.168.1.8 -setcookie routy -connect_all false

% routy:testSetupSweden('sweden@192.168.1.8').
% routy:testBroadcastAndUpdateRouters().
% routy:testSendMessageStockholmMalmo().
% routy:testStopRouterProcesses().
% OR ALL IN ONE
% routy:testSweden('sweden@192.168.1.8').

% SAME FOR OTHER COUNTRIES

% SCRIPT TO SEND FROM STOCKHOLM TO RIF

% TEST 1

% Sweden
% erl -name sweden@130.229.175.44 -setcookie routy -connect_all false
% routy:testSetupSweden('sweden@130.229.175.44').
% routy:testSetupStockholmToRifCommunicationSweden('iceland@130.229.175.44').
% routy:testBroadcastAndUpdateRouters().
% r1 ! {send, rif, "THIS IS A MESSAGE FROM STOCKHOLM TO RIF"}.

% Iceland
% erl -name iceland@130.229.175.44 -setcookie routy -connect_all false
% routy:testSetupIceland('iceland@130.229.175.44').
% routy:testSetupStockholmToRifCommunicationIceland('sweden@130.229.175.44').

% TEST 2

% Iceland 2 ... connection from reykjavik-keflavik-rif
% then kill Rif and have the system recover

% Sweden
% erl -name sweden@130.229.175.44 -setcookie routy -connect_all false
% routy:testSetupSweden('sweden@130.229.175.44').
% routy:testSetupStockholmToRifCommunicationSweden('iceland@130.229.175.44').
% routy:testBroadcastAndUpdateRouters().
% r1 ! {send, rif, "THIS IS A MESSAGE FROM STOCKHOLM TO RIF"}.

% Iceland
% erl -name iceland@130.229.175.44 -setcookie routy -connect_all false
% routy:testSetupIceland2('iceland@130.229.175.44').
% routy:testSetupStockholmToRifCommunicationIceland('sweden@130.229.175.44').

% Now let's kill keflavik
% Iceland
% routy:stop(r5).
% Update Icelandic routers ... have to do them one by one now because we only have 4 left, so the script doesn't work
% r1 ! broadcast.
% r2 ! broadcast.
% r3 ! broadcast.
% r4 ! broadcast.
% r1 ! update.
% r2 ! update.
% r3 ! update.
% r4 ! update.

% Sweden
% routy:testBroadcastAndUpdateRouters().
% r1 ! {send, rif, "THIS IS A MESSAGE FROM STOCKHOLM TO RIF"}.

% Then we can re-add keflavik

% Iceland
% routy:start(r5, keflavik).
% r1 ! {add, keflavik, {r5, 'iceland@130.229.175.44'}}.
% r5 ! {add, reykjavik, {r1, 'iceland@130.229.175.44'}}.
% r4 ! {add, keflavik, {r5, 'iceland@130.229.175.44'}}.
% r5 ! {add, rif, {r4, 'iceland@130.229.175.44'}}.
% r5 ! broadcast.
% r1 ! broadcast.
% r4 ! broadcast.
% r1 ! update.
% r4 ! update.
% r5 ! update.

% Sweden
% r1 ! {send, rif, "THIS IS A MESSAGE FROM STOCKHOLM TO RIF"}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%