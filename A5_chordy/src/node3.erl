%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 14:51
%%%-------------------------------------------------------------------
-module(node3).
-author("tts").

-define(Stabilize, 1000).
-define(Timeout, 10000).

%% API
-export([node/5, start/1, start/2]).

% First implementation that only handles a growing ring.

% Start node - we are the first node in a ring
start(Id) ->
  start(Id, nil).

% Start node - we are joining a ring
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  % Connect to our successor
  {ok, Successor} = connect(Id, Peer),
  %printSuccessor(Successor),
  schedule_stabilize(),
  % We don't know our successor's successor yet (Next), just putting in as nil
  node(Id, Predecessor, Successor, storage:create(), nil).

connect(Id, nil) ->
  % We are the first node in the ring
  % We are in fact connecting to ourselves
  % We are our own successor
  {ok, {Id, self()}};
connect(Id, Peer) ->
  % Id is our own ID
  % Peer is the PID of the node in the ring we are connecting to
  % We are connecting to an existing ring
  % We are connecting to Peer
  % Send a key message to Peer and wait for a reply
  Qref = make_ref(),  % Returns a unique reference. The reference is unique among connected nodes.
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      % We have received the key for Peer
      % We have our new successor
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

% Predecessor and successor are of the form {Key, Pid}
% Next is our successor's successor (safety pointer for fault tolerance)
node(Id, Predecessor, Successor, Store, Next) ->
  receive
    {key, Qref, Peer} ->
      % A peer needs to know our key
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next);
    {notify, New} ->
      % A new node informs us of its existence
      % i.e. suggesting that it might be our predecessor
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, NewStore, Next);
    {request, Peer} ->
      % A predecessor needs to know our predecessor, and our successor
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {status, Pred, Nx} ->
      % Our successor informs us about its predecessor, and its successor
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      %io:format("[~p] STABILIZE/3 JUST FINISHED, NEW SUCCESSOR: ~p~n", [self(), Succ]),
      node(Id, Predecessor, Succ, Store, Nxt);
    stabilize ->
      % Send a request message to our successor.
      %io:format("[~p] WILL ENTER STABILIZE/1 WITH SUCCESSOR ARG AS: ~p~n", [self(), Successor]),
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next);
    probe ->
      % We should send a probe
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Id, Nodes, T} ->
      % We just received our own probe, let's remove it (and log it)
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next);
    {probe, Ref, Nodes, T} ->
      % We just received a prove from another node, let's forward it to our successor
      % (and add ourselves to the Nodes list)
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next);
    {add, Key, Value, Qref, Client} ->
      % Add key and value to store
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next);
    {lookup, Key, Qref, Client} ->
      % Lookup key in store
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next);
    {handover, Elements} ->
      % Received a batch of elements to add to my Store
      % Message from a node that has accepted us as their predecessor
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged, Next);
    printstore ->
      % Added this myself for debugging purposes
      storage:printStore(Store),
      node(Id, Predecessor, Successor, Store, Next);
    stop ->
      ok;
    _ ->
      io:format("Unknown message type")
  end.

% Add a new key value to the store
% Must determine if our node is the node that should take care of the key.
% Will take care of all keys from (not including) ID of predecessor to (and including) ID of myself.
% If we are not responsible for this key-value we send an add message to our successor.
% Return new Store
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  % Can use the key:between function
  case key:between(Key, Pkey, Id) of
    true ->
      % I should take care of this key
      Client ! {Qref, ok},
      NewStore = storage:add(Key, Value, Store),
      NewStore;
    false ->
      % We should not take care of this key, send add message to our successor
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

% Lookup in the Store
% Check if we are responsible for this key-value
% If so, do a lookup in the Store and send the result to the Client
% If not, forward the request to our Successor
% Return nothing
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      % This Key is our responsibility
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      % This Key is not our responsibility
      {_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

% Send a probe message to Successor
% Probe message is of the form {probe, I, Nodes, Time}
% I is the ID of the node that sent the probe
% Nodes is a list of process identifiers in the ring
% Time is a timestamp created by the node that created the probe
% (does not mean anything on other nodes (it is local time))
create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Id, [{Id, self()}], erlang:system_time(micro_seconds)}.

% Handle receiving of our own probe
% T is the time when the probe was created
% Nodes is a list of node PIDs in the ring
remove_probe(T, Nodes) ->
  CurrentTime = erlang:system_time(micro_seconds),
  io:format("[~p] Received my own probe - Created time: ~p - Current time: ~p - Nodes: ~p~n",
    [self(), T, CurrentTime, Nodes]).

% Handle receiving of someone else's probe
% Add ourselves to the Nodes list and forward it to our successor
% Ref is the ID of the node that sent the probe
% T is the time when the probe was created (local time at the node that created it)
% Nodes is a list of the PIDs that the probe has passed through
% Id is our own ID (key)
% Successor is our successor, on the form {key, PID}
forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Ref, lists:append(Nodes, [{Id, self()}]), T}.

% Node sends a {request, self()} message to its successor and then expects a {status, Pred} in return.
% Gets the successor's predecessor and runs stabilization
% Ring is either stable or the successor has to be notified about our existence through
% a {notify, {Id, self()}} message
% Pred: Our successor's current predecessor
% Nx: Our successor's current successor
% Id: Our Id (key)
% Successor: Our current successor, in the form {Key, Pid}
% Return {new successor, new successor's sucessor} ({Succ, Nxt})
stabilize(Pred, Nx, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      % Our successor has no predecessor
      % Inform our successor of our existence
      % Our successor does not change - adopt successors's successor as our Next node (Nx)
      Spid ! {notify, {Id, self()}},
      {Successor, Nx};
    {Id, _} ->
      % Pred points back to us, we are our successor's predecessor, don't do anything
      % Our successor does not change - adopt successors's successor as our Next node (Nx)
      {Successor, Nx};
    {Skey, _} ->
      % Pred is pointing back to itself (our successor's predecessor is our successor)
      % Inform our successor of our existence
      % Our successor does not change - adopt successors's successor as our Next node (Nx)
      Spid ! {notify, {Id, self()}},
      {Successor, Nx};
    {Xkey, Xpid} ->
      % Pred is pointing to another node
      % Should we slide ourselves between the two nodes or behind the other node (successor's predecessor)
      case key:between(Xkey, Id, Skey) of
        true ->
          % The other node is between us and our successor
          % Me - Pred - Successor
          % Adopt the other node as our successor and run stabilization again
          % Our old successor should be Pred's successor
          NewSuccessor = {Xkey, Xpid},
          NewNext = Successor,
          % TODO Should I send myself a message here or just run stabilize/1?
          % Don't think it matters ... when we receive (actually, handle) the message from ourselves the
          % successor should be set to the new successor
          self() ! stabilize,
          {NewSuccessor, NewNext};
        false ->
          % The other node is NOT between us and our successor
          % Pred should always be in front of Successor, so
          % we are in between our successor and and the other node
          % Pred - Me - Successor
          % Inform our successor of our existence
          % Our successor does not change - adopt successors's successor as our Next node (Nx)
          Spid ! {notify, {Id, self()}},
          {Successor, Nx}
      end
  end.

% Set up a timer and send a stabilize message to ourselves after a predefined interval.
% NOTE: Doing it like this so the timer doesn't have to keep track of the current successor.
schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

% Send a request message to a PID.
stabilize({_, Spid}) ->
  Spid ! {request, self()}.

% Request message received from Peer
% Inform Peer of our current predecessor and our successor
request(Peer, Predecessor, Successor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, Successor};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, Successor}
  end.

% A node has suggested that it might be our predecessor
% {Nkey, Npid} is the key and pid of the node that thinks it is our predecessor (New)
% Id is our ID (key)
% Predecessor is our current predecessor
% Return {correct predecessor, Store}
% TODO Do we need a special case to detect that we’re pointing to ourselves?
notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      % Our own predecessor is nil
      % Make the New node our predecessor, we are accepting it as our predecessor
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey,  _} ->
      % We already have a predecessor
      % Check if New should be our predecessor instead
      case key:between(Nkey, Pkey, Id) of
        true ->
          % New is between current predecessor and me
          % Predecessor - New - Me
          % New should be our predecessor, we are accepting it as our predecessor
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          % New is NOT between current predecessor and me
          % New - Predecessor - Me
          % Keep my old predecessor
          {Predecessor, Store}
      end
  end.

% Split our store based on Nkey
handover(Id, Store, Nkey, Npid) ->
  % We want to have keys that are from (Nkey, Id]
  % split(From, To, Store)
  % (From, To] - from Id, up to and including Nkey, will be in the first tuple list
  % The rest will be in the second tuple list
  % NOTE: Reversed to how I would think about it
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

printSuccessor(Node) ->
  % Node is of the form {key, pid}
  io:format("[~p] SUCCESSOR: ~p~n", [self(), Node]).

