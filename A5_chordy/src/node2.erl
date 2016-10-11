%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 20:43
%%%-------------------------------------------------------------------
-module(node2).
-author("tts").

-define(Stabilize, 1000).
-define(Timeout, 10000).

%% API
-export([node/4, start/1, start/2]).

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
  node(Id, Predecessor, Successor, storage:create()).

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
node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      % A peer needs to know our key
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    {notify, New} ->
      % A new node informs us of its existence
      % i.e. suggesting that it might be our predecessor
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor, Store);
    {request, Peer} ->
      % A predecessor needs to know our predecessor
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
      % Our successor informs us about its predecessor
      Succ = stabilize(Pred, Id, Successor),
      %io:format("[~p] STABILIZE/3 JUST FINISHED, NEW SUCCESSOR: ~p~n", [self(), Succ]),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
      % Send a request message to our successor.
      %io:format("[~p] WILL ENTER STABILIZE/1 WITH SUCCESSOR ARG AS: ~p~n", [self(), Successor]),
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);
    probe ->
      % We should send a probe
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      % We just received our own probe, let's remove it (and log it)
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      % We just received a prove from another node, let's forward it to our successor
      % (and add ourselves to the Nodes list)
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      % Add key and value to store
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      % Lookup key in store
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
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
% Id: Our Id (key)
% Successor: Our current successor, in the form {Key, Pid}
% Return the new successor
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      % Our successor has no predecessor
      % Inform our successor of our existence
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
      % Pred points back to us, we are our successor's predecessor, don't do anything
      Successor;
    {Skey, _} ->
      % Pred is pointing back to itself (our successor's predecessor is our successor)
      % Inform our successor of our existence
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      % Pred is pointing to another node
      % Should we slide ourselves between the two nodes or behind the other node (successor's predecessor)
      case key:between(Xkey, Id, Skey) of
        true ->
          % The other node is between us and our successor
          % Me - Pred - Successor
          % Adopt the other node as our successor and run stabilization again
          NewSuccessor = {Xkey, Xpid},
          % TODO Should I send myself a message here or just run stabilize/1?
          % Don't think it matters ... when we receive (actually, handle) the message from ourselves the
          % successor should be set to the new successor
          self() ! stabilize,
          NewSuccessor;
        false ->
          % The other node is NOT between us and our successor
          % Pred should always be in front of Successor, so
          % we are in between our successor and and the other node
          % Pred - Me - Successor
          % Inform our successor of our existence
          Spid ! {notify, {Id, self()}},
          Successor
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
% Inform Peer of our current predecessor
request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

% A node has suggested that it might be our predecessor
% {Nkey, Npid} is the key and pid of the node that thinks it is our predecessor (New)
% Id is our ID (key)
% Predecessor is our current predecessor
% Return the correct predecessor
% TODO Do we need a special case to detect that we’re pointing to ourselves?
notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      % Our own predecessor is nil
      % Make the New node our predecessor (i.e. return it)
      {Nkey, Npid};
    {Pkey,  _} ->
      % We already have a predecessor
      % Check if New should be our predecessor instead
      case key:between(Nkey, Pkey, Id) of
        true ->
          % New is between current predecessor and me
          % Predecessor - New - Me
          % New should be our predecessor
          {Nkey, Npid};
        false ->
          % New is NOT between current predecessor and me
          % New - Predecessor - Me
          % Keep my old predecessor
          Predecessor
      end
  end.

printSuccessor(Node) ->
  % Node is of the form {key, pid}
  io:format("[~p] SUCCESSOR: ~p~n", [self(), Node]).
