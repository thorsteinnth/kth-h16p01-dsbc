%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2016 17:42
%%%-------------------------------------------------------------------
-module(node1).
-author("tts").

-define(Stabilize, 1000).
-define(Timeout, 10000).

%% API
-export([node/3, start/1, start/2]).

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
  schedule_stabilize(),
  node(Id, Predecessor, Successor).

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
node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      % A peer needs to know our key
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->
      % A new node informs us of its existence
      % i.e. suggesting that it might be our predecessor
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    {request, Peer} ->
      % A predecessor needs to know our predecessor
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    {status, Pred} ->
      % Our successor informs us about its predecessor
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      % Send a request message to our successor.
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    stop ->
      ok;
    _ ->
      io:format("Unknown message type")
  end.

% Node sends a {request, self()} message to its successor and then expects a {status, Pred} in return.
% Gets the successor's predecessor and runs stabilization
% Ring is either stable or the successor has to be notified about our existence through
% a {notify, {Id, self()}} message
% Pred: Our successor's current predecessor
% Id: Our Id (key)
% Successor: Our current successor, in the form {Key, Pid}
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      % Inform our successor of our existence
      Spid ! {notify, {Id, self()}};
    {Id, _} ->
      % Pred points back to us, don't do anything
      ok;
    {Skey, _} ->
      % Pred is pointing back to itself (our successor's predecessor is our successor)
      % Inform our successor of our existence
      Spid ! {notify, {Id, self()}};
    {Xkey, Xpid} ->
      % Pred is pointing to another node
      % Should we slide ourselves between the two nodes or behind the other node (successor's predecessor)
      case key:between(Xkey, Id, Skey) of
        true ->
          % The other node is between us and our successor
          % Me - Pred - Successor
          % Adopt the other node as our successor and run stabilization again
          NewSuccessor = {Xkey, Xpid},
          stabilize(Pred, Id, NewSuccessor);
        false ->
          % The other node is NOT between us and our successor
          % Pred should always be in front of Successor, so
          % we are in between our successor and and the other node
          % Pred - Me - Successor
          % Inform our successor of our existence
          Spid ! {notify, {Id, self()}}
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