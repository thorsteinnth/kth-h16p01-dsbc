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

%% API
-export([node/3]).

% First implementation that only handles a growing ring.

% Predecessor and successor are of the form {Key, Pid}
node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      % A peer needs to know our key
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->
      % A new node informs us of its existence
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
% Successor: Our current successor, on the form {Key, Pid}
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      % Inform our successor of our existence
      something;
    {Id, _} ->
      % Pred points back to us, don't do anything
      something;
    {Skey, _} ->
      % Pred is pointing back to itself (our successor's predecessor is our successor)
      % Inform our successor of our existence
      something;
    {Xkey, Xpid} ->
      % Pred is pointing to another node
      % Should we slide ourselves between the two nodes or in front of the other node (successor's predecessor)
      case key:between(Xkey, Id, Skey) of
        true ->
          % The other node is between us and our successor
          % Adopt the other node as our successor and run stabilization again
          something;
        false ->
          % The other node is not between us and our successor
          % We are in between our successor and and the other node
          % Inform our successor of our existence
          something
      end
  end.
