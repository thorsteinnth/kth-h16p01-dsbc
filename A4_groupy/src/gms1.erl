%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2016 15:36
%%%-------------------------------------------------------------------
-module(gms1).
-author("tts").

%% API
-export([]).

% Leader process
% Id: a unique name, of the node, only used for debugging
% Master: the process identifier of the application layer
% Slaves: an ordered list of the process identifiers of all slaves in the group (ordered by join time)
% Group: a list of all application layer processes in the group
leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      % Message either from its own master or from a peer node.
      % A message {msg, Msg} is multicasted to all peers and a message Msg is sent to the application layer.
      bcast(Id, {msg, Msg}, Slaves),  % Send a message to each of the processes in a list
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->
      % Message, from a peer or the master, that is a request from a node to join the group.
      % The message contains both the process identifier of the application layer, Wrk, and the
      % process identifier of its group process.
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
    stop ->
      ok
  end.