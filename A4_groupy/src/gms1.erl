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
-export([start/1, start/2, leader/4, slave/5]).

% Initialize a process that is the first node in a group
% Give it an empty list of peers and let it know that its master is the only node in the group
% (since it is the only node in the group it will of course be the leader of the group)
start(Id) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Self) end)}.
init(Id, Master) ->
  leader(Id, Master, [], [Master]).

% Initialize a node that should join an existing group.
start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.
init(Id, Grp, Master) ->
  Self = self(),
  % Send a join message to a node in the group and wait for an invitation
  % Initial state of node will be a slave
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      % Invitation received
      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group)
  end.

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

% Slave process
% Id: a unique name, of the node, only used for debugging
% Master: the process identifier of the application layer
% Leader: the leader, must keep track of the leader
% Slaves: an ordered list of the process identifiers of all slaves in the group (ordered by join time)
% Group: a list of all application layer processes in the group
slave(Id, Master, Leader, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      % a request from its master to multicast a message, the message is forwarded to the leader.
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->
      % a request from the master to allow a new node to join the group, the message is forwarded to the leader.
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->
      % a multicasted message from the leader. A message Msg is sent to the master.
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader|Slaves2], Group2} ->
      % {view, Peers, Group}
      % a multicasted view from the leader. A view is delivered to the master process.
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    stop ->
      ok
  end.

% Send a message to each process in a list
bcast(SenderId, Message, Recipients) ->
  % TODO What to do with sender ID?
  lists:foreach(
    fun(Recipient) ->
      Recipient ! Message
    end,
    Recipients
  ).