%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2016 12:01
%%%-------------------------------------------------------------------
-module(gms3).
-author("tts").

%% API
-export([start/1, start/2, leader/4, slave/5]).

% Timeout while waiting for a join reply (invitation)
-define(timeout, 1000).
% Risk of crashing. A value of 100 means that a process will crash in average once in a hundred attempts.
-define(arghh, 50).

% Initialize a process that is the first node in a group
% Give it an empty list of peers and let it know that its master is the only node in the group
% (since it is the only node in the group it will of course be the leader of the group)
start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.
init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd), % Seed random num gen so not all processes crash at same time
  leader(Id, Master, [], [Master]).

% Initialize a node that should join an existing group.
start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.
init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd), % Seed random num gen so not all processes crash at same time
  Self = self(),
  % Send a join message to a node in the group and wait for an invitation
  % Initial state of node will be a slave
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      % Invitation received
      % Set up a monitor for the leader
      erlang:monitor(process, Leader),
      % Notify application layer master
      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group)
  after ?timeout ->
    % We didn't receive an invitation, leader may be dead
    Master ! {error, "no reply from leader"}
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
      % process identifier of its group process, Peer.
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
      % TODO Just received a new view from the leader, should I set up a monitor on it?
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      % Message received from monitor, leader is dead
      % Enter election state
      election(Id, Master, Slaves, Group);
    stop ->
      ok
  end.

% Send a message to each process in a list
%bcast(SenderId, Message, Recipients) ->
%  % TODO What to do with sender ID?
%  lists:foreach(
%    fun(Recipient) ->
%      Recipient ! Message
%    end,
%    Recipients
%  ).

% Send a message to each process in a list
% May crash after sending the message
bcast(Id, Msg, Nodes) ->
  lists:foreach(
    fun(Node) ->
      Node ! Msg,
      %io:format("[~p][~p] LEADER SENT MESSAGE TO NODE: ~p, MESSAGE: ~p~n", [Id, self(), Node, Msg]),
      crash(Id)
    end
    ,
    Nodes
  ).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ -> ok
  end.

% Elect new leader
election(Id, Master, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      % I am the first node in the list of slaves, I should be the new leader
      % Broadcast to all other slaves the new view (with me as the first node in the slave list -> leader)
      % Note that we removed the first application layer process from Group (the leader) before sending it here
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      printLeader(Id, myself),
      leader(Id, Master, Rest, Group);
    [Leader|Rest] ->
      % I am not the first node in the slave list, therefore I am not the new leader
      % Set up a monitor on the new leader
      erlang:monitor(process, Leader),
      printLeader(Id, Leader),
      slave(Id, Master, Leader, Rest, Group)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

printLeader(MyId, Leader) ->
  io:format("[~p][~p] LEADER IS: ~p~n", [MyId, self(), Leader]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%