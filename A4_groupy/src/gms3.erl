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
-export([start/1, start/2, leader/5, slave/7]).

% Timeout while waiting for a join reply (invitation)
-define(timeout, 1000).
% Risk of crashing. A value of 100 means that a process will crash in average once in a hundred attempts.
-define(arghh, 50).
%-define(arghh, 200). % Useful for testing the running group (joins after deaths)

% Initialize a process that is the first node in a group
% Give it an empty list of peers and let it know that its master is the only node in the group
% (since it is the only node in the group it will of course be the leader of the group)
start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.
init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd), % Seed random num gen so not all processes crash at same time
  leader(Id, Master, 0, [], [Master]).

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
    {view, NewN, [Leader|Slaves], Group} ->
      % Invitation received
      % Set up a monitor for the leader
      erlang:monitor(process, Leader),
      % Notify application layer master
      Master ! {view, Group},
      slave(Id, Master, Leader, NewN, {view, NewN, [Leader|Slaves], Group}, Slaves, Group)
  after ?timeout ->
    % We didn't receive an invitation, leader may be dead
    Master ! {error, "no reply from leader"}
  end.

% Leader process
% Id: a unique name, of the node, only used for debugging
% Master: the process identifier of the application layer
% N: sequence number of the next message (regular message or view) to be sent
% Slaves: an ordered list of the process identifiers of all slaves in the group (ordered by join time)
% Group: a list of all application layer processes in the group
% TODO The assignment has the extended function signature as leader(Id, Master, N, Slaves), Group missing
% TODO Probably not correct, they say extend the function, not change it
leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      % Message either from its own master or from a peer node.
      % A message {msg, Msg} is multicasted to all peers and a message Msg is sent to the application layer.
      bcast(Id, {msg, N, Msg}, Slaves),  % Send a message to each of the processes in a list
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group);
    {join, Wrk, Peer} ->
      % Message, from a peer or the master, that is a request from a node to join the group.
      % The message contains both the process identifier of the application layer, Wrk, and the
      % process identifier of its group process, Peer.
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2);
    stop ->
      ok
  end.

% Slave process
% Id: a unique name, of the node, only used for debugging
% Master: the process identifier of the application layer
% Leader: the leader, must keep track of the leader
% N: expected sequence number of the next message
% Last: copy of last message (a regular message or a view) received from the leader
% Slaves: an ordered list of the process identifiers of all slaves in the group (ordered by join time)
% Group: a list of all application layer processes in the group
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      % FROM MASTER
      % a request from its master to multicast a message, the message is forwarded to the leader.
      % message from application layer, not from leader, no seqnum
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      % FROM MASTER
      % a request from the master to allow a new node to join the group, the message is forwarded to the leader.
      % message from application layer, not from leader, no seqnum
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, Msg} when I =< N ->
      % FROM LEADER
      % Discard messages that we have already seen (duplicates)
      %io:format("[~p][~p] DISCARDING MESSAGE WITH SEQNUM: ~p, CURRENT SEQNUM: ~p, MESSAGE: ~p~n", [Id, self(), I, N, Msg]),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, NewN, Msg} ->
      % FROM LEADER
      % a multicasted message from the leader. A message Msg is sent to the master.
      %printMessage(Id, {msg, NewN, Msg}),
      Master ! Msg,
      % Update current seqnum and update the latest message received from leader
      slave(Id, Master, Leader, NewN, {msg, NewN, Msg}, Slaves, Group);
    {view, I, [Leader|Slaves2], Group2} when I =< N ->
      % FROM LEADER
      % Discard messages that we have already seen (duplicates)
      %io:format("[~p][~p] DISCARDING MESSAGE WITH SEQNUM: ~p, CURRENT SEQNUM: ~p, MESSAGE: ~p~n", [Id, self(), I, N, {view, I, [Leader|Slaves2], Group2}]),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, NewN, [Leader|Slaves2], Group2} ->
      %FROM LEADER
      % {view, Peers, Group}
      % a multicasted view from the leader. A view is delivered to the master process.
      %printMessage(Id, {view, NewN, [Leader|Slaves2], Group2}),
      % TODO Just received a new view from the leader, should I set up a monitor on it?
      % Update current seqnum and update the latest message received from leader
      Master ! {view, Group2},
      slave(Id, Master, Leader, NewN, {view, NewN, [Leader|Slaves2], Group2}, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      % Message received from monitor, leader is dead
      % Enter election state
      election(Id, Master, N, Last, Slaves, Group);
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
% N: expected sequence number of the next message
% Last: copy of last message (a regular message or a view) received from the leader
% TODO The assignment has the extended function signature as election(Id, Master, N, Last, Slaves, Group)
% TODO Probably not correct, they say extend the function, not change it
election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      % I am the first node in the list of slaves, I should be the new leader
      printLeader(Id, myself),
      % Forward the last received message to all peers in the group (in case they didn't receive it)
      io:format("[~p][~p] NEW LEADER BROADCASTING LAST RECEIVED MSG: ~p~n", [Id, self(), Last]),
      bcast(Id, Last, Rest),  % Seqnum of the message Last should be N
      % Broadcast to all other slaves the new view (with me as the first node in the slave list -> leader)
      % Note that we removed the first application layer process from Group (the leader) before sending it here
      NewN = N+1, % Increment seqnum, this is a new message from the leader (me)
      bcast(Id, {view, NewN, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, NewN+1 , Rest, Group); % Start the leader loop with a new seqnum
    [Leader|Rest] ->
      % I am not the first node in the slave list, therefore I am not the new leader
      printLeader(Id, Leader),
      % Set up a monitor on the new leader
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

printLeader(MyId, Leader) ->
  io:format("[~p][~p] LEADER IS: ~p~n", [MyId, self(), Leader]).

printMessage(MyId, Msg) ->
  io:format("[~p][~p] RECEIVED MESSAGE: ~p~n", [MyId, self(), Msg]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%