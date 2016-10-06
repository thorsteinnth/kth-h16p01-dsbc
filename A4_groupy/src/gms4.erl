%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2016 16:49
%%%-------------------------------------------------------------------
-module(gms4).
-author("tts").

%% API
-export([start/1, start/2, leader/6, slave/7]).

% Timeout while waiting for a join reply (invitation)
-define(timeout, 1000).
% Risk of crashing. A value of 100 means that a process will crash in average once in a hundred attempts.
-define(arghh, 50).
%-define(arghh, 200). % Useful for testing the running group (joins after deaths)
-define(messageLossArghh, 10).

% Let's keep sent-but-not-acked messages in an OutgoingQueue
% a list of {{RecipientPid, SeqNum}, Message}

% Initialize a process that is the first node in a group
% Give it an empty list of peers and let it know that its master is the only node in the group
% (since it is the only node in the group it will of course be the leader of the group)
start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.
init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd), % Seed random num gen so not all processes crash at same time
  leader(Id, Master, 0, [], [Master], []).

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
      %printMessage(Id, {view, NewN, [Leader|Slaves], Group}),
      sendAckToLeader(Leader, NewN),
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
% OutgoingQueue: A list of sent-but-not-acked messages
% TODO The assignment has the extended function signature as leader(Id, Master, N, Slaves), Group missing
% TODO Probably not correct, they say extend the function, not change it
leader(Id, Master, N, Slaves, Group, OutgoingQueue) ->
  receive
    {mcast, Msg} ->
      % Message either from its own master or from a peer node.
      % A message {msg, Msg} is multicasted to all peers and a message Msg is sent to the application layer.
      %io:format("[~p][~p] LEADER BROADCASTING MCAST MESSAGE WITH SEQNUM: ~p TO: ~p~n", [Id, self(), N, Slaves]),
      resendMessagesInOutgoingQueue(OutgoingQueue),
      NewOutgoingQueue = bcast(Id, {msg, N, Msg}, Slaves, OutgoingQueue),  % Send a message to each of the processes in a list
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group, NewOutgoingQueue);
    {join, Wrk, Peer} ->
      % Message, from a peer or the master, that is a request from a node to join the group.
      % The message contains both the process identifier of the application layer, Wrk, and the
      % process identifier of its group process, Peer.
      resendMessagesInOutgoingQueue(OutgoingQueue),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      %io:format("[~p][~p] LEADER BROADCASTING VIEW MESSAGE WITH SEQNUM: ~p TO: ~p~n", [Id, self(), N, Slaves2]),
      NewOutgoingQueue = bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2, OutgoingQueue),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2, NewOutgoingQueue);
    {ack, {AckingPID, AckedSeqNum}} ->
      %io:format("[~p][~p] LEADER RECEIVING ACK FROM ~p FOR SEQNUM ~p~n", [Id, self(), AckingPID, AckedSeqNum]),
      NewOutgoingQueue = removeAckedMessageFromOutgoingQueue(AckingPID, AckedSeqNum, OutgoingQueue),
      leader(Id, Master, N, Slaves, Group, NewOutgoingQueue);
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

    % FROM MASTER
    {mcast, Msg} ->
      % a request from its master to multicast a message, the message is forwarded to the leader.
      % message from application layer, not from leader, no seqnum
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      % a request from the master to allow a new node to join the group, the message is forwarded to the leader.
      % message from application layer, not from leader, no seqnum
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    % FROM LEADER
    {msg, I, Msg} when I =< N ->
      % Discard messages that we have already seen (duplicates)
      %io:format("[~p][~p] DISCARDING MESSAGE WITH SEQNUM: ~p, CURRENT SEQNUM: ~p, MESSAGE: ~p~n", [Id, self(), I, N, Msg]),
      %printMessage(Id, {msg, I, Msg}),
      ShouldLoseMessage = simulateMessageLoss(),
      if
        ShouldLoseMessage ->
          io:format("[~p][~p] SIMULATING MESSAGE LOSS, MESSAGE: ~p~n", [Id, self(), {msg, I, Msg}]),
          slave(Id, Master, Leader, N, Last, Slaves, Group);
        true ->
          sendAckToLeader(Leader, I),
          slave(Id, Master, Leader, N, Last, Slaves, Group)
      end;
    {msg, NewN, Msg} ->
      % a multicasted message from the leader. A message Msg is sent to the master.
      %printMessage(Id, {msg, NewN, Msg}),
      ShouldLoseMessage = simulateMessageLoss(),
      if
        ShouldLoseMessage ->
          io:format("[~p][~p] SIMULATING MESSAGE LOSS, MESSAGE: ~p~n", [Id, self(), {msg, NewN, Msg}]),
          slave(Id, Master, Leader, N, Last, Slaves, Group);
        true ->
          sendAckToLeader(Leader, NewN),
          Master ! Msg,
          % Update current seqnum and update the latest message received from leader
          slave(Id, Master, Leader, NewN, {msg, NewN, Msg}, Slaves, Group)
      end;
    {view, I, [Leader|Slaves2], Group2} when I =< N ->
      % Discard messages that we have already seen (duplicates)
      %io:format("[~p][~p] DISCARDING MESSAGE WITH SEQNUM: ~p, CURRENT SEQNUM: ~p, MESSAGE: ~p~n", [Id, self(), I, N, {view, I, [Leader|Slaves2], Group2}]),
      %printMessage(Id, {view, I, [Leader|Slaves2], Group2}),
      ShouldLoseMessage = simulateMessageLoss(),
      if
        ShouldLoseMessage ->
          io:format("[~p][~p] SIMULATING MESSAGE LOSS, MESSAGE: ~p~n", [Id, self(), {view, I, [Leader|Slaves2], Group2}]),
          slave(Id, Master, Leader, N, Last, Slaves, Group);
        true ->
          sendAckToLeader(Leader, I),
          slave(Id, Master, Leader, N, Last, Slaves, Group)
      end;
    {view, NewN, [Leader|Slaves2], Group2} ->
      % {view, Peers, Group}
      % a multicasted view from the leader. A view is delivered to the master process.
      %printMessage(Id, {view, NewN, [Leader|Slaves2], Group2}),
      ShouldLoseMessage = simulateMessageLoss(),
      if
        ShouldLoseMessage ->
          io:format("[~p][~p] SIMULATING MESSAGE LOSS, MESSAGE: ~p~n", [Id, self(), {view, NewN, [Leader|Slaves2], Group2}]),
          slave(Id, Master, Leader, N, Last, Slaves, Group);
        true ->
          sendAckToLeader(Leader, NewN),
          % Update current seqnum and update the latest message received from leader
          Master ! {view, Group2},
          slave(Id, Master, Leader, NewN, {view, NewN, [Leader|Slaves2], Group2}, Slaves2, Group2)
      end;

    % FROM MONITOR
    {'DOWN', _Ref, process, Leader, _Reason} ->
      % Message received from monitor, leader is dead
      % Enter election state
      election(Id, Master, N, Last, Slaves, Group);

    stop ->
      ok

  end.

sendAckToLeader(Leader, NToAck) ->
  %io:format("[~p] WILL SEND ACK FOR SEQNUM ~p TO LEADER: ~p~n", [self(), NToAck, Leader]),
  Leader ! {ack, {self(), NToAck}}.

% Send a message to each process in a list
%bcast(SenderId, Message, Recipients) ->
%  % TODO What to do with sender ID?
%  lists:foreach(
%    fun(Recipient) ->
%      Recipient ! Message
%    end,
%    Recipients
%  ).

%% Send a message to each process in a list
%% May crash after sending the message
%bcast(Id, Msg, Nodes) ->
%  lists:foreach(
%    fun(Node) ->
%      Node ! Msg,
%      %io:format("[~p][~p] LEADER SENT MESSAGE TO NODE: ~p, MESSAGE: ~p~n", [Id, self(), Node, Msg]),
%      crash(Id)
%    end
%    ,
%    Nodes
%  ).

% Send a message to each process in a list
% May crash after sending the message
% Return new OutgoingQueue, list of {{RecipientPid, SeqNum}, Message}, where all outgoing messages have been added
bcast(Id, Msg, Nodes, OutgoingQueue) ->
  NewOutgoingQueue = lists:foldl(
    fun(Node, Accumulator) ->
      Node ! Msg,
      %io:format("[~p][~p] LEADER SENT MESSAGE TO NODE: ~p, MESSAGE: ~p~n", [Id, self(), Node, Msg]),
      case Msg of
        {_, N, _} ->
          % {msg, N, Msg}
          NewAccumulator = lists:append(Accumulator, [{{Node, N}, Msg}]);
        {_, N, _, _} ->
          % {view, N, Peers, Group}
          NewAccumulator = lists:append(Accumulator, [{{Node, N}, Msg}]);
        _ ->
          io:format("gms4:bcast/4 - Add message to OutgoingQueue error - unknown msg format", []),
          NewAccumulator = Accumulator
      end,
      crash(Id),
      NewAccumulator
    end,
    OutgoingQueue,
    Nodes
  ),
  %io:format("[~p][~p] BCAST NEW OUTGOING QUEUE: ~p~n", [Id, self(), NewOutgoingQueue]),
  NewOutgoingQueue.

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      % TODO Re-add crashing after testing missing messages
      %io:format("leader ~w: crash~n", [Id]),
      %exit(no_luck);
      ok;
    _ -> ok
  end.

simulateMessageLoss() ->
  case random:uniform(?messageLossArghh) of
    ?messageLossArghh ->
      % Message should be lost
      true;
    _ ->
      false
  end.

removeAckedMessageFromOutgoingQueue(AckingPid, AckedSeqNum, OutgoingQueue) ->
  % The queue is of the form {{RecipientPid, SeqNum}, Message}
  lists:keydelete({AckingPid, AckedSeqNum}, 1, OutgoingQueue).

resendMessagesInOutgoingQueue([]) ->
  ok;
resendMessagesInOutgoingQueue([FirstEntryInOutgoingQueue | Rest]) ->
  % The queue is of the form {{RecipientPid, SeqNum}, Message}
  {{RecipientPid, _}, Message} = FirstEntryInOutgoingQueue,
  io:format("[~p] RESENDING MESSAGE IN QUEUE TO NODE: ~p, MESSAGE: ~p~n", [self(), RecipientPid, Message]),
  RecipientPid ! Message,
  resendMessagesInOutgoingQueue(Rest).

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
      NewOutgoingQueue = bcast(Id, Last, Rest, []),  % Seqnum of the message Last should be N
      % Broadcast to all other slaves the new view (with me as the first node in the slave list -> leader)
      % Note that we removed the first application layer process from Group (the leader) before sending it here
      NewN = N+1, % Increment seqnum, this is a new message from the leader (me)
      NewOutgoingQueue2 = bcast(Id, {view, NewN, Slaves, Group}, Rest, NewOutgoingQueue),
      Master ! {view, Group},
      leader(Id, Master, NewN+1, Rest, Group, NewOutgoingQueue2); % Start the leader loop with a new seqnum
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

printOutgoingQueue(MyId, OutgoingQueue) ->
  io:format("[~p][~p] LEADER OUTGOING QUEUE: ~p~n", [MyId, self(), OutgoingQueue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%