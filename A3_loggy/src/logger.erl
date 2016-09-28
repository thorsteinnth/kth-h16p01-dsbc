%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2016 20:35
%%%-------------------------------------------------------------------
-module(logger).
-author("tts").

%% API
-export([start/1, stop/1,
  testQueue/0, testLogSafeLogRequestsFromQueue/0]).

% The logger should have a clock that keeps track of the timestamps of the last messages seen from each of the workers.
% It should also have a hold-back queue where it keeps log messages that are still not safe to print.
% When a new log message arrives it should update the clock, add the message to the hold-back queue and then go through
% the queue to find messages that are now safe to print.

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  Queue = newQueue(),
  Clock = time:clock(Nodes),
  loop(Queue, Clock).

loop(Queue, Clock) ->
  %printClock(Clock),
  receive
    {log, From, Time, Msg} ->
      % Update entry in clock with new time
      NewClock = time:update(From, Time, Clock),
      % Add message to queue
      NewQueue = addLogRequestToQueueAndSort({log, From, Time, Msg}, Queue),
      % Log safe messages from queue
      NewAndUpdatedQueue = logSafeLogRequestsFromQueue(NewQueue, NewClock),
      loop(NewAndUpdatedQueue, NewClock);
    stop ->
      io:format("Stopping logger, will log remaining queue~n", []),
      printQueue(Queue),
      logMessagesInQueue(Queue),
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

newQueue() ->
  [].

% LogRequests are of the form {log, From, Time, Msg}
% with Msg expanded {log, Name, Time, {sending, Message}}
% where Message is of the form {hello, random:uniform(100)}
% {log, Name, Time, {sending, {hello, random:uniform(100)}}}
addLogRequestToQueueAndSort(LogRequest, Queue) ->
  NewQueue = lists:append(Queue, [LogRequest]),
  SortedQueue = lists:keysort(3, NewQueue),
  SortedQueue.

% Go through queue and and log messages that are safe to print
% Return new queue where safe log requests have been removed.
logSafeLogRequestsFromQueue(Queue, Clock) ->
  logLogRequestFromQueueIfSafe(Queue, Clock),
  removeSafeLogRequestsFromQueue(Queue, Clock).

logLogRequestFromQueueIfSafe([], _) ->
  % We have logged all messages we can log
  ok;
logLogRequestFromQueueIfSafe(RemainingQueue, Clock) ->
  [FirstLogRequest | RestOfLogRequests] = RemainingQueue,
  % LogRequests are of the form {log, From, Time, Msg}
  {log, From, Time, Msg} = FirstLogRequest,
  SafeToLog = time:safe(Time, Clock),
  if
    SafeToLog ->
      % Let's log the request
      log(From, Time, Msg),
      % Let's move on to the next request in the queue
      logLogRequestFromQueueIfSafe(RestOfLogRequests, Clock);
    true ->
      % Not safe to log, reached the end of what is safe to log (since the queue is ordered)
      % The RemainingQueue (what we have yet to check) is now empty
      % Stop here.
      logLogRequestFromQueueIfSafe([], Clock)
  end,
  ok.

% NOTE: Was having trouble building a list or removing from a list in the logLogRequestFromQueueIfSafe
% so I split this into two different functions instead
removeSafeLogRequestsFromQueue(Queue, Clock) ->
  UpdatedQueue = lists:foldl(
    fun(LogRequestElement, Accumulator) ->
      LogRequestIsSafe = isLogRequestSafe(LogRequestElement, Clock),
      if
        LogRequestIsSafe ->
          Accumulator;
        true ->
          lists:append(Accumulator, [LogRequestElement])
      end
    end,
    [],
    Queue
  ),
  UpdatedQueue.

isLogRequestSafe(LogRequest, Clock) ->
  % LogRequests are of the form {log, From, Time, Msg}
  {log, _, Time, _} = LogRequest,
  time:safe(Time, Clock).

% Log messages in queue (no matter if they are safe or not)
% Return an empty queue (we have logged all messages in the queue)
logMessagesInQueue([]) ->
  [];
logMessagesInQueue(Queue) ->
  [FirstElement | RestOfElements] = Queue,
  {log, From, Time, Msg} = FirstElement,
  log(From, Time, Msg),
  logMessagesInQueue(RestOfElements).

printClock(Clock) ->
  io:format("Clock: ~p~n", [Clock]).

printQueue(Queue) ->
  io:format("Queue: ~p~n", [Queue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testPrintQueue() ->
  printQueue([]).

testQueue() ->
  Queue = newQueue(),
  Queue1 = addLogRequestToQueueAndSort({log, john, 4, {sending, {hello, 9999}}}, Queue),
  Queue2 = addLogRequestToQueueAndSort({log, paul, 3, {sending, {hello, 9999}}}, Queue1),
  Queue3 = addLogRequestToQueueAndSort({log, ringo, 1, {sending, {hello, 9999}}}, Queue2),
  Queue4 = addLogRequestToQueueAndSort({log, george, 2, {sending, {hello, 9999}}}, Queue3),
  printQueue(Queue4).

testLogSafeLogRequestsFromQueue() ->
  Clock = time:clock([node1, node2, node3, node4]),
  Clock1 = time:update(node1, 2, Clock),
  Clock2 = time:update(node2, 2, Clock1),
  Clock3 = time:update(node3, 3, Clock2),
  Clock4 = time:update(node4, 4, Clock3),
  Queue = newQueue(),
  Queue1 = addLogRequestToQueueAndSort({log, john, 1, {sending, {hello, 9999}}}, Queue),
  Queue2 = addLogRequestToQueueAndSort({log, paul, 2, {sending, {hello, 9999}}}, Queue1),
  Queue3 = addLogRequestToQueueAndSort({log, ringo, 3, {sending, {hello, 9999}}}, Queue2),
  Queue4 = addLogRequestToQueueAndSort({log, george, 4, {sending, {hello, 9999}}}, Queue3),
  UpdatedQueue = logSafeLogRequestsFromQueue(Queue4, Clock4),
  io:format("Updated queue: ~p~n", [UpdatedQueue]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%