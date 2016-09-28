%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2016 15:50
%%%-------------------------------------------------------------------
-module(time).
-author("tts").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2,
  testClock/0, testUpdate/0, testSafe/0]).

% Return an initial Lamport value (could it be 0)
zero() ->
  0.

% Return the time T incremented by one (you will probably ignore the Name but we will use it later)
inc(Name, T) ->
  T+1.

% Merge the two Lamport time stamps (i.e. take the maximum value)
merge(Ti, Tj) ->
  erlang:max(Ti, Tj).

% True if Ti is less than or equal to Tj
leq(Ti,Tj) ->
  if
    Ti =< Tj -> true;
    true -> false
  end.

% The logger should have a clock that keeps track of the timestamps of the last messages seen from each of the workers.
% Return a clock that can keep track of the nodes
clock(Nodes) ->
  % Return a list of length length(Nodes) with tuple elements {node, time}, initialized as {node, 0}
  Clock = lists:foldl(
    fun(Element, Accumulator) ->
      lists:append(Accumulator, [{Element, 0}])
    end,
    [],
    Nodes
  ),
  Clock.

% Return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
  lists:keyreplace(Node, 1, Clock, {Node, Time}).

% Is it safe to log an event that happened at a given time, true or false
safe(Time, Clock) ->
  timeLessThanOrEqualToAllClockTimes(Time, Clock).

timeLessThanOrEqualToAllClockTimes(_, []) ->
  % Made it to the end of the Clock list, so Time is less than or equal to all clock times
  true;
timeLessThanOrEqualToAllClockTimes(Time, Clock) ->
  [FirstClockTimeTuple | RestOfClockTimeTuples] = Clock,
  {_,FirstClockTime} = FirstClockTimeTuple,
  LessThanOrEqual = leq(Time-1, FirstClockTime), % Can log time 12 when I have received time 11 from all -> (12-1) <= 11
  if
    LessThanOrEqual ->
      % Time is less than or equal to this clock time, moving on to the next one
      timeLessThanOrEqualToAllClockTimes(Time, RestOfClockTimeTuples);
    true ->
      % Time is greater than this clock time, therefore it is not less than or equal to all clock times
      false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testClock() ->
  Nodes = [node1, node2, node3, node4],
  clock(Nodes).

testUpdate() ->
  Clock = clock([node1, node2, node3, node4]),
  update(node3, 90, Clock).

testSafe() ->
  Clock = clock([node1, node2, node3, node4]),
  Clock1 = update(node1, 10, Clock),
  Clock2 = update(node2, 20, Clock1),
  Clock3 = update(node3, 30, Clock2),
  Clock4 = update(node4, 40, Clock3),
  Is9Safe = safe(9, Clock4),
  Is10Safe = safe(10, Clock4),
  Is11Safe = safe(11, Clock4),
  Is50Safe = safe(50, Clock4),
  io:format("9 safe ~p,~n10 safe ~p,~n11 safe ~p,~n50 safe ~p~n", [Is9Safe, Is10Safe, Is11Safe, Is50Safe]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%