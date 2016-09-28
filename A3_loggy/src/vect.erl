%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 12:14
%%%-------------------------------------------------------------------
-module(vect).
-author("tts").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2,
  testInc/0, testMerge/0]).

% Let's represent the vector clock like this:
% [{john, 3}, {ringo, 2}, {paul, 4}, {george, 1}]

% Return an initial vector clock
zero() ->
  [].

% Return the Time with the entry for Name incremented by one
inc(Name, Time) ->
  FoundTuple = lists:keyfind(Name, 1, Time),
  if
    FoundTuple == false ->
      % Entry for Name was not found in Time, let's add it
      [{Name, 1}|Time];
    true ->
      {Name, CurrentTime} = FoundTuple,
      lists:keyreplace(Name, 1, Time, {Name, CurrentTime+1})
  end.

% Merge the two vector clocks.
% If P receives a message with vector time stamp T, it forms the maximum of the new version (doesn't actually matter
% if we increment before merge or not) of V (P's vector) and T, component-by-component
merge(Ti, Tj) ->
  componentMaxMerge(Ti, Tj).

componentMaxMerge([], Time) ->
  Time;
componentMaxMerge([{Name, IncomingTime} | Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, CurrentTime} ->
      % Found the entry with Name in the incoming time in my own Time
      % Update the value for that Name and carry on merging the rest of the incoming Time and my own Time
      % (with the entry for Name deleted)
      [{Name, erlang:max(CurrentTime, IncomingTime)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      % Did not find the entry for Name in my own Time, let's add it
      [{Name, IncomingTime} | merge(Rest, Time)]
end.

%----------------------------------------------------------------------

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
  lists:keyreplace(Node, 1, Clock, {Node, Time}). % TODO Does this have to be max+1?

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

printTime(Time) ->
  io:format("Time: ~p~n", [Time]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testInc() ->
  Time = zero(),
  Time1 = inc(john, Time),
  Time2 = inc(ringo, Time1),
  Time3 = inc(paul, Time2),
  Time4 = inc(george, Time3),
  Time5 = inc(ringo, Time4),
  Time6 = inc(ringo, Time5),
  printTime(Time6).

testMerge() ->
  Time = zero(),
  Time1 = inc(john, Time),
  Time2 = inc(ringo, Time1),
  Time3 = inc(ringo, Time2),
  Time4 = inc(ringo, Time3),
  Time5 = inc(paul, Time4),
  Time6 = inc(george, Time5),
  Time7 = inc(george, Time6),
  printTime(Time7),
  ExtTime = zero(),
  ExtTime1 = inc(john, ExtTime),
  ExtTime2 = inc(john, ExtTime1),
  ExtTime3 = inc(ringo, ExtTime2),
  ExtTime4 = inc(ringo, ExtTime3),
  ExtTime5 = inc(paul, ExtTime4),
  ExtTime6 = inc(george, ExtTime5),
  printTime(ExtTime6),
  MergedTime = merge(Time7, ExtTime6),
  printTime(MergedTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%