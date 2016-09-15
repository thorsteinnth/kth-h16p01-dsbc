%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2016 15:37
%%%-------------------------------------------------------------------
-module(map).
-author("tts").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1,
  testUpdate/0, testReachable/0]).

% NOTES:
% Directional map where you should easily be able to update the map and find nodes directly
% connected to a given node.
% List of entries where each entry consists of a city with a list of directly connected cities.

% Returns an empty map (an empty list)
new() ->
  [].

% Update the map to reflect that Node has directional links to all nodes in the list Links.
% The old entry is removed.
update(Node, Links, Map) ->
  % Remove old entry
  % lists:keydelete(Key, N, TupleList1) -> TupleList2:
  % Returns a copy of TupleList1 where the first occurrence of a tuple whose Nth element compares equal to Key
  % is deleted, if there is such a tuple.
  MapDeleted = lists:keydelete(Node, 1, Map),
  % Append new entry
  MapAppended = lists:append(MapDeleted, [{Node, Links}]),
  MapAppended.

% Returns the list of nodes directly reachable from Node.
reachable(Node, Map) ->
  % Find Node in Map
  % keyfind(Key, N, TupleList) -> Tuple | false
  % Searches the list of tuples TupleList for a tuple whose Nth element compares equal to Key.
  % Returns Tuple if such a tuple is found, otherwise false.
  FoundTuple = lists:keyfind(Node, 1, Map),
  if
    FoundTuple == false -> [];
    true ->
      {_, ReachableDestinations} = FoundTuple,
      ReachableDestinations
  end.

% Returns a list of all nodes in the map, also the ones without outgoing links.
% So if berlin is is linked to london but london does not have any outgoing links (and thus no entry in the list),
% london should still be in the returned list.
all_nodes(Map) ->
  ok.

printMap([]) ->
  io:format("[~p]map:printMap/1: []~n", [self()]),
  ok;
printMap(Map) ->
  io:format("[~p]map:printMap/1: ~p~n", [self(), Map]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testUpdate() ->
  Map1 = map:update(berlin, [london, paris], []),
  Map2 = map:update(reykjavik, [stockholm, berlin, helsinki], Map1),
  Map3 = map:update(berlin, [this, works, now], Map2),
  Success = listCompare(Map3, [{reykjavik,[stockholm,berlin,helsinki]},{berlin,[this,works,now]}]),
  if
    Success -> ok;
    true -> error
  end.

testReachable() ->
  Map1 = map:update(berlin, [london, paris], []),
  Map2 = map:update(reykjavik, [stockholm, berlin, helsinki], Map1),
  Result1 = map:reachable(reykjavik, Map2),
  Result2 = map:reachable(berlin, Map2),
  Result3 = map:reachable(fakecity, Map2),
  io:format("map:testReachable/0: ~p~n", [Result1]),
  io:format("map:testReachable/0: ~p~n", [Result2]),
  io:format("map:testReachable/0: ~p~n", [Result3]),
  ok.

listCompare(List, List) -> true;
listCompare(_, _) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%