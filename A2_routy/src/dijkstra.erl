%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2016 11:20
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("tts").

%% API
-export([entry/2, replace/4]).

% Returns the length of the shortest path to the node or 0 if the node is not found.
% Sorted is a sorted list of { node, lengthOfPathToNode, gateway }, sorted by increasing lengthOfPathToNode
entry(Node, Sorted) ->
  % Keyfind searches from left to right, should find the first tuple that compares true to key.begin
  % Since Sorted is sorted by path length this will return the shortest path.
  FoundTuple = lists:keyfind(Node, 1, Sorted),
  if
    FoundTuple == false -> 0;
    true ->
      {_, LengthOfPathToNode, _} = FoundTuple,
      LengthOfPathToNode
  end.

% Replaces the entry for Node in Sorted with a new entry having a new length N and Gateway.
% The resulting list should of course be sorted.
% Sorted is a sorted list of { node, lengthOfPathToNode, gateway }, sorted by increasing lengthOfPathToNode
% NOTE: Elements in sorted are unique by { node, _, _ }.
replace(Node, N, Gateway, Sorted) ->
  % keyreplace(Key, N, TupleList1, NewTuple) -> TupleList2
  % Returns a copy of TupleList1 where the first occurrence of a T tuple
  % whose Nth element compares equal to Key is replaced with NewTuple, if there is such a tuple T.
  % NOTE: Have to make sure that the entry exists in the list.
  % keyreplace will return the same list if it doesn't find the tuple with Key in it.begin
  % We accept that and sort the list again even if the list is unchanged.
  ListWithNodeReplaced = lists:keyreplace(Node, 1, Sorted, { Node, N, Gateway }),
  % Have to sort the list again since the new tuple may be in the wrong place.
  % keysort(N, TupleList1) -> TupleList2
  % Returns a list containing the sorted elements of list TupleList1.
  % Sorting is performed on the Nth element of the tuples. The sort is stable.
  SortedListWithNodeReplaced = lists:keysort(2, ListWithNodeReplaced),
  SortedListWithNodeReplaced.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testEntry() ->
  Result1 = entry(berlin, [{berlin, 1, reykjavik}, {stockholm, 5, moscow}, {berlin, 3, helsinki}]),
  Result2 = entry(stockholm, [{berlin, 1, reykjavik}, {stockholm, 5, moscow}, {berlin, 3, helsinki}]),
  io:format("dijkstra:testEntry/0: Result 1 should be 1: ~p, Result 2 should be 5: ~p~n", [Result1, Result2]).

testReplace() ->
  List0 = [{berlin, 1, reykjavik}, {stockholm, 5, moscow}, {berlin, 3, helsinki}],
  io:format("List0: ~p~n", [List0]),
  List1 = replace(berlin, 9, reykjavik, List0),
  io:format("List1: ~p~n", [List1]),
  List2 = replace(moscow, 10, newyork, List1),
  io:format("List2: ~p~n", [List2]),
  List3 = replace(stockholm, 1, copenhagen, List2),
  io:format("List3: ~p~n", [List3]),
  List4 = replace(berlin, 4, oslo, List3),
  io:format("List4: ~p~n", [List4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%