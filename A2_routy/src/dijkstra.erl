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
-export([entry/2, testEntry/0]).

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
replace(Node, N, Gateway, Sorted) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testEntry() ->
  Result1 = entry(berlin, [{berlin, 1, reykjavik}, {stockholm, 5, moscow}, {berlin, 3, helsinki}]),
  Result2 = entry(stockholm, [{berlin, 1, reykjavik}, {stockholm, 5, moscow}, {berlin, 3, helsinki}]),
  io:format("dijkstra:testEntry/0: Result 1 should be 1: ~p, Result 2 should be 5: ~p~n", [Result1, Result2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%