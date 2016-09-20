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
-export([table/2, route/2]).

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

% Update the list Sorted given the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added.
% Only if we have a better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
  % Check if we have an entry for this node
  CurrentShortestPathLength = entry(Node, Sorted),
  if
    CurrentShortestPathLength == 0
    % We do not have an entry for this node
      -> Sorted;
    true ->
      % We have an entry for this node
      if
        CurrentShortestPathLength > N ->
          % The new path is shorter than the current one, let's replace the current one
          UpdatedSorted = replace(Node, N, Gateway, Sorted),
          UpdatedSorted;
        true ->
          % The new path is equally long or longer than the current one, change nothing
          Sorted
      end
  end.

% Construct a table given a sorted list of nodes, a map and a table constructed so far.
% iterate(Sorted, Map, Table)
% If there are no more entries in the sorted list then we are done and the given routing table is complete.
iterate([], Map, Table) ->
  Table;
% If the first entry is a dummy entry with an infinite path to a city we know that the rest of the sorted list
% is also of infinite length and the given routing table is complete.
% Otherwise, take the first entry in the sorted list, find the nodes in the map reachable from this entry and
% for each of these nodes update the Sorted list. The entry that you took from the sorted list
% is added to the routing table.
iterate(Sorted, Map, Table) ->
  [FirstElement | RestOfElements] = Sorted,
  {FirstElementNode, FirstElementN, FirstElementGateway} = FirstElement,
  if
    FirstElementN == inf ->
      % Have handled all destinations in the sorted list. All following destinations will also not be reachable.
      Table;
    true ->
      % Find the nodes that are reachable from the first element in the sorted list
      ReachableNodesFromFirstElement = map:reachable(FirstElementNode, Map),
      % For each node that is reachable from the first element in the sorted list, update it in the sorted list
      % (will only update it if we have a better path to it) (NOTE: Map contains cities that are directly connected)
      NewSorted = updateReachableNodes(ReachableNodesFromFirstElement, FirstElementN + 1, FirstElementGateway, RestOfElements),
      NewTable = lists:append(Table, [{ FirstElementNode, FirstElementGateway }]),
      iterate(NewSorted, Map, NewTable)
  end.

updateReachableNodes([], N, Gateway, Sorted) ->
  Sorted;
updateReachableNodes(ReachableNodes, N, Gateway, Sorted) ->
  [FirstReachableNode | RestOfReachableNodes] = ReachableNodes,
  NewSorted = update(FirstReachableNode, N, Gateway, Sorted),
  updateReachableNodes(RestOfReachableNodes, N, Gateway, NewSorted).

% Construct a routing table given the gateways and a map.
% Take a list of gateways and a map and produce a routing table with one entry per node in the map.
% The table could be a list of entries where each entry states which gateway to use to find the shortest path
% to a node (if we have a path).
table(Gateways, Map) ->
  % Construct initial sorted list
  InitialSortedList = constructInitialSortedList(Gateways, Map),
  % When you have constructed the initial sorted list you can call iterate with an empty table.
  iterate(InitialSortedList, Map, []).

% constructInitialSortedList(Gateways, Map)
% Gateways is a list of all gateways e.g. [reykjavik, nuuk]
% Map is a list of tuples [{Node, [City1, City2, City3]}]
constructInitialSortedList(Gateways, Map) ->
  AllMapNodes = map:all_nodes(Map),
  constructInitialSortedList(Gateways, AllMapNodes, []).

% constructInitialSortedList(Gateways, MapNodes, SortedList)
% Take the list of map nodes and construct an initial sorted list.
% This list should have dummy entries for all nodes with the length set to infinity, inf, and the gateway
% to unknown. The entries of the gateways should have length zero and gateway set to itself.
% MapNodes is a list of nodes [Node1,Node2,Node3]
% SortedList is a sorted list of { node, lengthOfPathToNode, gateway }, sorted by increasing lengthOfPathToNode
constructInitialSortedList(_, [], SortedList) ->
  % Let's sort the list before we return it
  ActuallySortedList = lists:keysort(2, SortedList),
  ActuallySortedList;
constructInitialSortedList(Gateways, MapNodes, SortedList) ->
  [FirstMapNode | RestOfMapNodes] = MapNodes,
  FirstMapNodeIsGateway = lists:member(FirstMapNode, Gateways),
  if
    FirstMapNodeIsGateway == true ->
      % The node is a gateway, should have path length zero and the gateway set to itself
      NewSortedList = lists:append([{FirstMapNode, 0, FirstMapNode}], SortedList),
      constructInitialSortedList(Gateways, RestOfMapNodes, NewSortedList);
    true ->
      % The node is not a gateway, should have length set to infinity and gateway to unknown
      NewSortedList = lists:append([{FirstMapNode, inf, unknown}], SortedList),
      constructInitialSortedList(Gateways, RestOfMapNodes, NewSortedList)
  end.

% Search the routing table and return the gateway suitable to route messages to a node.
% If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
% Table is a routing table of the form e.g. [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]
% The table is of the form [{node,gateway}]
% This could be a list of entries where each entry states which gateway to use to find the shortest path to
% a node (if we have a path).
route(Node, Table) ->
  FoundTuple = lists:keyfind(Node, 1, Table),
  if
    FoundTuple == false -> notfound;
    true ->
      {_, Gateway} = FoundTuple,
      {ok,Gateway}
  end.

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

testUpdate() ->
  Result1 = update(london, 2, amsterdam, []),
  Result2 = update(london, 2, amsterdam, [{london, 2, paris}]),
  Result3 = update(london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]),
  io:format("dijkstra:testUpdate/0:~n Result 1: ~p~n Result 2: ~p~n Result 3: ~p~n", [Result1, Result2, Result3]).

testIterate() ->
  iterate([{paris, 0, paris}, {berlin, inf, unknown}], [{paris, [berlin]}], []).

testConstructInitialSortedList() ->
  Map1 = map:update(berlin, [london, paris], []),
  Map2 = map:update(reykjavik, [stockholm, berlin, helsinki], Map1),
  Map3 = map:update(stockholm, [nuuk], Map2),
  Gateways = [reykjavik, helsinki, nuuk],
  constructInitialSortedList(Gateways, Map3).

testTable() ->
  table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]).

testRoute() ->
  Table = table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]),
  route(berlin, Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%