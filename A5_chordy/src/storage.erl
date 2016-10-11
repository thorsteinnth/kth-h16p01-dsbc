%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 21:50
%%%-------------------------------------------------------------------
-module(storage).
-author("tts").

%% API
-export([create/0, add/3, lookup/2, split/3, merge/2,
  testCreate/0, testAdd/0, testLookup/0, testSplit/0, testMerge/0]).

% The storage is a list of {Key, Value} pairs
% NOTE: Keys will be integers

% Create a new store
create() ->
  [].

% Add a key value pair, return the updated store
add(Key, Value, Store) ->
  lists:append(Store, [{Key, Value}]).

% Return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

% Return a tuple {Updated, Rest} where the updated store only contains the key value pairs
% requested and the rest are found in a list of key-value pairs
% Will be used when a new node joins the ring and should take over a part of the store
split(From, To, Store) ->
  % Keys should be a list of numbers
  % Updated will be a list of the entries in Store that fall between From and To
  % Rest should be the rest of the entries in the Store
  Updated = lists:foldl(
    fun(Element, Accumulator) ->
      {EKey, _} = Element,
      ShouldBeInSplit = shouldBeInSplit(EKey, From, To),
      if
        ShouldBeInSplit ->
          lists:append(Accumulator, [Element]);
        true ->
          Accumulator
      end
    end,
    [],
    Store
  ),
  Rest = lists:foldl(
    fun(Element, Accumulator) ->
      {EKey, _} = Element,
      ShouldBeInSplit = shouldBeInSplit(EKey, From, To),
      if
        ShouldBeInSplit ->
          Accumulator;
        true ->
          lists:append(Accumulator, [Element])
      end
    end,
    [],
    Store
  ),
  {Updated, Rest}.

shouldBeInSplit(Key, From, To) ->
  ((Key >= From) and (Key =< To)).

% Add a list of key-value pairs to a store
% Will be used when a new node joins the ring and should take over a part of the store
merge(Entries, Store) ->
  lists:append(Store, Entries).

printStore(Store) ->
  io:format("[~p] STORE: ~p~n", [self(), Store]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testCreate() ->
  create().

testAdd() ->
  Store = create(),
  Store1 = add(key1, value1, Store),
  Store2 = add(key2, value2, Store1),
  printStore(Store2).

testLookup() ->
  Store = create(),
  Store1 = add(key1, value1, Store),
  Store2 = add(key2, value2, Store1),
  lookup(key3, Store2).

testSplit() ->
  Store = create(),
  Store1 = add(1, value1, Store),
  Store2 = add(2, value2, Store1),
  Store3 = add(3, value3, Store2),
  Store4 = add(4, value4, Store3),
  Store5 = add(5, value5, Store4),
  Store6 = add(6, value6, Store5),
  split(2, 4, Store6).

testMerge() ->
  Store1 = create(),
  Store11 = add(1, value1, Store1),
  Store12 = add(2, value2, Store11),
  Store13 = add(3, value3, Store12),
  Store2 = create(),
  Store21 = add(9, value9, Store2),
  Store22 = add(10, value10, Store21),
  Store23 = add(11, value11, Store22),
  merge(Store23, Store13).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%