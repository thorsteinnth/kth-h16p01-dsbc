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
-export([]).

% The storage is a list of {Key, Value} pairs

% Create a new store
create() ->
  ok.

% Add a key value pair, return the updated store
add(Key, Value, Store) ->
  ok.

% Return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
  ok.

% Return a tuple {Updated, Rest} where the updated store only contains the key value pairs
% requested and the rest are found in a list of key-value pairs
% Will be used when a new node joins the ring and should take over a part of the store
split(From, To, Store) ->
  ok.

% Add a list of key-value pairs to a store
% Will be used when a new node joins the ring and should take over a part of the store
merge(Entries, Store) ->
  ok.
