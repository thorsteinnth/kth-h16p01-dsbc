%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2016 02:11
%%%-------------------------------------------------------------------
-module(hist).
-author("tts").

%% API
-export([new/1, update/3]).

% Avoid cyclic paths. Messages tagged with a per router increasing counter (clock).
% If we know we have seen message number 15 from london than we know all messages less than 15 are old and can be
% thrown away. This avoids circular loops and prevents old messages from messing with our view of the system.

% Implement a data structure called history that keeps track of what messages we have seen.

% Let's have it look like this:
% {Name, MessageCounter}
% TODO: Should this be a list of tuples or just a tuple? Doing this as a list of tuples first.

% Return a new history, where messages from Name will always be seen as old.
new(Name) ->
  [{Name, inf}].  % NOTE: inf is greater than any integer

% Check if message number N from the Node is old or new.
% If it is old then return old but if it is new return {new, Updated} where Updated is the updated history.
update(Node, N, History) ->
  FoundTuple = lists:keyfind(Node, 1, History),
  if
    FoundTuple == false ->
      % Don't have a record of this node, add it to the history
      UpdatedHistory = lists:append(History, [{Node, N}]),
      {new, UpdatedHistory};
    true ->
      {_, MessageCounter} = FoundTuple,
      if
        MessageCounter < N ->
          % This is a new message, update history
          UpdatedHistory = lists:keyreplace(Node, 1, History, {Node, N}),
          {new, UpdatedHistory};
        true ->
          % This is an old message, already seen it
          old
      end
  end.


%%%%%%%%%%%%%%%
% TESTS

testNew() ->
  new(reykjavik).

testUpdate() ->
  History = [{berlin, 20}, {london, 15}, {stockholm, 12}],
  update(london, 16, History).

%%%%%%%%%%%%%%%