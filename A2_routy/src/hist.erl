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
-export([]).

% Return a new history, where messages from Name will always be seen as old.
new(Name) ->
  ok.

% Check if message number N from the Node is old or new.
% If it is old then return old but if it new return {new, Updated} where Updated is the updated history.
update(Node, N, History) ->
  ok.