%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2016 16:09
%%%-------------------------------------------------------------------
-module(tic).
-author("tts").

%% API
-export([first/0]).

first() ->
  receive
    {tic, X} ->
      io:format("tic: ~w~n", [X]),
      second()
  end.

second() ->
  receive
    {tac, X} ->
      io:format("tac: ~w~n", [X]),
      last();
    {toe, X} ->
      io:format("toe: ~w~n", [X]),
      last()
  end.

last() ->
  receive
    X ->
      io:format("end: ~w~n", [X])
  end.

% Messages get queued until the process receives something matching the {tic, X} pattern