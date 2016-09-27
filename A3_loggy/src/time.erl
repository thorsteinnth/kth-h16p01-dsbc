%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2016 15:50
%%%-------------------------------------------------------------------
-module(time).
-author("tts").

%% API
-export([zero/0, inc/2, merge/2, leq/2]).

% Return an initial Lamport value (could it be 0)
zero() ->
  0.

% Return the time T incremented by one (you will probably ignore the Name but we will use it later)
inc(Name, T) ->
  T+1.

% Merge the two Lamport time stamps (i.e. take the maximum value)
merge(Ti, Tj) ->
  erlang:max(Ti, Tj).

% True if Ti is less than or equal to Tj
leq(Ti,Tj) ->
  if
    Ti =< Tj -> true;
    true -> false
  end.
