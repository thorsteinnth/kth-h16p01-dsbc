%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2016 15:51
%%%-------------------------------------------------------------------
-module(wait).
-author("tts").

%% API
-export([hello/0]).

hello() ->
  receive
    X -> io:format("MESSAGE RECEIVED: ~s~n", [X])
  end.
