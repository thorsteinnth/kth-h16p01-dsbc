%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2016 15:31
%%%-------------------------------------------------------------------
-module(hello_world).
-author("tts").

-export([hello_world/0]).
hello_world() -> io:fwrite("hello, world, from thorsteinn\n").

