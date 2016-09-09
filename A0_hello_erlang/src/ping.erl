%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2016 16:54
%%%-------------------------------------------------------------------
-module(ping).
-author("tts").

%% API
-export([ping/2]).
-export([pong/0]).

%% Try to define a process on one machine "ping",
%% that sends a message to a registered process on another machine
%% with its own process identifier in the message.
%% The process should wait for a reply.
%% The receiver on the other machine should look at the message
%% and use the process identifier to send a reply.
%% Once a process knows a process identifier of another process
%% it can use the identifier to communicate with it and does not
%% have to know if it is a local or remote process.

%%%%%%%%%%%%%%%%%%%%%%%

% c(ping).

% Start node 1:
% erl -name node1@192.168.1.8 -setcookie secret
% Start node 2:
% erl -name node2@192.168.1.8 -setcookie secret

% On pong node:
% P = spawn(ping, pong, []).
% register(pongproc, P).

% On ping node:
% ping:ping('pongproc', 'node2@192.168.1.8').

%%%%%%%%%%%%%%%%%%%%%%%

ping(FOREIGNNODE_PROCNAME, FOREIGNNODE) ->
  % Send my own PID to the process FOREIGNNODE_PROCNAME on the foreign node FOREIGNNODE
  {FOREIGNNODE_PROCNAME, FOREIGNNODE} ! self(),
  % Wait for reply
  receive
    % Expecting a pong back
    X -> io:format("MESSAGE RECEIVED IN PING NODE: ~s~n", [X])
  end.

pong() ->
  receive
    % Expecting a remote process PID as the message.
    % Send a pong (as a reply) to that PID.
    FOREIGNPROCPID ->
      io:format("MESSAGE RECEIVED IN PONG NODE, WILL SEND PONG REPLY~n", []),
      FOREIGNPROCPID ! "PONG"
  end.
