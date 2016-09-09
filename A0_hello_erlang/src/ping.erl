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
-export([ping/1]).
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

ping(FOREIGNPROCADDR) ->
  % Send my own PID to FOREIGNPROCADDR
  FOREIGNPROCADDR ! self();
  % Wait for reply
  receive
    % Expecting a remote process PID as the message.
    % Send a reply to that PID.
    FOREIGNPROCPID -> io:format("MESSAGE RECEIVED: ~s~n", [FOREIGNPROCPID])
  end.

pong() ->
  receive
    % Expecting a remote process PID as the message.
    % Send a reply to that PID.
    FOREIGNPROCPID -> io:format("MESSAGE RECEIVED: ~s~n", [FOREIGNPROCPID])
  end.



%ping(ISORIGINATOR, FOREIGNPROCADDR) ->
 % if
  %  ISORIGINATOR == 1 ->
      % Am originator.
      % Should send my PID to FOREIGNPROCADDR
      % and then wait for a reply.
   %   FOREIGNPROCADDR ! "self()";
    %true ->
      % Not originator.
      % Will receive a message containing a PID.
      % Should send a reply to that PID.
     % receive
      %  RECVPID -> io:format("RECVPID RECEIVED: ~s~n", [RECVPID])
      %end
  %end.


%% Stupid að vera að setja þetta í sama fallið. Ætti að búa til tvö föll, ping og pong
