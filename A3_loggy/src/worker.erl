%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2016 20:50
%%%-------------------------------------------------------------------
-module(worker).
-author("tts").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

% Initialize a worker. Note it will not start until it receives a peers message.
init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter);
    stop ->
      ok
  end.

% Start worker with peers
peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      % Received message from peer
      Log ! {log, Name, Time, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
    % Send message to random peer
    Selected = select(Peers),
    % Dummy value for time, the worker does not know about the time
    Time = na,
    % Create message with hopefully random number value
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Time, Message},
    % Delay between sending message to peer and sending a message to the logger
    jitter(Jitter),
    Log ! {log, Name, Time, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter)
  end.

% Select a random peer from the list of peers
select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

% Sleep for a random time
jitter(0) ->
  ok;
jitter(Jitter) ->
  timer:sleep(random:uniform(Jitter)).
