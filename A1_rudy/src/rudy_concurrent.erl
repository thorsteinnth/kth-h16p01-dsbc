%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2016 17:54
%%%-------------------------------------------------------------------
-module(rudy_concurrent).
-author("tts").

%% API
%-export([init/1]). % Use start and stop instead
-export([start/1, stop/0]).

% c(rudy_concurrent).
% rudy_concurrent:start(8080). and then rudy_concurrent:stop().
% In browser, request: http://localhost:8080/foo

% NOTE: A socket that a server listens to is not the same thing as the socket later user for communication

start(Port) ->
  register(rudy_concurrent, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die"). % whereis() returns process identifier or port identifier

% init(Port):
% The procedure that will initialize the server, takes a port number (for example 8080),
% opens a listening socket and passes the socket to handler/1. Once the request
% has been handled the socket will be closed.
init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of % Open listening socket
    {ok, Listen} -> % {ok, listensocket}
      handler(Listen),
      % Wait for stop command before closing listening socket
      % TODO Figure out how to do this properly, right now we never go past handler(Listen).
      receive
        stop ->
          io:format("[~p]rudy:init: closing listening socket~n", [self()]),
          gen_tcp:close(Listen) % Close listening socket
      end,
      ok;
    {error, Error} -> % {error, reason}
      io:format("[~p]rudy:init: error: ~w~n", [self(), Error]),
      error
  end.

% handler(Listen):
% Will listen to the socket for an incoming connection. Once a client has connected it will
% pass the connection to request/1. When the request is handled the connection is closed.
handler(Listen) ->
  case gen_tcp:accept(Listen) of  % Accept an incoming request. If successful we have a communication channel w client.
    {ok, Client} -> % {ok, Socket} ... i.e. the socket used for communication with the client
      % Spawn a worker to handle the client connection
      spawnRequestWorker(Client),
      handler(Listen);  % 2.2 - recursive call to handle the next request
    {error, Error} -> % {error, Reason}
      io:format("[~p]rudy:handler: error: ~w~n", [self(), Error]),
      error
  end.

spawnRequestWorker(Client) ->
  P = spawn(fun() -> request(Client) end),
  io:format("[~p]rudy:spawnRequestWorker: Spawned worker with PID ~p~n", [self(), P]).

% request(Client):
% Will read the request from the client connection and parse it. It will then parse the request using
% your http parser and pass the request to reply/1. The reply is then sent back to the client.
request(Client) ->
  Recv = gen_tcp:recv(Client, 0), % Read from client (as string), 0: read as much as possible
  case Recv of
    {ok, Str} ->  % {ok, Packet}
      Request = http:parse_request(Str), % FOR ME TO FILL IN
      Response = reply(Request),
      gen_tcp:send(Client, Response); % Send reply to client (in our case in form of string)
    {error, Error} -> % {error, Reason}
      io:format("[~p]rudy:request: error: ~w~n", [self(), Error])
  end,
  io:format("[~p]rudy:request: Finished, will close client connection ~n", [self()]),
  gen_tcp:close(Client).  % Close connection to client

% reply(Request):
% This is where we decide what to reply, how to turn the reply into a well formed HTTP reply
reply({{get, URI, _}, _, _}) ->
  timer:sleep(20000),  % Insert small delay to simulate file handling, server side scripting etc.
  http:ok("THIS IS THE RESPONSE").
