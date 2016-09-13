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
% http http://localhost:8080/foo

% NOTE: A socket that a server listens to is not the same thing as the socket later user for communication

start(Port) ->
  register(rudy_concurrent, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy_concurrent), "time to die"). % whereis() returns process identifier or port identifier

% init(Port):
% The procedure that will initialize the server, takes a port number (for example 8080),
% opens a listening socket and passes the socket to handler/1. Once the request
% has been handled the socket will be closed.
init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of % Open listening socket
    {ok, Listen} -> % {ok, listensocket}

      % We have a listen socket

      % Spawn poolManager process
      PoolManagerPID = spawn(fun() -> poolManager(3) end),

      % Start handling the listen socket
      handler(Listen, PoolManagerPID),

      % Close listening socket
      % TODO Figure out a good way to take the server down. We actually never reach here since the handler() call is
      % recursive and waits for new clients forever. Our current way of taking the server down is to just kill the
      % server.
      io:format("[~p]rudy:init: closing listening socket~n", [self()]),
      gen_tcp:close(Listen), % Close listening socket
      ok;
    {error, Error} -> % {error, reason}
      io:format("[~p]rudy:init: error: ~w~n", [self(), Error]),
      error
  end.

% handler(Listen):
% Will listen to the socket for an incoming connection. Once a client has connected it will
% pass the connection to request/1. When the request is handled the connection is closed.
handler(Listen, PoolManagerPID) ->
  case gen_tcp:accept(Listen) of  % Accept an incoming request. If successful we have a communication channel w client.
    {ok, Client} -> % {ok, Socket} ... i.e. the socket used for communication with the client

      % We have a communication connection with a client

      % Spawn a worker to handle the client connection
      PoolManagerPID ! { spawn, Client },

      % Recursive call to handle the next request from a client
      handler(Listen, PoolManagerPID);
    {error, Error} -> % {error, Reason}
      io:format("[~p]rudy:handler: error: ~w~n", [self(), Error]),
      error
  end.

% request(Client, CreatorPID):
% Client is the client socket. CreatorPID is the PID of the process that created this process.
% Will read the request from the client connection and parse it. It will then parse the request using
% your http parser and pass the request to reply/1. The reply is then sent back to the client.
request(Client, CreatorPID) ->
  Recv = gen_tcp:recv(Client, 0), % Read from client (as string), 0: read as much as possible
  case Recv of
    {ok, Str} ->  % {ok, Packet}
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response); % Send reply to client (in our case in form of string)
    {error, Error} -> % {error, Reason}
      io:format("[~p]rudy:request: error: ~w~n", [self(), Error])
  end,
  io:format("[~p]rudy:request: Finished, will notify creator process and close client connection~n", [self()]),
  CreatorPID ! finished,
  gen_tcp:close(Client).  % Close connection to client

% reply(Request):
% This is where we decide what to reply, how to turn the reply into a well formed HTTP reply
reply({{get, URI, _}, _, _}) ->
  timer:sleep(20000),  % Insert small delay to simulate file handling, server side scripting etc.
  http:ok("THIS IS THE RESPONSE").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process pool

% poolManager(PoolSize)
% Initialize a PoolManager with a process pool with max number of processes Size.
poolManager(PoolSize) ->
  P = spawn(fun() -> pool(PoolSize) end),
  poolManager(PoolSize, P).

% poolManager(PoolSize, PoolPID)
% Recursive function to handle messages sent to the poolManager process.
poolManager(PoolSize, PoolPID) ->
  receive
    { spawn, Client } ->
      io:format("[~p]rudy:poolManager/2: Requesting spawn~n", [self()]),
      PoolPID ! { spawn, Client, self() },
      poolManager(PoolSize, PoolPID);
    accepted ->
      io:format("[~p]rudy:poolManager/2: Spawn request ACCEPTED~n", [self()]),
      poolManager(PoolSize, PoolPID);
    rejected ->
      io:format("[~p]rudy:poolManager/2: Spawn request REJECTED~n", [self()]),
      poolManager(PoolSize, PoolPID)
  end.

% pool(Size)
% Initialize a process pool with max number of processes Size.
% NOTES:
% When trap_exit is set to true, exit signals arriving to a process are converted to {'EXIT', From, Reason} messages,
% which can be received as ordinary messages.
% If trap_exit is set to false, the process exits if it receives an exit signal other than normal and the exit signal
% is propagated to its linked processes. Application processes are normally not to trap exits.
pool(Size) ->
  process_flag(trap_exit, true),
  pool(0, Size).

% pool(CurrentSize, MaxSize)
% Recursive function to handle messages sent to the pool.
pool(CurrentSize, MaxSize) ->
  io:format("[~p]rudy:pool/2:[CurrentSize, MaxSize] = [~p, ~p]~n", [self(), CurrentSize, MaxSize]),
  receive
    finished ->
      % A worker process from the pool is declaring itself finished, reduce currentSize by one.
      pool(CurrentSize - 1, MaxSize);
    { spawn, Client, RequestingPID } when CurrentSize < MaxSize ->
      % Spawn request received. There is room in the process pool so we spawn a new process and send a confirmation
      % to the requesting process.
      spawnRequestWorker(Client, self()),
      RequestingPID ! accepted,
      pool(CurrentSize + 1, MaxSize);
    { spawn, Client, RequestingPID } ->
      % Spawn request received. There is no room left in the pool so we deny the request and send a 'rejected'
      % message to the requesting process.
      RequestingPID ! rejected,
      pool(CurrentSize, MaxSize)
  end.

% spawnRequestWorker(Client, RequesterPID)
% Spawn a new request worker to handle Client. RequesterPID is the PID of the process that is requesting the spawn.
spawnRequestWorker(Client, RequesterPID) ->
  P = spawn(fun() -> request(Client, RequesterPID) end),
  io:format("[~p]rudy:spawnRequestWorker: Spawned worker with PID ~p~n", [self(), P]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%