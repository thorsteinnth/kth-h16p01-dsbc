%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Sep 2016 19:21
%%%-------------------------------------------------------------------
-module(rudy).
-author("tts").

%% API
-export([init/1]).

% c(rudy).
% rudy:init(8080).
% In browser, request: http://localhost:8080/foo

% NOTE: A socket that a server listens to is not the same thing as the socket later user for communication

% init(Port):
% The procedure that will initialize the server, takes a port number (for example 8080),
% opens a listening socket and passes the socket to handler/1. Once the request
% has been handled the socket will be closed.
init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of % Open listening socket
      {ok, Listen} -> % {ok, listensocket}
        handler(Listen), % FOR ME TO FILL IN
        gen_tcp:close(Listen),
        ok;
      {error, Error} -> % {error, reason}
        error
    end.

% handler(Listen):
% Will listen to the socket for an incoming connection. Once a client has connected it will
% pass the connection to request/1. When the request is handled the connection is closed.
handler(Listen) ->
  case gen_tcp:accept(Listen) of  % Accept an incoming request. If successful we have a communication channel w client.
    {ok, Client} -> % {ok, Socket} ... i.e. the socket used for communication with the client
      request(Client);  % FOR ME TO FILL IN
    {error, Error} -> % {error, Reason}
      error
  end.

% request(Client):
% Will read the request from the client connection and parse it. It will then parse the request using
% your http parser and pass the request to reply/1. The reply is then sent back to the client.
request(Client) ->
  Recv = gen_tcp:recv(Client, 0), % Read from client (in our case return it as string), 0: read as much as possible
  case Recv of
    {ok, Str} ->  % {ok, Packet}
      Request = http:parse_request(Str), % FOR ME TO FILL IN
      Response = reply(Request),
      gen_tcp:send(Client, Response); % Send reply to client (in our case in form of string)
    {error, Error} -> % {error, Reason}
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).  % Close connection and listening socket

% reply(Request):
% This is where we decide what to reply, how to turn the reply into a well formed HTTP reply
reply({{get, URI, _}, _, _}) ->
  http:ok("THIS IS THE RESPONSE").







