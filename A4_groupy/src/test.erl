-module(test).

-compile(export_all).



% Used to create the first worker, try:
%
% W1 = test:first(1, gms1, 1000)

first(N, Module, Sleep) ->
   worker:start(N, Module, random:uniform(256), Sleep).

% Used to create additional workers, try:
%
%  test:add(2, gms1, W1, 1000) and 
%  test:add(3, gms1, W1, 1000) and ...

add(N, Module, Wrk, Sleep) ->
   worker:start(N, Module, random:uniform(256), Wrk, Sleep).

%% To create a number of workers in one go, 

more(N, Module, Sleep) when N > 1 ->
    Wrk = first(1, Module, Sleep),
    Ns = lists:seq(2,N),
    lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns),
    Wrk.
		      

% These are messages that we can send to one of the workers. It will
% multicast it to all workers. They should (if everything works)
% receive the message at the same (logical) time.

freeze(Wrk) ->
    Wrk ! {send, freeze}.

go(Wrk) ->
    Wrk ! {send, go}.

sleep(Wrk, Sleep) ->
    Wrk ! {send, {sleep, Sleep}}.

stop(Wrk) ->
    Wrk ! {send, stop}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Added by me
kill(Wrk) ->
  Wrk ! stop.

test_gms1() ->
  W1 = test:first(1, gms1, 1000),
  W2 = test:add(2, gms1, W1, 1000),
  W3 = test:add(3, gms1, W1, 1000),
  io:format("Will kill leader (W1) in 10 sec~n", []),
  timer:sleep(10000),
  test:kill(W1),
  io:format("Will kill all others in 30 sec~n", []),
  timer:sleep(30000),
  test:kill(W2),
  test:kill(W3).

test_gms2() ->
  W1 = test:first(1, gms2, 1000),
  W2 = test:add(2, gms2, W1, 1000),
  W3 = test:add(3, gms2, W1, 1000),
  io:format("Will kill leader (W1) in 10 sec~n", []),
  timer:sleep(10000),
  test:kill(W1),
  io:format("Will kill all others in 30 sec~n", []),
  timer:sleep(30000),
  test:kill(W2),
  test:kill(W3).

test_gms2_random_crash() ->
  W1 = test:first(1, gms2, 1000),
  W2 = test:add(2, gms2, W1, 1000),
  W3 = test:add(3, gms2, W1, 1000),
  W4 = test:add(4, gms2, W1, 1000),
  W5 = test:add(5, gms2, W1, 1000),
  io:format("Will kill all processes in 90 sec~n", []),
  timer:sleep(90000),
  test:kill(W1),
  test:kill(W2),
  test:kill(W3),
  test:kill(W4),
  test:kill(W5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


			  
















