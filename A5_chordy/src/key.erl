%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2016 17:45
%%%-------------------------------------------------------------------
-module(key).
-author("tts").

%% API
-export([generate/0, between/3,
  testGenerate/0, testBetween/0]).

% Return a random number from 1 to 1.000.000.000 (30 bits)
generate() ->
  % Returns, for a specified integer N >= 1,
  % a random integer uniformly distributed between 1 and N
  random:uniform(1000000000).

% Check if a Key is between From and To, or equal to To
% Partly closed interval (From, To]
% NOTE: From could be larger than To (this is a ring)
% From could also be equal to To - interpret this as the full circle, i.e. anything is in between
between(Key, From, To) ->
  if
    From == To ->
      % From is equal to To, anything is in between
      true;
    From < To ->
      % No wrap around
      if
        (Key > From) and (Key =< To) ->
          true;
        true ->
          false
      end;
    To < From ->
      if
        (Key > From) or (Key =< To) ->
          true;
        true ->
          false
      end;
    true ->
      ok
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testGenerate() ->
  generate().

% 0 1 2 3 4 5 6 7 8 0 1 2 3 4 5 6 7 8 0 1 2 3 4 ... ring
%     F       T
% Stærri en F og minni eða jafnt of T
% 0 1 2 3 4 5 6 7 8
%     T       F
% Vil núna fá 7,8,0,1 true
% True ef stærri en F EÐA minni eða jafnt of T

testBetween() ->
  EqualsTest = between(6, 9, 9),
  io:format("Equals, should be true: ~p~n", [EqualsTest]),
  FLessThanTTestTrue1 = between(3, 2, 6),
  FLessThanTTestTrue2 = between(4, 2, 6),
  FLessThanTTestTrue3 = between(5, 2, 6),
  FLessThanTTestTrue4 = between(6, 2, 6),
  io:format("F less than T, should all be true: ~p ~p ~p ~p~n",
    [FLessThanTTestTrue1, FLessThanTTestTrue2, FLessThanTTestTrue3, FLessThanTTestTrue4]),
  FLessThanTTestFalse1 = between(7, 2, 6),
  FLessThanTTestFalse2 = between(8, 2, 6),
  FLessThanTTestFalse3 = between(0, 2, 6),
  FLessThanTTestFalse4 = between(1, 2, 6),
  FLessThanTTestFalse5 = between(2, 2, 6),
  io:format("F less than T, should all be false: ~p ~p ~p ~p ~p~n",
    [FLessThanTTestFalse1, FLessThanTTestFalse2, FLessThanTTestFalse3, FLessThanTTestFalse4, FLessThanTTestFalse5]),
  FLargerThanTTestTrue1 = between(7, 6, 2),
  FLargerThanTTestTrue2 = between(8, 6, 2),
  FLargerThanTTestTrue3 = between(0, 6, 2),
  FLargerThanTTestTrue4 = between(1, 6, 2),
  FLargerThanTTestTrue5 = between(2, 6, 2),
  io:format("F larger than T, should all be true: ~p ~p ~p ~p ~p~n",
    [FLargerThanTTestTrue1, FLargerThanTTestTrue2, FLargerThanTTestTrue3, FLargerThanTTestTrue4, FLargerThanTTestTrue5]),
  FLargerThanTTestFalse1 = between(3, 6, 2),
  FLargerThanTTestFalse2 = between(4, 6, 2),
  FLargerThanTTestFalse3 = between(5, 6, 2),
  FLargerThanTTestFalse4 = between(6, 6, 2),
  io:format("F larger than T, should all be false: ~p ~p ~p ~p~n",
    [FLargerThanTTestFalse1, FLargerThanTTestFalse2, FLargerThanTTestFalse3, FLargerThanTTestFalse4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%