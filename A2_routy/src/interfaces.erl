%%%-------------------------------------------------------------------
%%% @author tts
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2016 01:08
%%%-------------------------------------------------------------------
-module(interfaces).
-author("tts").

%% API
-export([testNew/0, testAdd/0, testRemove/0, testLookup/0]).

% A router will also need to keep track of a set of interfaces.
% A interface is described by the symbolic name (london), a process reference and a process identifier.
% {Name, Ref, Pid}

% return an empty set of interfaces.
new() ->
  [].

% add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
  lists:append([{Name, Ref, Pid}], Intf).

% Remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

% Find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
  FoundTuple = lists:keyfind(Name, 1, Intf),
  if
    FoundTuple == false -> notfound;
    true ->
      {_, _, Pid} = FoundTuple,
      {ok, Pid}
  end.

% Find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf) ->
  FoundTuple = lists:keyfind(Name, 1, Intf),
  if
    FoundTuple == false -> notfound;
    true ->
      {_, Ref, _} = FoundTuple,
      {ok, Ref}
  end.

% Find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) ->
  FoundTuple = lists:keyfind(Ref, 2, Intf),
  if
    FoundTuple == false -> notfound;
    true ->
      {Name, _, _} = FoundTuple,
      {ok, Name}
  end.

% Return a list with all names.
list(Intf) ->
  listNames(Intf, []).

listNames([], NameList) ->
  NameList;
listNames(Intf, NameList) ->
  [FirstInterface | RestOfInterfaces] = Intf,
  {FirstInterfaceName, _, _} = FirstInterface,
  NewNameList = lists:append([FirstInterfaceName], NameList),
  listNames(RestOfInterfaces, NewNameList).

% Send the message to all interface processes.
broadcast(Message, Intf) ->
  AllPids = getAllPids(Intf),
  lists:foreach(
    fun(Pid) ->
      Pid ! Message
    end,
    AllPids
  ).

getAllPids(Intf) ->
  getAllPids(Intf, []).
getAllPids([], PidList) ->
  PidList;
getAllPids(Intf, PidList) ->
  [FirstInterface | RestOfInterfaces] = Intf,
  {_, _, FirstInterfacePid} = FirstInterface,
  NewPidList = lists:append([FirstInterfacePid], PidList),
  getAllPids(RestOfInterfaces, NewPidList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

testNew() ->
  new().

testAdd() ->
  Intf1 = new(),
  Intf2 = add(london, londonref, londonpid, Intf1),
  Intf3 = add(berlin, berlinref, berlinpid, Intf2),
  add(stockholm, stockholmref, stockholmpid, Intf3).

testRemove() ->
  Intf = testAdd(),
  remove(berlin, Intf).

testLookup() ->
  Intf = testAdd(),
  lookup(berlin, Intf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
