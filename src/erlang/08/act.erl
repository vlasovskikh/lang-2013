-module(act).
-author("vlan").
-include_lib("eunit/include/eunit.hrl").

-export([]).


counter(Value) ->
  receive
    {set, NewValue} ->
      counter(NewValue);
    {get, Pid} ->
      Pid ! Value,
      counter(Value);
    terminate ->
      ok;
    _ ->
      counter(Value)
  end.


start(Value) ->
  spawn(fun () -> counter(Value) end).


stop(Pid) ->
  Pid ! terminate.


get_value(Pid) ->
  Pid ! {get, self()},
  receive
    Value -> Value
  end.


set_value(Pid, Value) ->
  Pid ! {set, Value}.


inc_value(Pid) ->
  set_value(Pid, get_value(Pid) + 1).


counter_test() ->
  Cnt = start(0),
  ?assertEqual(0, get_value(Cnt)),
  inc_value(Cnt),
  ?assertEqual(1, get_value(Cnt)),
  stop(Cnt).
