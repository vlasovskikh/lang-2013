-module(hof).
-author("vlan").
-export([map/2, filter/2, foldr/3, foldl/3, join/2]).
-include_lib("eunit/include/eunit.hrl").


-spec map(fun((A) -> B), [A]) -> [B].
-spec filter(fun((A) -> boolean()), [A]) -> [A].
-spec foldr(fun((A, B) -> B), B, [A]) -> B.


map(_, []) ->
  [];
map(F, [X|Xs]) ->
  [F(X)|map(F, Xs)].


filter(_, []) ->
  [];
filter(F, [X|Xs]) ->
  case F(X) of
    true ->
      [X|filter(F, Xs)];
    _ ->
      Xs
  end.


foldr(_, Z, []) ->
  Z;
foldr(F, Z, [X|Xs]) ->
  F(X, foldr(F, Z, Xs)).


foldl(_, Z, []) ->
  Z;
foldl(F, Z, [X|Xs]) ->
  foldl(F, F(X, Z), Xs).


join([], _Sep) ->
  "";
join([First|Rest], Sep) ->
  foldl(fun (S, Result) -> Result ++ Sep ++ S end,
        First,
        Rest).


map_f(F, Xs) ->
  foldr(fun (X, Acc) -> [F(X)|Acc] end, [], Xs).


map_test() ->
  ?_assertEqual([1, 2, 3], map(fun (X) -> X end, [1, 2, 3])),
  ?_assertEqual([1, 4, 9], map(fun (X) -> X * X end, [1, 2, 3])).


map_f_test() ->
  ?_assertEqual([1, 2, 3], map_f(fun (X) -> X end, [1, 2, 3])),
  ?_assertEqual([1, 4, 9], map_f(fun (X) -> X * X end, [1, 2, 3])).


join_test() ->
  ?_assertEqual("foo, bar, baz", join(["foo", "bar", "baz"], ", ")),
  ?_assertEqual("foo", join(["foo"], ", ")),
  ?_assertEqual("", join([], ", ")),
  Ip = [127, 0, 0, 1],
  Actual = join(map(fun (X) -> integer_to_list(X, 10) end, Ip), "."),
  ?_assertEqual("127.0.0.1", Actual).
