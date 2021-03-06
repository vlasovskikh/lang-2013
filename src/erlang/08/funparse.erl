-module(funparse).
-author("vlan").
-export([char/1, tok/1, tok/2, seq/1, alt/1, many/1, map/2, finished/1]).
-include_lib("eunit/include/eunit.hrl").


char(C) ->
  fun(S) ->
    case S of
      [C|S2] -> {ok, C, S2};
      [C2|_] -> {error, msg("Expected '~c', but found '~c'", [C, C2]), S};
      [] -> {error, msg("Expected '~c', but found end of file", [C]), S}
    end
  end.


tok(Type) ->
  fun (Toks) ->
    case Toks of
      [{token, Type, Value}|Toks2] -> {ok, {token, Type, Value}, Toks2};
      [OtherTok|_] -> {error, msg("Expected type '~p', but found '~p'", [Type, OtherTok]), Toks};
      [] -> {error, msg("Expected type '~p', but found end of file", [Type]), Toks}
    end
  end.


tok(Type, Value) ->
  fun (Toks) ->
    case Toks of
      [{token, Type, Value}|Toks2] -> {ok, {token, Type, Value}, Toks2};
      [OtherTok|_] -> {error, msg("Expected type '~p', but found '~p'", [Type, OtherTok]), Toks};
      [] -> {error, msg("Expected type '~p', but found end of file", [Type]), Toks}
    end
  end.


seq([]) ->
  fun(S) ->
    {ok, [], S}
  end;
seq([P|Ps]) ->
  fun(S) ->
    case P(S) of
      {ok, Value, S2} ->
        P2 = seq(Ps),
        case P2(S2) of
          {ok, Values, S3} ->
            {ok, [Value|Values], S3};
          {error, Msg, _} ->
            {error, Msg, S}
        end;
      {error, Msg, _} ->
        {error, Msg, S}
    end
  end.


alt([]) ->
  fun (S) ->
    {error, "No alternatives found", S}
  end;
alt([P|Ps]) ->
  fun (S) ->
    case P(S) of
      {ok, Value, S2} ->
        {ok, Value, S2};
      {error, _, S2} ->
        P2 = alt(Ps),
        P2(S2)
    end
  end.


many(P) ->
  fun(S) ->
    case P(S) of
      {ok, Value, S2} ->
        P2 = many(P),
        {ok, Values, S3} = P2(S2),
        {ok, [Value|Values], S3};
      {error, _, S2} ->
        {ok, [], S2}
    end
  end.


maybe(P) ->
  fun (S) ->
    case P(S) of
      {ok, Value, S2} ->
        {ok, {just, Value}, S2};
      {error, _, S2} ->
        {ok, nothing, S2}
    end
  end.


map(F, P) ->
  fun (S) ->
    case P(S) of
      {ok, Value, S2} -> {ok, F(Value), S2};
      {error, Msg, S2} -> {error, Msg, S2}
    end
  end.


finished(S) ->
  case S of
    [] -> {ok, finished, []};
    [C|_] -> {error, msg("Expected end of file, but found '~p'", [C]), S}
  end.


msg(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).


simple_test() ->
  P1 = many(char($.)),
  ?assertEqual({ok, [$., $., $.], []}, P1("...")),
  P2 = seq([char($(), many(char($.)), char($))]),
  ?assertEqual({ok, [$(, "...", $)], []}, P2("(...)")),
  P3 = map(fun ([_, Dots, _]) -> Dots end, P2),
  ?assertEqual({ok, "...", []}, P3("(...)")).


finished_test() ->
  P1 = many(char($.)),
  P2 = seq([P1, fun finished/1]),
  ?assertEqual({ok, [$., $., $.], "a"}, P1("...a")),
  ?assertEqual({error, "Expected end of file, but found '97'", "...a"}, P2("...a")).


alt_test() ->
  P1 = alt([char($a), char($b)]),
  ?assertEqual({ok, $a, ""}, P1("a")),
  ?assertEqual({ok, $b, ""}, P1("b")).


maybe_test() ->
  P1 = maybe(alt([char($a), char($b)])),
  ?assertEqual({ok, {just, $a}, ""}, P1("a")),
  ?assertEqual({ok, {just, $b}, ""}, P1("b")),
  ?assertEqual({ok, nothing, "c"}, P1("c")).
