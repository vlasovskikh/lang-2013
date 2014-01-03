-module(lex).
-author("vlan").
-include_lib("eunit/include/eunit.hrl").


-export([make_tokenizer/1]).


-type pos() :: {integer(), integer()}.
-type token() :: {token, atom(), string()}.
-type token_spec() :: {atom(), string()}.


-spec make_tokenizer([token_spec()]) -> fun((string()) -> [token()]).


make_tokenizer(Specs) ->
  case lists:foldr(
      fun ({Type, Regexp}, Res) ->
        case Res of
          {ok, ResList} ->
            case re:compile(Regexp, [anchored]) of
              {ok, Pattern} ->
                {ok, [{Type, Pattern}|ResList]};
              {error, ErrSpec2} ->
                {error, ErrSpec2}
            end;
          {error, ErrSpec} ->
            {error, ErrSpec}
        end
      end,
      {ok, []},
      Specs) of
    {ok, Compiled} ->
      Tokenize = fun (String) ->
        match_specs(String, Compiled, 0, string:len(String), [])
      end,
      {ok, Tokenize};
    {error, ErrSpec} ->
      {error, ErrSpec}
  end.

match_specs(String, Specs, Pos, Len, Tokens) ->
  if
    Pos >= Len ->
      {ok, lists:reverse(Tokens)};
    true ->
      case lists:foldl(
          fun ({Type, Regexp}, Acc) ->
            case Acc of
              {match, Token, Pos2} ->
                {match, Token, Pos2};
              nomatch ->
                case re:run(String, Regexp, [{offset, Pos}]) of
                  {match, Captured} ->
                    {Start, Length} = lists:nth(1, Captured),
                    OneBasedStart = Start + 1,
                    S = string:substr(String, OneBasedStart, Length),
                    {match, {token, Type, S}, Start + Length};
                   nomatch ->
                     nomatch
                end
            end
          end,
          nomatch,
          Specs) of
        {match, Token, NextPos} ->
          match_specs(String, Specs, NextPos, Len, [Token|Tokens]);
        nomatch ->
           {error, msg("No match at position ~p", [Pos])}
      end
  end.


msg(S, Args) ->
  lists:flatten(io_lib:format(S, Args)).


lisp_specs() -> [
    {space, "[ \t\r\n]+"},
    {bool, "(#f|#t)"},
    {op, "[()]"},
    {name, "[A-Za-z]+"}
  ].


simple_test() ->
  {ok, Tokenize} = make_tokenizer(lisp_specs()),
  Result = Tokenize("(hello (lisp world))"),
  Expected = [
    {token, op, "("},
    {token, name, "hello"},
    {token, space, " "},
    {token, op, "("},
    {token, name, "lisp"},
    {token, space, " "},
    {token, name, "world"},
    {token, op, ")"},
    {token, op, ")"}
  ],
  ?assertEqual({ok, Expected}, Result).


nomatch_test() ->
  {ok, Tokenize} = make_tokenizer(lisp_specs()),
  Result = Tokenize("(foo (! bar))"),
  ?assertEqual({error, "No match at position 6"}, Result).

