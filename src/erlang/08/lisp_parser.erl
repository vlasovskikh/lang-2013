-module(lisp_parser).
-author("vlan").

-export([parse/1]).


% top-level   = { expr }, ? eof ?;
% expr        = bool | atom | list;
% list        = "(" , { expr } , ")";
% atom        = ? name ?;


token_specs() -> [
  {space, "[ \t\r\n]+"},
  {bool, "(#f|#t)"},
  {op, "[()]"},
  {name, "[A-Za-z]+"}
].


tokenize(String) ->
  case lex:make_tokenizer(token_specs()) of
    {ok, Tokenize} ->
      Tokenize(String);
    {error, Msg} ->
      {error, Msg}
  end.


bool(Tokens) ->
  P = funparse:map(
    fun({token, bool, Value}) ->
      case Value of
        "#t" -> {bool, true};
        "#f" -> {bool, false}
      end
    end,
    funparse:tok(bool)),
  P(Tokens).


atom(Tokens) ->
  P = funparse:map(
    fun({token, name, Value}) -> {atom, Value} end,
    funparse:tok(name)),
  P(Tokens).


expr(Tokens) ->
  P = funparse:alt([
    fun bool/1,
    fun atom/1,
    fun list/1
  ]),
  P(Tokens).


list(Tokens) ->
  P = funparse:map(
    fun([_, Exprs, _]) -> {list, Exprs} end,
    funparse:seq([
      funparse:tok(op, "("),
      funparse:many(fun expr/1),
      funparse:tok(op, ")")
    ])),
  P(Tokens).


top_level(Tokens) ->
  P = funparse:map(
    fun([Result, _]) -> Result end,
    funparse:seq([
      funparse:many(fun expr/1),
      fun funparse:finished/1
    ])),
  P(Tokens).


parse(String) ->
  case tokenize(String) of
    {ok, Tokens} ->
      Useful = lists:filter(
        fun({token, Type, _}) ->
          case Type of
            space -> false;
            _ -> true
          end
        end,
        Tokens),
      case top_level(Useful) of
        {ok, Parsed, _} -> {ok, Parsed};
        {error, Msg, _} -> {error, Msg}
      end
  end.
