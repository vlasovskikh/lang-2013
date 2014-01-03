%% Copyright
-module(lisp_parser_tests).
-author("vlan").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).


empty_test() -> ok.


simple_test() ->
  ?assertEqual({ok, [{atom, "hello"}]}, lisp_parser:parse("hello")).


empty_list_test() ->
  ?assertEqual({ok, [{list, []}]}, lisp_parser:parse("()")).


flat_list_test() ->
  ?assertEqual(
    {ok, [{list, [
      {atom, "hello"},
      {atom, "lisp"},
      {atom, "world"}]}]},
    lisp_parser:parse("(hello lisp world)")).


nested_list_test() ->
  ?assertEqual(
    {ok, [
      {list, [
        {atom, "define"},
        {list, [
          {atom, "id"},
          {atom, "x"}
        ]},
        {atom, "x"}
      ]},
      {list, [
        {atom, "define"},
        {atom, "id"},
        {list, [
          {atom, "lambda"},
          {list, [
            {atom, "x"}
          ]},
          {atom, "x"}
        ]}
      ]}]},
    lisp_parser:parse("(define (id x) x)
           (define id (lambda (x) x))")).


bool_list_test() ->
  ?assertEqual(
    {ok, [
      {list, [
        {bool, true},
        {bool, false},
        {atom, "true"},
        {list, [
          {bool, true}
        ]}
      ]}
    ]},
    lisp_parser:parse("(#t #f true (#t))")).
