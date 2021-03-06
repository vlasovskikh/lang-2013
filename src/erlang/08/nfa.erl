-module(nfa).
-author("vlan").
-export([from_regexp/1, run/2]).
-include_lib("eunit/include/eunit.hrl").


from_regexp(Regexp) -> from_regexp(Regexp, graph:new()).


run(S, {Start, Stop, Graph}) ->
  States = epsilon_closure(sets:from_list([Start]), Graph),
  run(S, States, Stop, Graph).


from_regexp({char, C}, G) ->
  new_nfa({char, C}, G);
from_regexp({class, ClassSpec}, G) ->
  new_nfa({class, ClassSpec}, G);
from_regexp({seq, Regexps}, G) ->
  lists:foldl(fun (Regexp, {Start1, Stop1, G1}) ->
                {Start2, Stop2, G2} = from_regexp(Regexp, G1),
                G3 = graph:add_edge(Stop1, Start2, epsilon, G2),
                {Start1, Stop2, G3}
              end,
              new_nfa(epsilon, G),
              Regexps);
from_regexp({star, Regexp}, G) ->
  {Start1, Stop1, G2} = from_regexp(Regexp, G),
  {Start2, Stop2, G3} = new_nfa(epsilon, G2),
  G4 = graph:add_edge(Start2, Start1, epsilon, G3),
  G5 = graph:add_edge(Stop1, Stop2, epsilon, G4),
  G6 = graph:add_edge(Stop1, Start1, epsilon, G5),
  {Start2, Stop2, G6}.


new_nfa(Label, G) ->
  {S1, G2} = graph:add_node(G),
  {S2, G3} = graph:add_node(G2),
  G4 = graph:add_edge(S1, S2, Label, G3),
  {S1, S2, G4}.


fixpoint(F, X) ->
  Y = F(X),
  if X =:= Y -> X;
     true -> fixpoint(F, Y)
  end.


epsilon_set(States, Graph) ->
  sets:fold(
    fun (State, Acc) ->
      Edges = graph:edges_from(State, Graph),
      EpsilonEdges = lists:filter(
        fun (Edge) ->
          graph:edge_label(Edge) =:= epsilon
        end,
        Edges),
      Targets = lists:map(fun graph:edge_target/1, EpsilonEdges),
      sets:union(Acc, sets:from_list(Targets))
    end,
    sets:new(),
    States).


epsilon_closure(States, Graph) ->
  fixpoint(
    fun (Set) ->
      sets:union(Set, epsilon_set(Set, Graph))
    end,
    States).


max_pos(States, Stop, Pos, Max) ->
  case sets:is_element(Stop, States) of
    true -> max(Pos, Max);
    false -> Max
  end.


run(S, States, Stop, Graph) ->
  {_, _, Max} = lists:foldl(
    fun (C, {States, Pos, Max}) ->
      Moved = sets:fold(
        fun (State, Acc) ->
          States2 = move(C, State, Graph),
          sets:union(Acc, States2)
        end,
        sets:new(),
        States),
      States2 = epsilon_closure(Moved, Graph),
      Pos2 = Pos + 1,
      {States2, Pos2, max_pos(States2, Stop, Pos2, Max)}
    end,
    {States, 0, max_pos(States, Stop, 0, -1)},
    S),
  if Max >= 0 -> {match, [{0, Max}]};
     true -> nomatch
  end.


move(C, State, Graph) ->
  Edges = graph:edges_from(State, Graph),
  ActiveEdges = lists:filter(
    fun (Edge) ->
      match(C, graph:edge_label(Edge))
    end,
    Edges),
  NextStates = lists:map(fun graph:edge_target/1, ActiveEdges),
  sets:from_list(NextStates).


match(_, epsilon) ->
  false;
match(C1, {char, C2}) ->
  C1 =:= C2;
match(C, {range, Start, Stop}) ->
  C >= Start andalso C =< Stop;
match(C, {class, ClassSpec}) ->
  lists:any(fun (Spec) -> match(C, Spec) end, ClassSpec).


run_regexp(S, Ast) ->
  Pattern = from_regexp(Ast),
  run(S, Pattern).


ab_star_test() ->
  Ast = {star, {seq, [
    {char, $a},
    {char, $b}
  ]}},
  ?assertEqual({match, [{0, 4}]}, run_regexp("abab", Ast)),
  ?assertEqual({match, [{0, 0}]}, run_regexp("b", Ast)),
  ?assertEqual({match, [{0, 0}]}, run_regexp("", Ast)).


substring_match_test() ->
  Ast = {star, {seq, [
    {char, $a},
    {char, $b}
  ]}},
  ?assertEqual({match, [{0, 4}]}, run_regexp("ababc", Ast)).


no_match_seq_test() ->
  Ast = {seq, [
    {char, $a},
    {char, $b}
  ]},
  ?assertEqual(nomatch, run_regexp("ac", Ast)).


char_class_test() ->
  Ast = {seq, [
    {char, $a},
    {class, [{range, $5, $9}, {char, $_}]}
  ]},
  ?assertEqual({match, [{0, 2}]}, run_regexp("a5", Ast)),
  ?assertEqual({match, [{0, 2}]}, run_regexp("a7", Ast)),
  ?assertEqual({match, [{0, 2}]}, run_regexp("a9", Ast)),
  ?assertEqual({match, [{0, 2}]}, run_regexp("a_", Ast)),
  ?assertEqual(nomatch, run_regexp("a4", Ast)),
  ?assertEqual(nomatch, run_regexp("a-", Ast)),
  ?assertEqual(nomatch, run_regexp("ab", Ast)).
