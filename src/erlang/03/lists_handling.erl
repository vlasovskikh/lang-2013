-module(lists_handling).
-author("vlan").
-export([length_rec/1, length_iter/1, reverse/1]).


% Our own ways to define a linked list, see
% http://www.erlang.org/doc/reference_manual/typespec.html


-type untyped_list() :: nil | {pair, any(), untyped_list()}.


-type typed_list(T) :: nil | {pair, T, typed_list(T)}.


-type pair(T1, T2) :: {pair, T1, T2}.
-type paired_list(T) :: nil | pair(T, paired_list(T)).


% Functions over lists are defined using structural recursion:
%
% * The empty list is its base case
% * Head + Tail is the general case (inductive step in induction)
%
% Structural recursion and its companion, structural induction, are
% generalized versions of mathematical induction over natural
% numbers.


% Recursive version of length, it is based on structural recursion


-spec length_rec([T]) -> integer().


length_rec([]) ->
    0;
length_rec([_|Xs]) ->
    length_rec(Xs) + 1.


% Iterative version of length that uses tail call optimization, it
% also uses structural recursion along with extra parameters that
% maintain the iteration state.


length_iter(Xs) -> length_iter(Xs, 0).


length_iter([], Acc) ->
    Acc;
length_iter([_|Xs], Acc) ->
    length_iter(Xs, Acc + 1).


% Pay some attention to this reverse function to get the idea how
% the list is actually reversed.


reverse(List) -> reverse(List, []).


reverse([], Acc) ->
  Acc;
reverse([X|Xs], Acc) ->
  reverse(Xs, [X|Acc]).
