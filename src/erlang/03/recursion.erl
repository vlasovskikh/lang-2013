-module(recursion).
-author("vlan").
-export([fact/1, fact_iter/1, inf/0]).


% # Compilation and Help
%
% * erlc, c(), [export_all], Module:module_info/0
% * #!/usr/bin/env escript, main(Args)
% * help(), f(Variable)
% * erl -man Module
% * http://erlang.org/doc/ http://erldocs.com/


% # Recursion and Iteration
%
% * No variables, no loops, no explicit iteration

% Mathematical recursive factorial


-spec fact(integer()) -> integer().


fact(0) ->
  1;
fact(X) when X > 0 ->
  X * fact(X - 1).


% fact(4)
% 4 * fact(3)
% 4 * (3 * fact(2))
% 4 * (3 * (2 * fact(1))
% 4 * (3 * (2 * (1 * fact(0)))
% 4 * (3 * (2 * (1 * 1)))
% 4 * (3 * (2 * 1))
% 4 * (3 * 2)
% 4 * 6
% 24


% O(N), O(N)


% Iterative factorial


fact_iter(X) -> fact_iter(X, 1).


fact_iter(0, Acc) ->
  Acc;
fact_iter(X, Acc) when X > 0 ->
  fact_iter(X - 1, X * Acc).


% fact_iter(4)
% fact_iter(4, 1)
% fact_iter(3, 4)
% fact_iter(2, 12)
% fact_iter(1, 24)
% fact_iter(0, 24)
% 24


% O(N), O(1)


% # Tail recursion
%
% * The last computation is a function call expression
% * Transform non-tail to tail recursion using accumulation in
%   parameters


% This function results in a StackOverflowException in Java, but
% works fine in Erlang.


inf() -> inf().
