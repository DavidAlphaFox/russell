Nonterminals forms form stmts stmt tokens token steps step step0 atoms atoms0.
Terminals atom var '(' ')' '.'.
Rootsymbol forms.

forms -> forms form:
  '$1' ++ ['$2'].

forms -> '$empty':
  [].

form -> atom stmts '.' stmt:
  {def, mk_symbol('$1'), '$2', '$4'}.

form -> step0 steps '.':
  {proof, '$1', '$2'}.

stmts -> stmts stmt:
  '$1' ++ ['$2'].

stmts -> '$empty':
  [].

stmt -> tokens '.':
  '$1'.

tokens -> tokens token:
  '$1' ++ ['$2'].

tokens -> token:
  ['$1'].

token -> '(' tokens ')':
  '$2'.

token -> atom:
  mk_token('$1').

token -> var:
  mk_token('$1').

steps -> steps step:
  '$1' ++ ['$2'].

steps -> step:
  ['$1'].

step -> var atoms:
  {mk_symbol('$1'), '$2'}.

atoms -> atoms atom:
  '$1' ++ [mk_symbol('$2')].

atoms -> atom:
  [mk_symbol('$1')].

step0 -> var atoms0:
  {mk_symbol('$1'), '$2'}.

atoms0 -> atoms0 atom:
  '$1' ++ [mk_symbol('$2')].

atoms0 -> '$empty':
  [].


Erlang code.

mk_token({atom, _, A}) ->
    A;
mk_token({var, _, V}) ->
    {var, V}.

mk_symbol({atom, Line, A}) ->
    {A, Line};
mk_symbol({var, Line, V}) ->
    {V, Line}.
