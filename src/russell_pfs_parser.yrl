Nonterminals pf steps nstep step stmts stmt symbols symbol substs subst tokens token.
Terminals atom var ignore ':' ',' '(' ')' '.'.
Rootsymbol pf.

pf -> atom symbols ':' steps '.':
  {mk_symbol('$1'), '$2', '$4'}.

steps -> steps ',' nstep:
  '$1' ++ ['$3'].

steps -> nstep:
  ['$1'].

nstep -> symbol step:
    {'$1', '$2'}.

step -> '.' atom stmts:
  {apply, mk_symbol('$2'), '$3'}.

step -> atom substs:
  {subst, mk_symbol('$1'), '$2'}.

step -> atom:
  {subst, mk_symbol('$1'), []}.

step -> step ':' atom stmts:
  {apply, mk_symbol('$3'), ['$1'|'$4']}.

stmts -> stmts stmt:
  '$1' ++ ['$2'].

stmts -> '$empty':
  [].

stmt -> '(' step ')':
  '$2'.

stmt -> symbol:
  '$1'.

stmt -> ignore:
  mk_symbol('$1').

symbols -> symbols symbol:
  '$1' ++ ['$2'].

symbols -> '$empty':
  [].

symbol -> atom:
  mk_symbol('$1').

symbol -> var:
  mk_symbol('$1').

substs -> substs subst:
  '$1' ++ ['$2'].

substs -> subst:
  ['$1'].

subst -> var ':' token:
  {mk_symbol('$1'), '$3'}.

tokens -> tokens token:
  '$1' ++ ['$2'].

tokens -> token:
  ['$1'].

token -> atom:
  mk_token('$1').

token -> var:
  mk_token('$1').

token -> '(' tokens ')':
  '$2'.

Erlang code.

mk_symbol({atom, Line, A}) ->
    {A, Line};
mk_symbol({var, Line, V}) ->
    {V, Line};
mk_symbol({ignore, Line}) ->
    {'_', Line}.

mk_token({atom, _, A}) ->
    A;
mk_token({var, _, V}) ->
    {var, V}.
