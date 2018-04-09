Nonterminals pf stmts stmt symbols symbol.
Terminals atom var ':' ',' '.'.
Rootsymbol pf.

pf -> atom symbols ':' stmts '.':
  {mk_symbol('$1'), '$2', '$4'}.

stmts -> stmts ',' stmt:
  '$1' ++ ['$3'].

stmts -> stmt:
  ['$1'].

stmt -> symbol atom symbols:
  {mk_symbol('$2'), '$3', '$1'}.

symbols -> symbols symbol:
  '$1' ++ ['$2'].

symbols -> '$empty':
  [].

symbol -> atom:
  mk_symbol('$1').

symbol -> var:
  mk_symbol('$1').


Erlang code.

mk_symbol({atom, Line, A}) ->
    {A, Line};
mk_symbol({var, Line, V}) ->
    {V, Line}.
