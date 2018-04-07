Nonterminals defs def stmts stmt tokens token.
Terminals atom var '(' ')' ':' '.' ','.
Rootsymbol defs.

defs -> defs def:
  '$1' ++ ['$2'].

defs -> '$empty':
  [].

def -> atom stmts ':' stmts '.':
  {mk_token('$1'), {'$2', '$4'}}.

stmts -> stmts stmt:
  '$1' ++ ['$2'].

stmts -> '$empty':
  [].

stmt -> tokens ',':
  '$1'.

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

mk_token({atom, _, A}) ->
    A;
mk_token({var, _, V}) ->
    {var, V}.
