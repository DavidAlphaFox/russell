Nonterminals forms form stmts stmt name def assert tokens token.
Terminals symbol '(' ')' ':' '@' '.' '|-' '.Pp' '.Df'.
Rootsymbol forms.

token -> symbol:
  mk_token('$1').

token -> '(' tokens ')':
  '$2'.

tokens -> tokens token:
  '$1' ++ ['$2'].

tokens -> '$empty':
  [].

name -> '@' symbol:
  {name, line_of('$1'), '$2'}.

def -> token ':' token:
  {def, line_of('$2'), '$1', '$3'}.

assert -> '|-' token:
  {assert, line_of('$1'), '$2'}.

stmt -> def:
  '$1'.

stmt -> assert:
  '$1'.

stmts -> stmts stmt:
  '$1' ++ ['$2'].

stmts -> '$empty':
   [].

form -> symbol ':' token:
  {var, line_of('$2'), '$1', '$3'}.

form -> name '.Df' def :
  {df, line_of('$2'), '$1', '$3'}.

form -> name '.Pp' stmts '.' stmt:
  {pp, line_of('$2'), '$1', '$3', '$5'}.

forms -> forms form:
  '$1' ++ ['$2'].

forms -> '$empty':
  [].

Erlang code.

line_of(X) ->
    element(2, X).

mk_token({symbol, _, S}) ->
    S.
