Nonterminals forms form stmts stmt bracket rule num substs subst names name def assert tokens token.
Terminals symbol '(' ')' '[' ']' ':' '@' '.' '|-' '.Pp' '.Df'.
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

names -> names name:
  '$1' ++ ['$2'].

names -> name:
  ['$1'].

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

num -> '(' symbol ')':
  {num, line_of('$2'), '$2'}.

subst -> symbol token:
  {'$1', '$2'}.

substs -> substs subst:
  '$1' ++ ['$2'].

substs -> subst:
  ['$1'].

rule -> num:
  '$1'.

rule -> name:
  '$1'.

rule -> symbol:
  '$1'.

rule -> name substs:
  {subst, line_of('$1'), '$1', '$2'}.

rule -> symbol substs:
  {subst, line_of('$1'), '$1', '$2'}.

bracket -> '[' rule ']':
  '$2'.

form -> symbol ':' token:
  {var, line_of('$2'), '$1', '$3'}.

form -> name '.Df' def :
  {df, line_of('$2'), '$1', '$3'}.

form -> name '.Pp' stmts '.' stmt:
  {pp, line_of('$2'), '$1', '$3', '$5'}.

form -> symbol names '.':
  {alias, line_of('$1'), '$1', '$2'}.

form -> name stmts '.' stmt bracket:
  {prop, line_of('$1'), '$1', '$2', '$4', '$5'}.

forms -> forms form:
  '$1' ++ ['$2'].

forms -> '$empty':
  [].

Erlang code.

line_of(X) ->
    element(2, X).

mk_token({symbol, _, S}) ->
    S.
