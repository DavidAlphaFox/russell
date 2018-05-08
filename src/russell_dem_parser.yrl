Nonterminals forms form stmts stmt dem abbr1 step bracket rules rule op dot colon num substs subst names name def assert tokens token.
Terminals symbol '(' ')' '[' ']' ':' '@' '.' '|-' '.Pp' '.Df' '.Dem' '.Prop'.
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

dot -> dot '.':
  '$1' + 1.

dot -> '.':
  1.

colon -> colon ':':
  '$1' + 1.

colon -> ':':
  1.

op -> dot:
  {1, '$1'}.

op -> colon:
  {2, '$1'}.

rules -> rule op rules:
  ['$1', '$2'|'$3'].

rules -> rule:
  ['$1'].

bracket -> '[' rules ']':
  '$2'.

abbr1 -> abbr1 bracket symbol token:
  '$1' ++ ['$3', {'$2', '$4'}].

abbr1 -> bracket assert:
  [{'$1', '$2'}].

step -> abbr1:
  '$1'.

dem -> step num dem:
  [{'$2', '$1'}|'$3'].

dem -> step:
  ['$1'].

dem -> bracket '|-' '.Prop':
  [[{'$1', '$3'}]].

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

form -> name stmts '.' stmt '.Dem' dem:
  {dem, line_of('$1'), '$1', '$2', '$4', '$6'}.

forms -> forms form:
  '$1' ++ ['$2'].

forms -> '$empty':
  [].

Erlang code.

line_of(X) ->
    element(2, X).

mk_token({symbol, _, S}) ->
    S.
