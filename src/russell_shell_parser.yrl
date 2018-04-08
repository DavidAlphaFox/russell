Nonterminals line command stmt symbols symbol.
Terminals atom var ignore cmd.
Rootsymbol line.

line -> stmt:
  {step, '$1'}.

line -> command:
  {cmd, '$1'}.

stmt -> atom symbols:
  {mk_symbol('$1'), '$2'}.

command -> cmd symbols:
  {mk_symbol('$1'), '$2'}.

symbols -> symbols symbol:
  '$1' ++ ['$2'].

symbols -> '$empty':
  [].

symbol -> atom:
  mk_symbol('$1').

symbol -> var:
  mk_symbol('$1').

symbol -> ignore:
  mk_symbol('$1').


Erlang code.

mk_symbol({atom, Line, A}) ->
    {A, Line};
mk_symbol({var, Line, V}) ->
    {V, Line};
mk_symbol({ignore, Line}) ->
    {'_', Line};
mk_symbol({cmd, Line, C}) ->
    {C, Line}.
