Nonterminals line atoms.
Terminals var atom.
Rootsymbol line.

line -> atom atoms:
  {'$1', '$2'}.

line -> var atoms:
  {'$1', '$2'}.

atoms -> atoms atom:
  '$1' ++ [mk_symbol('$2')].

atoms -> '$empty':
  [].

Erlang code.

mk_symbol({atom, Line, A}) ->
    {A, Line}.
