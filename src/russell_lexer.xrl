Definitions.

Atom       = [^_A-Z():,.\000-\s][^():,\000-\s]*
Variable   = [A-Z][^():,.\000-\s]*
Ignore     = _[^():,\000-\s]*
Whitespace = [\000-\s]+
Reserved   = [():,.]

Rules.

{Reserved}   : {token, {list_to_atom(TokenChars), TokenLine}}.
{Variable}   : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{Ignore}     : {token, {ignore, TokenLine}}.
{Atom}       : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{Whitespace} : skip_token.

Erlang code.
