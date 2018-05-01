Definitions.

Atom       = [^().\000-\s]+
Atom2      = [^().\000-\s][^()\000-\s]*[^().\000-\s]
Variable   = [.][^()\000-\s]*[^().\000-\s]
Whitespace = [\000-\s]+
Reserved   = [().]

Rules.

{Reserved}   : {token, {list_to_atom(TokenChars), TokenLine}}.
{Variable}   : {token, {var, TokenLine, list_to_atom(tl(TokenChars))}}.
{Atom}       : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{Atom2}      : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{Whitespace} : skip_token.

Erlang code.
