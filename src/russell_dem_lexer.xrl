Definitions.

Whitespace = [\000-\s]+
Symbol     = [^@:.()\[\]\000-\s]
Symbol2    = [^@:.()\[\]\000-\s][^@:()\[\]\000-\s]*[^@:.()\[\]\000-\s]
Reserved   = [@:.()\[\]]|\|-|.Df|.Pp|.Prop|.Dem

Rules.

{Reserved}   : {token, {list_to_atom(TokenChars), TokenLine}}.
{Symbol}     : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
{Symbol2}    : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
{Whitespace} : skip_token.

Erlang code.
