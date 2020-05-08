grammar edu:umn:cs:melt:rewritingRegexMatching:abstractsyntax;

imports core:monad;
imports silver:langutil;
imports silver:langutil:pp;

synthesized attribute nullable::Boolean;

inherited attribute wrt::String;
synthesized attribute deriv::Regex;

inherited attribute isEqualTo::Regex;
synthesized attribute isEqual::Boolean;

strategy attribute simpl =
  innermost(
    rule on Regex of
    | seq(empty(), r) -> empty()
    | seq(r, empty()) -> empty()
    | seq(epsilon(), r) -> r
    | seq(r, epsilon()) -> r
    | alt(empty(), r) -> r
    | alt(r, empty()) -> r
    | alt(epsilon(), r) when r.nullable -> r
    | alt(r, epsilon()) when r.nullable -> r
    | alt(r1, r2) when decorate r1 with { isEqualTo = r2; }.isEqual -> r1
    | star(empty()) -> epsilon()
    | star(epsilon()) -> epsilon()
    end);

strategy attribute simplDeriv = deriv <* simpl;

function matches
Boolean ::= r::Regex s::String
{
  return
    foldl(
      \ r::Regex c::String -> decorate r with { wrt = c; }.simplDeriv,
      r, explode("", s)).nullable;
}

nonterminal Regex with nullable, wrt, deriv, isEqualTo, isEqual, simpl, simplDeriv;

propagate simpl, simplDeriv on Regex;

abstract production empty
top::Regex ::=
{
  top.nullable = false;
  top.deriv = empty();
  top.isEqual = case top.isEqualTo of empty() -> true | _ -> false end;
}

abstract production epsilon
top::Regex ::=
{
  top.nullable = true;
  top.deriv = empty();
  top.isEqual = case top.isEqualTo of epsilon() -> true | _ -> false end;
}

abstract production char
top::Regex ::= c::String
{
  top.nullable = false;
  top.deriv = if c == top.wrt then epsilon() else empty(); 
  top.isEqual = case top.isEqualTo of char(c1) -> c == c1 | _ -> false end;
}

abstract production seq
top::Regex ::= r1::Regex r2::Regex
{
  top.nullable = r1.nullable && r2.nullable;
  top.deriv =
    alt(
      seq(r1.deriv, r2),
      if r1.nullable then r2.deriv else empty());
  r1.wrt = top.wrt;
  r2.wrt = top.wrt;
  
  top.isEqual = case top.isEqualTo of seq(_, _) -> r1.isEqual && r2.isEqual | _ -> false end;
  r1.isEqualTo = case top.isEqualTo of seq(r, _) -> r end;
  r2.isEqualTo = case top.isEqualTo of seq(_, r) -> r end;
}

abstract production alt
top::Regex ::= r1::Regex r2::Regex
{
  top.nullable = r1.nullable || r2.nullable;
  top.deriv = alt(r1.deriv, r2.deriv);
  r1.wrt = top.wrt;
  r2.wrt = top.wrt;
  
  top.isEqual = case top.isEqualTo of alt(_, _) -> r1.isEqual && r2.isEqual | _ -> false end;
  r1.isEqualTo = case top.isEqualTo of alt(r, _) -> r end;
  r2.isEqualTo = case top.isEqualTo of alt(_, r) -> r end;
}

abstract production star
top::Regex ::= r::Regex
{
  top.nullable = true;
  top.deriv = seq(r.deriv, top);
  r.wrt = top.wrt;
  
  top.isEqual = case top.isEqualTo of star(_) -> r.isEqual | _ -> false end;
  r.isEqualTo = case top.isEqualTo of star(r) -> r end;
}
