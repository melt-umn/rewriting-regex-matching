grammar edu:umn:cs:melt:rewritingRegexMatching:abstractsyntax;

imports silver:core hiding empty, alt;
imports silver:rewrite;
imports silver:langutil;
imports silver:langutil:pp;

synthesized attribute nullable::Boolean;

inherited attribute wrt::Integer; -- Char
synthesized attribute deriv::Regex;

equality attribute isEqualTo, isEqual;

-- Simplification using term rewriting
global simpl::Strategy =
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
    | alt(r1, r2) when decorate r1 with { isEqualTo = decorate r2 with {}; }.isEqual -> r1
    | star(empty()) -> epsilon()
    | star(epsilon()) -> epsilon()
    end);

-- Simplification using strategy attributes
strategy attribute simpl =
  innermost(
    rule on Regex of
    | seq(empty(), r) -> empty()
    | seq(r, empty()) -> empty()
    | seq(epsilon(), r) -> ^r
    | seq(r, epsilon()) -> ^r
    | alt(empty(), r) -> ^r
    | alt(r, empty()) -> ^r
    | alt(epsilon(), r) when r.nullable -> ^r
    | alt(r, epsilon()) when r.nullable -> ^r
    | alt(r1, r2) when decorate ^r1 with { isEqualTo = r2; }.isEqual -> ^r1
    | star(empty()) -> epsilon()
    | star(epsilon()) -> epsilon()
    end);

{- Alternative way deriv could be defined with strategy attributes.
 - This may be considered less idiomatic than writing explicit equations as
 - cases are needed for all productions except empty and alt, thus this
 - attribute is minimally "strategic".
strategy attribute deriv =
  allTopDown(
    rule on top::Regex of
    | epsilon() -> empty()
    | char(c) -> if c == top.wrt then epsilon() else empty()
    | charRange(l, u) -> if l <= top.wrt && top.wrt <= u then epsilon() else empty()
    | negChars(r) -> if r.deriv.nullable then empty() else epsilon()
    | seq(r1, r2) ->
      alt(
        seq(r1.deriv, r2),
        if r1.nullable then r2.deriv else empty())
    | star(r) -> seq(r.deriv, top)
    end);
-}

strategy attribute simplDeriv = deriv <* simpl;

function matchStep
Regex ::= r::Regex c::Integer
{
  r.wrt = c;
  
  -- Simplification using term rewriting
  --return rewriteWith(simpl, r.deriv).fromJust;
  
  -- Simplification using strategy attributes
  return r.simplDeriv;
}

fun matches Boolean ::= r::Regex s::String =
  foldl(matchStep, r, stringToChars(s)).nullable;

nonterminal Regex with nullable, wrt, deriv, isEqualTo, isEqual, simpl, simplDeriv;

propagate isEqualTo, isEqual, simpl, simplDeriv on Regex;

abstract production empty
top::Regex ::=
{
  top.nullable = false;
  top.deriv = empty();
}

abstract production epsilon
top::Regex ::=
{
  top.nullable = true;
  top.deriv = empty();
}

abstract production char
top::Regex ::= c::Integer
{
  top.nullable = false;
  top.deriv = if c == top.wrt then epsilon() else empty();
}

abstract production charRange
top::Regex ::= l::Integer u::Integer
{
  top.nullable = false;
  top.deriv = if l <= top.wrt && top.wrt <= u then epsilon() else empty();
}

abstract production negChars
top::Regex ::= r::Regex
{
  top.nullable = false;
  top.deriv = if r.deriv.nullable then empty() else epsilon();
  r.wrt = top.wrt;
}

abstract production seq
top::Regex ::= r1::Regex r2::Regex
{
  top.nullable = r1.nullable && r2.nullable;
  top.deriv =
    alt(
      seq(r1.deriv, ^r2),
      if r1.nullable then r2.deriv else empty());
  r1.wrt = top.wrt;
  r2.wrt = top.wrt;
}

abstract production alt
top::Regex ::= r1::Regex r2::Regex
{
  top.nullable = r1.nullable || r2.nullable;
  top.deriv = alt(r1.deriv, r2.deriv);
  r1.wrt = top.wrt;
  r2.wrt = top.wrt;
}

abstract production star
top::Regex ::= r::Regex
{
  top.nullable = true;
  top.deriv = seq(r.deriv, ^top);
  r.wrt = top.wrt;
}
