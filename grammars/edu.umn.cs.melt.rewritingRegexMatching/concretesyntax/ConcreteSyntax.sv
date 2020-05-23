grammar edu:umn:cs:melt:rewritingRegexMatching:concretesyntax;

-- From silver:definition:regex

imports silver:langutil;
imports edu:umn:cs:melt:rewritingRegexMatching:abstractsyntax;

lexer class Operator;
lexer class Escape;

terminal Plus_t          '+' lexer classes { Operator };
terminal Kleene_t        '*' lexer classes { Operator };
terminal Optional_t      '?' lexer classes { Operator };
terminal Choice_t        '|' lexer classes { Operator };
terminal Range_t         '-' lexer classes { Operator };
terminal RegexNot_t      '^' lexer classes { Operator };
terminal RegexLBrack_t   '[' lexer classes { Operator };
terminal RegexRBrack_t   ']' lexer classes { Operator };
terminal RegexLParen_t   '(' lexer classes { Operator };
terminal RegexRParen_t   ')' lexer classes { Operator };
terminal RegexWildcard_t '.' lexer classes { Operator };
terminal RegexChar_t     /./ lexer classes { Escape };
terminal EscapedChar_t /\\./ lexer classes { Escape };

-- Disambiguate these, rather than using lexical precedence,
-- so we can avoid superfluous escapes (e.g. /--.*/).
-- This is the behavior of most regex libraries.
disambiguate RegexChar_t, Plus_t { pluck Plus_t; }
disambiguate RegexChar_t, Kleene_t { pluck Kleene_t; }
disambiguate RegexChar_t, Optional_t { pluck Optional_t; }
disambiguate RegexChar_t, Choice_t { pluck Choice_t; }
disambiguate RegexChar_t, Range_t { pluck Range_t; }
disambiguate RegexChar_t, RegexNot_t { pluck RegexNot_t; }
disambiguate RegexChar_t, RegexLBrack_t { pluck RegexLBrack_t; }
disambiguate RegexChar_t, RegexRBrack_t { pluck RegexRBrack_t; }
disambiguate RegexChar_t, RegexLParen_t { pluck RegexLParen_t; }
disambiguate RegexChar_t, RegexRParen_t { pluck RegexRParen_t; }
disambiguate RegexChar_t, RegexWildcard_t { pluck RegexWildcard_t; }


{--
 - A basic regular expression.
 -
 - At lowest precedence, a regex consists of a series of choices (a|b|c)
 -}
nonterminal Regex_c with ast<Regex>;

concrete production regexEpsilon
top::Regex_c ::=
{
  abstract epsilon;
}

concrete production regexSeq
top::Regex_c ::= h::RegexSeq
{
  top.ast = h.ast;
}

concrete production regexChoice
top::Regex_c ::= h::RegexSeq '|' t::Regex_c
{
  abstract alt;
}


{--
 - A sequence of regular expressions.
 -}
nonterminal RegexSeq with ast<Regex>;

concrete production regexSeqSnoc
top::RegexSeq ::= h::RegexSeq t::RegexRepetition
{
  abstract seq;
}

concrete production regexSeqOne
top::RegexSeq ::= t::RegexRepetition
{
  top.ast = t.ast;
}


{--
 - A RegexItem with an optional repetition operator (*+?)
 -}
nonterminal RegexRepetition with ast<Regex>;

concrete production regexKleene
top::RegexRepetition ::= i::RegexItem '*'
{
  abstract star;
}

concrete production regexPlus
top::RegexRepetition ::= i::RegexItem '+'
{
  top.ast = seq(i.ast, star(i.ast));
}

concrete production regexOptional
top::RegexRepetition ::= i::RegexItem '?'
{
  top.ast = alt(i.ast, epsilon());
}

concrete production regexOnce
top::RegexRepetition ::= i::RegexItem
{
  top.ast = i.ast;
}


{--
 - A single matched entity (char, group)
 -}
nonterminal RegexItem with ast<Regex>;      -- characters or sequences/sets

concrete production regexCharItem
top::RegexItem ::= c::RegexChar
{
  top.ast = char(c.ast);
}

concrete production regexWildcard
top::RegexItem ::= '.'
{
  top.ast = negChars(char(head(stringToChars("\n"))));
}

concrete production regexSet
top::RegexItem ::= '[' g::RegexCharSet ']'
{
  top.ast = g.ast;
}

concrete production regexSetInverted
top::RegexItem ::= '[' '^' g::RegexCharSet ']'
{
  top.ast = negChars(g.ast);
}

concrete production regexGroup
top::RegexItem ::= '(' r::Regex_c ')'
{
  top.ast = r.ast;
}


{--
 - A list of options or ranges within a regexSet.
 -}
nonterminal RegexCharSet with ast<Regex>;

concrete production regexCharSetSnoc
top::RegexCharSet ::= h::RegexCharSet  t::RegexCharSetItem
{
  top.ast = alt(h.ast, t.ast);
}

concrete production regexCharSetOne
top::RegexCharSet ::= t::RegexCharSetItem
{
  top.ast = t.ast;
}


{--
 - An option or range within a regexSet.
 -}
nonterminal RegexCharSetItem with ast<Regex>;

concrete production regexSetChar
top::RegexCharSetItem ::= c::RegexChar
{
  top.ast = char(c.ast);
}

concrete production regexSetRange
top::RegexCharSetItem ::= l::RegexChar '-' u::RegexChar
{
  top.ast = charRange(l.ast, u.ast);
}


{--
 - A character, escaped or otherwise.
 -}
nonterminal RegexChar with ast<Integer>;

concrete production regexChar
top::RegexChar ::= c::RegexChar_t
{
  top.ast = head(stringToChars(c.lexeme));
}

concrete production regexEscapedChar
top::RegexChar ::= esc::EscapedChar_t
{
  top.ast = head(tail(stringToChars(esc.lexeme)));
}
