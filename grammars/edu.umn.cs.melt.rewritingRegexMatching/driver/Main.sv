grammar edu:umn:cs:melt:rewritingRegexMatching:driver;

imports silver:langutil;
imports silver:langutil:pp;

imports edu:umn:cs:melt:rewritingRegexMatching:concretesyntax;
imports edu:umn:cs:melt:rewritingRegexMatching:abstractsyntax;

parser parse::Regex_c {
  edu:umn:cs:melt:rewritingRegexMatching:concretesyntax;
}

fun main IO<Integer> ::= args::[String] = do {
  if length(args) < 1 || length(args) > 2 then do {
    print("Usage: java -jar regex.jar [regex] (string)\nIf a string is not provided, input is read from stdin.\n");
    return 2;
  } else do {
    let regex::String = head(args);
    text::String <-
      if null(tail(args))
      then readFile("/dev/stdin")
      else pure(head(tail(args)));
    let result :: ParseResult<Regex_c> = parse(regex, regex);
    if !result.parseSuccess then do {
      print(result.parseErrors ++ "\n");
      return 3;
    } else do {
      let ast::Regex = result.parseTree.ast;
      --print(hackUnparse(stringToChars(text)) ++ "\n");
      --print(hackUnparse(ast) ++ "\n");
      if matches(ast, text) then do {
        print("Match success\n");
        return 0;
      } else do {
        print("Match failure\n");
        return 1;
      };
    };
  };
};
