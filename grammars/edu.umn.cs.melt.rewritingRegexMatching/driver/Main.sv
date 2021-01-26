grammar edu:umn:cs:melt:rewritingRegexMatching:driver;

imports silver:langutil;
imports silver:langutil:pp;

imports edu:umn:cs:melt:rewritingRegexMatching:concretesyntax;
imports edu:umn:cs:melt:rewritingRegexMatching:abstractsyntax;

parser parse::Regex_c {
  edu:umn:cs:melt:rewritingRegexMatching:concretesyntax;
}

function main
IOVal<Integer> ::= args::[String] ioIn::IO
{
  local fileName :: String = head(args);
  local result::IOMonad<Integer> = do {
    if length(args) < 1 || length(args) > 2 then do {
      printM("Usage: java -jar regex.jar [regex] (string)\nIf a string is not provided, input is read from stdin.\n");
      return 2;
    } else do {
      let regex::String = head(args);
      text::String <-
        if null(tail(args))
        then readFileM("/dev/stdin")
        else pure(head(tail(args)));
      let result :: ParseResult<Regex_c> = parse(regex, fileName);
      if !result.parseSuccess then do {
        printM(result.parseErrors ++ "\n");
        return 3;
      } else do {
        let ast::Regex = result.parseTree.ast;
        --printM(hackUnparse(stringToChars(text)) ++ "\n");
        --printM(hackUnparse(ast) ++ "\n");
        if matches(ast, text) then do {
          printM("Match success\n");
          return 0;
        } else do {
          printM("Match failure\n");
          return 1;
        };
      };
    };
  };
  
  return evalIO(result, ioIn);
}
