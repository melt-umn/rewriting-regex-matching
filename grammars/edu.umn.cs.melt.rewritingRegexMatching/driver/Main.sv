grammar edu:umn:cs:melt:rewritingRegexMatching:driver;

imports core:monad;
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
  local result::IOMonad<Integer> = do (bindIO, returnIO) {
    if length(args) != 2 then {
      printM("Usage: java -jar regex.jar [regex] [string]\n");
      return 2;
    } else {
      regex::String = head(args);
      text::String = head(tail(args));
      result :: ParseResult<Regex_c> = parse(regex, fileName);
      if !result.parseSuccess then {
        printM(result.parseErrors ++ "\n");
        return 3;
      } else {
        ast::Regex = result.parseTree.ast;
        if matches(ast, text) then {
          printM("Match success\n");
          return 0;
        } else {
          printM("Match failure\n");
          return 1;
        }
      }
    }
  };
  
  return evalIO(result, ioIn);
}
