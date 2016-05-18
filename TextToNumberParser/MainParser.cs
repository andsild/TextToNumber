using Antlr4;
using Antlr4.Runtime;

namespace TextToNumberParser
{
    public static class MainParser
    {
        public static int Evaluate(string expression)
        {
            var lexer = new TextNumberLexer(new AntlrInputStream(expression));
            lexer.RemoveErrorListeners();
            lexer.AddErrorListener(new ThrowExceptionErrorListener());

            var tokens = new CommonTokenStream(lexer);
            var parser = new TextNumberParser(tokens);

            var tree = parser.compileUnit();

            var visitor = new TextNumberBaseVisitor<int>();

            return visitor.Visit(tree);
        }
    }
}