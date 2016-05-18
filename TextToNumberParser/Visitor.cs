using Antlr4.Runtime.Misc;

namespace TextToNumberParser
{
    class NumberVisitor : TextNumberBaseVisitor<int>
    {
        public override int VisitNumber([NotNull] TextNumberParser.NumberContext context)
        {
            return base.VisitNumber(context);
        }

        public override int VisitCompileUnit([NotNull] TextNumberParser.CompileUnitContext context)
        {
            return base.VisitCompileUnit(context);
        }

        public override int VisitDigitExpression([NotNull] TextNumberParser.DigitExpressionContext context)
        {
            return base.VisitDigitExpression(context);
        }

    }
}