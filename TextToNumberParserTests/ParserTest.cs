using NUnit.Framework;
using Should;
using TextToNumberParser;

namespace TextToNumberParserTests
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void ParseGrammar_Regex_Works()
        {
            MainParser.Evaluate("1").ShouldEqual(1);
        }
    }
}
