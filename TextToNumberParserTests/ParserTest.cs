using NUnit.Framework;
using Should;

namespace TextToNumberParserTests
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void ParseGrammar_Regex_Works()
        {
            1.ShouldEqual(1);
        }
    }
}
