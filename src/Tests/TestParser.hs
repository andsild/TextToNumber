module Tests.TestParser where

import Parser
import Tokenizer

import           Test.HUnit
import           Text.Heredoc

test_parseNumberCombineTennerAndSingleDigit = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "forty") (GivenNumber (StringNumber "two") Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'forty two' as string"
              "42" output


-- Small number in front a big number
test_parseNumberCombineSingleWithHundred = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'two hundred' as string"
              "200" output

test_parseNumber = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (StringNumber "four") Nil))) 
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'two hundred and sixty four' as string"
              "264" output

tests :: Test
tests = TestList [
        TestLabel "test1" test_parseNumber
        , TestLabel "test1" test_parseNumberCombineSingleWithHundred
        , TestLabel "test1" test_parseNumberCombineTennerAndSingleDigit
  ]
