module Tests.TestParser where

import Parser
import Tokenizer

import           Test.HUnit
import           Text.Heredoc

import Data.String.Utils


test_parseNumberCombineBigIntegerSmallInteger_NoAddition = TestCase $ do
  let tokenizedNumber = GivenNumber (IntegerNumber 3) (Combiner (GivenNumber (IntegerNumber 200) Nil))
      output = interpreter tokenizedNumber 0

  assertEqual "parsing '200 then 3' as string"
              "2003" output


test_parseNumberCombineSmallIntegerBigInteger_NoAddition = TestCase $ do
  let tokenizedNumber = GivenNumber (IntegerNumber 3) (GivenNumber (IntegerNumber 200) Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing '3 200' as string"
              "3200" output

test_parseNumberCombineTenners = TestCase $ do
  let tokenizedNumber = GivenNumber (IntegerNumber 56) (GivenNumber (IntegerNumber 20) Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing '56 20' as string"
              "5620" output


test_parseNumberCombineFractionals = TestCase $ do
  let tokenizedNumber = GivenNumber (FractionalNumber 1.2) (GivenNumber (FractionalNumber 3.4) Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing '1.2 3.4 as string"
              "1.2 3.4" output

test_parseNumberCombineLeftFractionRightInteger = TestCase $ do
  let tokenizedNumber = GivenNumber (FractionalNumber 1.2) (GivenNumber (IntegerNumber 3) Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing '1.2 3' as string"
              "1.2 3" output

test_parseNumberCombineLeftIntegerRightFractional = TestCase $ do
  let tokenizedNumber = GivenNumber (IntegerNumber 1) (GivenNumber (FractionalNumber 2.3) Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing '1 2.3' as string"
              "1 2.3" output

test_parseNumberCombineTennerAndSingleDigit = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "forty") (GivenNumber (StringNumber "two") Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'forty two' as string"
              "42" output

test_parseNumberZero = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "zero") Nil
      output = rstrip $ interpreter tokenizedNumber 0

  assertEqual "parsing 'zero' as string"
              "0" output

-- Small number in front a big number
test_parseNumberCombineSingleWithHundred = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'two 100' as string"
              "200" output


test_parseWrittenAndNumericNumbers = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (IntegerNumber 4) Nil)))

      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'Two hundred sixty 4' as string"
              "264" output

test_parseNumber = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (StringNumber "four") Nil))) 
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'two hundred sixty four' as string"
              "264" output


test_parseNumberAndWord = TestCase $ do
  let tokenizedSentence = GivenNumber (StringNumber "two") (NonNumericString "tigers" (NonNumericString "and" (GivenNumber (StringNumber "six") (NonNumericString "bears" Nil))))

  let output = rstrip $ interpreter tokenizedSentence 0
      in assertEqual "multiword and number" "2 tigers and 6 bears" output

tests :: Test
tests = TestList [
        TestLabel "test1" test_parseNumber
        , TestLabel "test1" test_parseNumberCombineSingleWithHundred
        , TestLabel "test1" test_parseNumberCombineTennerAndSingleDigit
        , TestLabel "test1" test_parseNumberAndWord
        , TestLabel "test1" test_parseWrittenAndNumericNumbers
        , TestLabel "test1" test_parseNumberZero
        , TestLabel "test1" test_parseNumberCombineLeftIntegerRightFractional
        , TestLabel "test1" test_parseNumberCombineLeftFractionRightInteger
        , TestLabel "test1" test_parseNumberCombineFractionals
        , TestLabel "test1" test_parseNumberZero
        , TestLabel "test1" test_parseNumberCombineBigIntegerSmallInteger_NoAddition
        , TestLabel "test1" test_parseNumberCombineSmallIntegerBigInteger_NoAddition
  ]
