module Tests.TestNumbers where

import Numbers

import           Test.HUnit
import           Text.Heredoc

test_lookupNumberInList = TestCase $ do
  let number = "fortytwo"

  case elemIndex' number of
    Just n -> assertEqual "parsing 'forty two' as string" "42" 42
    Nothing -> assertFailure "parse error"


tests :: Test
tests = TestList [
        TestLabel "test1" test_lookupNumberInList
  ]
