module Test where

import Test.HUnit


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ aTest
  ]

aTest :: Test
aTest = TestCase $ do
  assertBool "" $ True
