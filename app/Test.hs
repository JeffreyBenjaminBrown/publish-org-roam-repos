module Test where

import Test.HUnit
import qualified Tests.Analyze


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ Tests.Analyze.allTests
  ]
