module Test where

import Test.HUnit
import qualified Tests.Analyze
import qualified Tests.Analyze.OneLine


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ Tests.Analyze.allTests
  , Tests.Analyze.OneLine.allTests
  ]
