module Test where

import Test.HUnit
import qualified Test.Analyze
import qualified Test.Analyze.OneLine
import qualified Test.Parse


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ Test.Analyze.allTests
  , Test.Analyze.OneLine.allTests
  , Test.Parse.allTests
  ]
