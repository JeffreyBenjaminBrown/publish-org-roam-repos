module Test where

import Test.HUnit
import qualified Test.Index
import qualified Test.Parse


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ Test.Index.allTests
  , Test.Parse.allTests
  ]
