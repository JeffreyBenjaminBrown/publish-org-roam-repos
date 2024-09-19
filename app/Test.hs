module Test where

import Test.HUnit

import qualified Test.Index
import qualified Test.Parse
import qualified Test.Rewrite


allTests :: IO Counts
allTests = runTestTT $ TestList
  -- in order of usage
  [ Test.Parse.allTests
  , Test.Index.allTests
  , Test.Rewrite.allTests
  ]
