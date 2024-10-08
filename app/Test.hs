module Test where

import Test.HUnit

import qualified Test.Anchor
import qualified Test.BuildIndex
import qualified Test.Parse
import qualified Test.Rewrite


allTests :: IO Counts
allTests = runTestTT $ TestList
  -- in order of usage
  [ Test.Parse.allTests
  , Test.BuildIndex.allTests
  , Test.Anchor.allTests
  , Test.Rewrite.allTests
  ]
