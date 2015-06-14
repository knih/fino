module Main where

import Data.List
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import ParserTest as P
import EvalTest as E
import InferTest as I

main = defaultMain $ hUnitTestToTests $ Main.tests

tests = TestList $ parserTests ++ evalTests ++ inferTests
