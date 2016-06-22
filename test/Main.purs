module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

import Main (makeGreeting)

main :: forall t1.
    Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | t1) Unit
main = runTest
 do suite "sample tests"
     do test "arithmetic"
         do Assert.assert "2 + 2 should be 4" $ (2 + 2) == 4
        test "more tests"
         do Assert.equal (2 + 2) 4
            Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal (2 + 2) 5
    suite "Main"
     do test "greeting"
         do Assert.equal "Hello, sailor!" $ makeGreeting "sailor"
