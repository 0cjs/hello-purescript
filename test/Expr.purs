module Test.Expr where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

import Expr (Expr(..), eval)

main :: forall t1.
    Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | t1) Unit
main = runTest do
    suite "eval" do
        test "equal"        $ is   $ eval (Equal "abc") "abc"
        test "!equal"       $ aint $ eval (Equal "abc") "def"
        test "prefix nothin"$ is   $ eval (Prefix "") ""
        test "prefix short" $ aint $ eval (Prefix "abc") "ab"
        test "prefix equal" $ is   $ eval (Prefix "abc") "abc"
        test "prefix long"  $ is   $ eval (Prefix "abc") "abcde"
        test "prefix wrong" $ aint $ eval (Prefix "abx") "abcde"
    where
        is   = Assert.equal true
        aint = Assert.equal false
