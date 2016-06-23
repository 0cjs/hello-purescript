module Test.Expr where

import Prelude (($), bind)
import Prelude as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

import Expr

main :: forall t1.
    Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | t1) P.Unit
main = runTest do
    suite "eval" do
        test "equal"        $ is   $ eval (Equal "abc") "abc"
        test "!equal"       $ aint $ eval (Equal "abc") "def"
        test "prefix long"  $ is   $ eval (Prefix "abc") "abcde"
        test "or"           $ is   $
                              eval ((Prefix "abc") .| (Equal "xyz")) "abcde"
    suite "isPrefixOf" do
        test "prefix nothin"$ is   $    "" `isPrefixOf`  ""
        test "prefix short" $ aint $ "abc" `isPrefixOf`  "ab"
        test "prefix equal" $ is   $ "abc" `isPrefixOf`  "abc"
        test "prefix long"  $ is   $ "abc" `isPrefixOf`  "abcde"
        test "prefix wrong" $ aint $ "abx" `isPrefixOf`  "abcde"
    where
        is   = Assert.equal true
        aint = Assert.equal false
