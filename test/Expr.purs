module Test.Expr where

import Prelude (($), bind)
import Prelude as P
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

import Expr


is      :: forall t1. Boolean -> Aff t1 P.Unit
is       = Assert.equal true
aint    :: forall t1. Boolean -> Aff t1 P.Unit
aint     = Assert.equal false


testExpr :: String -> Expr
testExpr s = Equal "foo" s || Prefix "bar" s

main :: forall t1.
    Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | t1) P.Unit
main = runTest do
    suite "eval" do
        test "equal"        $ is   $ eval ("abc" == "abc")
        test "!equal"       $ aint $ eval ("abc" == "def")
        test "prefix long"  $ is   $ eval (Prefix "abc" "abcde")
        test "or"           $ is   $ eval ((Prefix "abc" "abcde")
                                            || ("xyz" == "abcde"))
    suite "textExpr" do
        test "nothing"      $ aint $ eval (testExpr "nothing")
        test "foo"          $ is   $ eval (testExpr "foo")
        test "barbam"       $ is   $ eval (testExpr "barbam")
    suite "isPrefixOf" do
        test "prefix nothin"$ is   $    "" `isPrefixOf`  ""
        test "prefix short" $ aint $ "abc" `isPrefixOf`  "ab"
        test "prefix equal" $ is   $ "abc" `isPrefixOf`  "abc"
        test "prefix long"  $ is   $ "abc" `isPrefixOf`  "abcde"
        test "prefix wrong" $ aint $ "abx" `isPrefixOf`  "abcde"
