module Expr (
    Expr(..), (==), (||),
    eval, isPrefixOf
) where

import Prelude as P
import Prelude (otherwise)
import Data.String as S
import Data.Maybe (Maybe(Just, Nothing))

data Expr
    = False | True | Not Expr
    | Equal String String
    | Prefix String String
    | Or Expr Expr

infix 4 Equal as ==
infix 2 Or as ||

eval :: Expr -> Prim.Boolean
eval False              = false
eval True               = true
eval (Not x)            = P.not (eval x)
eval (x == y)           = P.(==) x y
eval (Prefix xxs yys)   = case S.stripPrefix xxs yys of
                            Just _  -> true
                            Nothing -> false
eval (e1 || e2)        = P.(||) (eval e1) (eval e2)

isPrefixOf :: String -> String -> Prim.Boolean
isPrefixOf xxs yys =
    case S.uncons xxs, S.uncons yys of
        Nothing, _       -> true
        _,       Nothing -> false
        Just { head:x, tail:xs }, Just { head:y, tail:ys }
            | P.(/=) x y -> false
            | otherwise  -> isPrefixOf xs ys
