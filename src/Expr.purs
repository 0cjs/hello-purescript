module Expr (
    Environment, Expr(..), (||),
    eval, isPrefixOf
) where

import Prelude as P
import Prelude (otherwise)
import Data.String as S
import Data.Maybe (Maybe(Just, Nothing))


data Environment

readEnv :: Environment -> String -> String
readEnv e name = "XXX"


data Expr
    = Equal String
    | Prefix String
    | Or Expr Expr


infix 2 Or as ||

eval :: Expr -> Environment -> Prim.Boolean
eval (Equal x) env      = P.(==) x (readEnv env "x")
eval (Prefix xxs) env   = case S.stripPrefix xxs yys of
                            Just _  -> true
                            Nothing -> false
    where yys = readEnv env "x"
eval (e1 || e2) s      = P.(||) (eval e1 s) (eval e2 s)

isPrefixOf :: String -> String -> Prim.Boolean
isPrefixOf xxs yys =
    case S.uncons xxs, S.uncons yys of
        Nothing, _       -> true
        _,       Nothing -> false
        Just { head:x, tail:xs }, Just { head:y, tail:ys }
            | P.(/=) x y -> false
            | otherwise  -> isPrefixOf xs ys
