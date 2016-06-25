module Expr (
    Expr(..), (.|),
    eval, isPrefixOf
) where

import Prelude
import Data.String as S
import Data.Maybe (Maybe(Just, Nothing))

data Expr
    = Equal String
    | Prefix String
    | Or Expr Expr

infix 2 Or as .|

eval :: Expr -> String -> Prim.Boolean
eval (Equal x) y        = x == y
eval (Prefix xxs) yys   = case S.stripPrefix xxs yys of
                            Just _  -> true
                            Nothing -> false
eval (e1 .| e2) s      = eval e1 s || eval e2 s

isPrefixOf :: String -> String -> Prim.Boolean
isPrefixOf xxs yys =
    case S.uncons xxs, S.uncons yys of
        Nothing, _       -> true
        _,       Nothing -> false
        Just { head:x, tail:xs }, Just { head:y, tail:ys }
            | x /= y    -> false
            | otherwise -> isPrefixOf xs ys
