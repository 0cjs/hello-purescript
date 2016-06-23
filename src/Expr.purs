module Expr (
    Expr(..), eval, isPrefixOf
) where

import Prelude
import Data.String (uncons, stripPrefix)
import Data.Maybe (Maybe(Just, Nothing))

data Expr
    = Equal String
    | Prefix String

eval :: Expr -> String -> Prim.Boolean
eval (Equal x) y        = x == y
eval (Prefix xxs) yys   = case stripPrefix xxs yys of
                            Just _  -> true
                            Nothing -> false

isPrefixOf :: String -> String -> Prim.Boolean
isPrefixOf xxs yys =
    case uncons xxs, uncons yys of
        Nothing, _       -> true
        _,       Nothing -> false
        Just { head:x, tail:xs }, Just { head:y, tail:ys }
            | x /= y    -> false
            | otherwise -> isPrefixOf xs ys
