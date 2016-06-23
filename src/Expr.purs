module Expr where

import Prelude
import Data.String (uncons)
import Data.Maybe (Maybe(Just, Nothing))

data Expr
    = Equal String
    | Prefix String

eval :: Expr -> String -> Prim.Boolean
eval (Equal x) y        = x == y
eval (Prefix xxs) yys   =
    case uncons xxs, uncons yys of
        Nothing, _       -> true
        _,       Nothing -> false
        Just { head:x, tail:xs }, Just { head:y, tail:ys } ->
            if  x /= y  then false
                        else eval (Prefix xs) ys
