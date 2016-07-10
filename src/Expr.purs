module Expr (
    Value(..), Expr(..), (==), (||),
    eval, isPrefixOf
) where

import Prelude as P
import Prelude (otherwise)
import Data.String as S
import Data.Maybe (Maybe(Just, Nothing))

----------------------------------------------------------------------
-- Values

data Value = Const String

devalue :: Value -> String
devalue (Const x) = x

----------------------------------------------------------------------
-- Expressions

data Expr
    = False | True | Not Expr
    | Equal Value Value
    | Prefix Value Value
    | Or Expr Expr

infix 4 Equal as ==
infix 2 Or as ||

----------------------------------------------------------------------
-- Evaluation

eval :: Expr -> Prim.Boolean
eval False              = false
eval True               = true
eval (Not x)            = P.not (eval x)
eval (x == y)           = P.(==) (devalue x) (devalue y)
eval (Prefix xxs yys)   = case S.stripPrefix (devalue xxs) (devalue yys) of
                            Just _  -> true
                            Nothing -> false
eval (e1 || e2)        = P.(||) (eval e1) (eval e2)

----------------------------------------------------------------------
-- Utilities

-- This is not actually the way we'd want to do something like this in
-- a real PS/JS program, but more an exercise in how you do this in the
-- way that makes sense in Haskell or ML.
--
isPrefixOf :: String -> String -> Prim.Boolean
isPrefixOf xxs yys =
    case S.uncons xxs, S.uncons yys of
        Nothing, _       -> true
        _,       Nothing -> false
        Just { head:x, tail:xs }, Just { head:y, tail:ys }
            | P.(/=) x y -> false
            | otherwise  -> isPrefixOf xs ys
