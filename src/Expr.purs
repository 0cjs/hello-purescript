module Expr (
    Environment, empty, Value(..),
    Expr(..), (==), (||),
    eval, isPrefixOf
) where

import Prelude as P
import Prelude (($), otherwise)
import Data.String as S
import Data.Maybe (Maybe(Just, Nothing))
import Data.Map as M

----------------------------------------------------------------------
-- Values

type Environment = M.Map String String

empty :: Environment
empty = M.empty

data Value = Const String

devalue :: Environment -> Value -> String
devalue _ (Const x) = x

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

eval :: Environment -> Expr -> Maybe Prim.Boolean
eval _ False            = Just $ false
eval _ True             = Just $ true
eval e (Not x)          = case (eval e x) of
                              Nothing    -> Nothing
                              Just y     -> Just y
eval e (x == y)         = Just $ P.(==) (devalue e x) (devalue e y)
eval e (Prefix xxs yys) = Just $ case S.stripPrefix
                                         (devalue e xxs) (devalue e yys) of
                                     Just _  -> true
                                     Nothing -> false
eval e (x || y)         = Just $ P.(||) (eval e x) (eval e y)

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
