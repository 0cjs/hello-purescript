module Expr where

import Prelude
import Data.String (uncons)
import Data.Maybe (Maybe(Just, Nothing))

data Expr
    = Equal String
    | Prefix String

eval :: Expr -> String -> Prim.Boolean
eval (Equal x) y        = x == y
eval (Prefix "") _      = true
eval (Prefix _) ""      = false
{- All I want is...
    eval (Prefix (x:xs)) (y:ys) =
        | x /= y    = false
        | otherwise = eval (Prefix xs) ys 
-}
eval (Prefix xxs) yys   =
    case uncons xxs of
        Nothing -> false
        Just { head:x, tail:xs } ->
            | x /= y    = false
            | otherwise = eval (Prefix xs) ys
    where
        { y:Char, ys:String } = case uncons yys of
            Nothing  -> { '\0', "" }
            Just yys -> yys

