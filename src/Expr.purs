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

eval (Prefix s1) s2 = case uncons s1, uncons s2 of
    Just { head: c1, tail: cs1 }, Just { head: c2, tail: cs2 }
      | c1 == c2  -> eval (Prefix cs1) cs2
      | otherwise -> false
    _, _ -> false
