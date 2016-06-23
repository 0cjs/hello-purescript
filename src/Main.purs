module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console 
import Node.Yargs (argv)
import Data.Maybe
import Data.Array

makeGreeting :: Maybe String -> String
makeGreeting (Just name) = "Hello, " <> name <> "!"
makeGreeting Nothing = "Hello, friend!"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log(makeGreeting (head argv))
