module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

makeGreeting :: String -> String
makeGreeting (name) = "Hello, " <> name <> "!"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log(makeGreeting "sailor")
