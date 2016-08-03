module Main where

import           Control.Monad (forever)
import           Repl          (replLoop)


main :: IO ()
main = do
  -- repl
  replLoop
