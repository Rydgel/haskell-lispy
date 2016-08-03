module Repl
    ( replLoop
    ) where


import           Control.Monad
import           System.IO


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readLine :: String -> IO String
readLine prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   unless (pred result) $ do
     action result
     until_ pred prompt action

replLoop :: IO ()
replLoop = do
  -- print Version and Exit Information
  putStrLn "Lispy Version 0.0.0.0.1"
  putStrLn "Press Ctrl+c or type quit to Exit\n"
  -- main loop
  until_ (== "quit") (readLine "lispy> ") $ \input ->
    putStrLn $ "No you're a " ++ input ++ "\n"
