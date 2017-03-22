module Repl
  ( replLoop
  ) where


import           Control.Monad
import qualified Data.Text     as T
import           Parser        (readExpr)
import           System.IO


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readLine :: String -> IO String
readLine prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pre prompt action = do
   result <- prompt
   unless (pre result) $ do
     action result
     until_ pre prompt action

replLoop :: IO ()
replLoop = do
  -- print Version and Exit Information
  putStrLn "Lispy Version 0.1.0.0"
  putStrLn "Press Ctrl+c or type quit to Exit\n"
  -- main loop
  until_ (== "quit") (readLine "lispy> ") $ \input -> do
    let ast = readExpr $ T.pack input
    print ast
