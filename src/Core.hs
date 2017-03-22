{-# LANGUAGE OverloadedStrings #-}

module Core
  ( Sexpr (..)
  , Env
  ) where


data Sexpr
  = Atom String
  | List [Sexpr]
  | Function Env (Maybe String) [Sexpr] [Sexpr]
  | DottedList [Sexpr] Sexpr
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

type Env = [(String, Sexpr)]

-- instance Show Sexpr where
--   show (Atom x) = x
--   show (List x) =
--     case x of
--         Atom "quote" : _ -> "'" ++ unwords' (tail x)
--         _ -> "(" ++ unwords' x ++ ")"
--   show (DottedList h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
--   show (String s) = "\"" ++ s ++ "\""
--   show (Number n) = show n
--   show (Function _ (Just name) _ _) = "<λ " ++ name ++ " >"
--   show (Function _ Nothing _ _) = "<λ>"
--   show (Bool True) = "#t"
--   show (Bool False) = "#f"

instance Eq Sexpr where
  (==) (Atom a) (Atom b) = a == b
  (==) (List a) (List b) = a == b
  (==) (DottedList a b) (DottedList c d) = a == c && b == d
  (==) (String a) (String b) = a == b
  (==) (Number a) (Number b) = a == b
  (==) (Bool a) (Bool b) = a == b
  (==) Function{} Function{} = False
  (==) _a _b = False

-- Helpers
unwords' :: [Sexpr] -> String
unwords' = unwords . map show
