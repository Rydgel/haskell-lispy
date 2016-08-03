module Primitives
    ( primitives
    ) where

import           Core

-- Primitives, implemented in terms of haskell
primitives :: [(String, [Sexpr] -> Sexpr)]
primitives = [("eq", eq),
              ("null?", nullq),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("length", length'),
              ("list", List),
              ("*", numericBinop (*)),
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("/", numericBinop div),
              ("/=", numBoolBinop (/=)),
              ("<", numBoolBinop (<)),
              ("<=", numBoolBinop (<=)),
              ("=", numBoolBinop (==)),
              (">", numBoolBinop (>)),
              (">=", numBoolBinop (>=)),
              -- [verify] Get quotient from stdlib
              ("quotient", numericBinop mod),
              ("quot", numericBinop quot),
              ("quote", head),
              ("rem", numericBinop rem)]

-- [todo] - Add a prelude file which can have pure lisp definitions
-- [todo] - Define stdlib in pure lisp when possible
-- [todo] - Add haskell primitives to default env and remove applyPrimitive
-- Cond spec http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Conditionals.html

-- Lisp primitives
eq :: [Sexpr] -> Sexpr
eq [a, b] = Bool $ a == b
eq x = error $ "eq expected 2 arguments" ++ show x

nullq :: [Sexpr] -> Sexpr
nullq [a] = Bool $ a == List []
nullq x = error $ "null? expected 2 arguments" ++ show x

car :: [Sexpr] -> Sexpr
car [List (t : _)] = t
car x = error $ "car expected a single list, got " ++ show x

cdr :: [Sexpr] -> Sexpr
cdr [List (_ : t)] = List t
cdr x = error $ "cdr expected a single list, got " ++ show x

cons :: [Sexpr] -> Sexpr
cons (h : [List t]) = List (h:t)
cons x = error $ "cons expected a value and a list, got " ++ show x

length' :: [Sexpr] -> Sexpr
length' [List x] = Number $ toInteger $ length x
length' x = error $ "length expected a single list, got " ++ show x

-- [todo] Add input type to error message
-- [todo] Possibly auto generate unpack*
unpackNum :: Sexpr -> Integer
unpackNum (Number n) = n
unpackNum _ =  error "Unable to convert to number"

-- `numericBinop` takes a primitive Haskell function and wraps it with code to
-- unpack an argument list, apply the function to it, and wrap the result up in
-- LispVal Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [Sexpr] -> Sexpr
numericBinop op params = Number $ foldl1 op $ map unpackNum params

numBoolBinop :: (Integer -> Integer -> Bool) -> [Sexpr] -> Sexpr
numBoolBinop op [Number one, Number two] = Bool (one `op` two)
numBoolBinop _ _  = error "Unexpected arguments to numeric binary operator"
