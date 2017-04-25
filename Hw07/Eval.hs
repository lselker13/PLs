module Eval where

import Control.Applicative
import Test.QuickCheck

import Data.Char
import Data.Map hiding (null,foldr, foldl)

import Data.Maybe
import Data.Set hiding (null, foldr, foldl)

import System.Environment
import System.Exit
import System.IO

import Parse

subst :: LExp -> VarName -> LExp -> LExp
subst (Var x) y e | x == y    = e
                  | otherwise = Var x
subst (Ap e1 e2) y e         = Ap (subst e1 y e) (subst e2 y e)
subst (Lambda x t e1) y e | x == y = Lambda x t e1
                        | otherwise = Lambda x t (subst e1 y e)
subst (Num n) _ _ = (Num n)

eval :: LExp -> Either error LExp
eval (Var x) = Left $ error $ "Unbound variable " ++ x
eval (Lambda v t e) = Right $ Lambda v t e
eval (Num n) = Right $ Num n
eval (Ap (Lambda v1 t1 e1) e@(Lambda v2 t2 e2)) = eval (subst e1 v1 e)
eval (Ap (Lambda v1 t1 e1) e@(Num x)) = eval (subst e1 v1 e)
eval e@(Ap (Num n) (Num m)) = Left $ error $ "Not a number: " ++ (show e)
eval (Ap e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  eval (Ap ee1 ee2)
-- Remove
eval _ = undefined

free :: LExp -> Set VarName
free (Var x) = Data.Set.singleton x
free (Lambda v t e) = Data.Set.delete v (free e)
free (Ap e1 e2) = Data.Set.union (free e1) (free e2)
free _ = error $ "Running free on nums/succs"
-- If Church or Check are in the map at all they are true


te1, te2, te3, te4, te5 :: String
te1 = "lambda x . x"
te2 = "(lambda y z a . a y z) (lambda c.c)"
te3 = "lambda y . y"
te4 = "lambda a b . a"
te5 = "(lambda x . x) z"
three = "let zero = lambda s z. z in let succ = lambda n. lambda s z. s (n s z) in succ (succ (succ zero))"
