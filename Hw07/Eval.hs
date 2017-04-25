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



--data LExp =
--    | LetRec VarName Type LExp LExp

--let x = e1 in e2
--e2 -> x -> e1 
subst :: LExp -> VarName -> LExp -> LExp
subst (Var x) y e | x == y    = e
                  | otherwise = Var x
subst (Ap e1 e2) y e = Ap (subst e1 y e) (subst e2 y e)
subst (Lambda x t e1) y e | x == y    = Lambda x t e1
                          | otherwise = Lambda x t (subst e1 y e)
subst e@(Num _) _ _ = e
subst e@(Bool _) _ _ = e
subst (If e1 e2 e3) y e = If (subst e1 y e) (subst e2 y e) (subst e3 y e)
subst (Pair e1 e2) y e = Pair (subst e1 y e) (subst e2 y e)
subst (Plus e1 e2) y e = Plus (subst e1 y e) (subst e2 y e)
subst (Minus e1 e2) y e = Minus (subst e1 y e) (subst e2 y e)
subst (Times e1 e2) y e = Times (subst e1 y e) (subst e2 y e)
subst (Divide e1 e2) y e = Divide (subst e1 y e) (subst e2 y e)
subst (And e1 e2) y e = And (subst e1 y e) (subst e2 y e)
subst (Or e1 e2) y e = Or (subst e1 y e) (subst e2 y e)
subst (Lt e1 e2) y e = Lt (subst e1 y e) (subst e2 y e)
subst (Eqq e1 e2) y e = Eqq (subst e1 y e) (subst e2 y e)
subst (Neg e1) y e = Neg $ subst e1 y e
subst (Not e1) y e = Not $ subst e1 y e
subst (Fst e1) y e = Fst $ subst e1 y e
subst (Snd e1) y e = Snd $ subst e1 y e
subst (Typed e1 t) y e = subst e1 y e
subst (Let x e1 e2) y e = subst (subst e2 x e1) y e

{-
data LExp =
    | LetRec VarName Type LExp LExp
-}

data Error =
    UnboundVariable VarName
  | UndefinedApplication
  | InvalidBinop LExp {-invalid expression-}
  | InvalidBoolean LExp{-invalid expresssion-}
  | InvalidType LExp {-invalid expression-}
  deriving Show

eval :: LExp -> Either Error LExp
eval (Var x) = Left $ UnboundVariable x
eval (Lambda v t e) = Right $ Lambda v t e
eval (Num x) = Right $ Num x
eval (Bool b) = Right $ Bool b
eval (Ap e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case ee1 of
    (Lambda v t e) -> eval (subst e v ee2)
    _ -> Left $ UndefinedApplication
eval e@(Plus e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1,ee2) of
    (Num n, Num m) -> return (Num (m+n))
    _ -> Left $ InvalidBinop e
eval e@(Minus e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1,ee2) of
    (Num n, Num m) -> return (Num (n-m))
    _ -> Left $ InvalidBinop e
eval e@(Times e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1,ee2) of
    (Num n, Num m) -> return (Num (m*n))
    _ -> Left $ InvalidBinop e
eval e@(Divide e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1,ee2) of
    (Num n,Num m) -> return (Num (n `div` m))
    _ -> Left $ InvalidBinop e
eval e@(And e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1, ee2) of
    (Bool b1, Bool b2) -> return (Bool (b1 && b2))
    _ -> Left $ InvalidBinop e
eval e@(Or e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1, ee2) of
    (Bool b1, Bool b2) -> return (Bool (b1 || b2))
    _ -> Left $ InvalidBinop e
eval e@(Lt e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1,ee2) of
    (Num n, Num m) -> return (Bool (n <= m))
    _ -> Left $ InvalidBinop e
eval e@(If e1 e2 e3) = do
  ee1 <- eval e1
  ee2 <- eval e2
  ee3 <- eval e3
  case ee1 of
    (Bool True) -> return ee2
    (Bool False) -> return ee3
    _ -> Left $ InvalidBoolean e
eval e@(Pair e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  return (Pair ee1 ee2) --ask about this but it feels like it should work
eval e@(Not e1) = 
  case (eval e1) of
    Right (Bool b) -> Right $ Bool (not b)
    Right _ -> Left $ InvalidType e1
    l -> l
eval e@(Neg e1) =
  case (eval e1) of
    Right (Num n) -> Right $ Num (-n)
    Right _ -> Left $ InvalidType e
    l -> l
eval e@(Fst e1) =
  case (eval e1) of
    Right (Pair ee1 ee2) -> Right $ ee1
    Right _ -> Left $ InvalidType e
    l -> l
eval e@(Snd e1) =
  case (eval e1) of
    Right (Pair ee1 ee2) -> Right $ ee2
    Right _ -> Left $ InvalidType e
    l -> l
eval e@(Eqq e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  case (ee1,ee2) of
    (Num n, Num m) -> return $ Bool $ m==n
    (Bool b1, Bool b2) -> return $ Bool $ b1==b2
    (Pair x1 y1, Pair x2 y2) -> eval (And (Eqq x1 x2) (Eqq y1 y2))
    _ -> Left $ InvalidBinop e
eval (Typed e1 t) = eval e1
eval (Let x e1 e2) = eval $ subst e2 x e1
eval _ = undefined 


te1, te2, te3, te4, te5 :: String
te1 = "lambda x . x"
te2 = "(lambda y z a . a y z) (lambda c.c)"
te3 = "lambda y . y"
te4 = "lambda a b . a"
te5 = "(lambda x . x) z"
three = "let zero = lambda s z. z in let succ = lambda n. lambda s z. s (n s z) in succ (succ (succ zero))"
