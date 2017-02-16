{-# OPTIONS_GHC -W #-}
module Hw03 where

import qualified Data.Map as Map
import Data.Map (Map)
--import qualified Data.Set as Set
--import Data.Set (Set)
import Control.Applicative as Applicative

-- Problem 1
type VarName = String

type Store = Map VarName Int

data AExp =
    Var VarName
  | Num Int
  | Plus AExp AExp
  | Times AExp AExp
  | Neg AExp
  deriving (Show, Eq, Ord)

testStore = Map.insert "y" 1 (Map.singleton "x" 5) :: Store

testExp = Neg (Times ((Var "x") `Plus` (Var "y"))  (Num 3))

testStmt = While (Lt (Var "y") (Num 5)) (Assign "y" (Plus (Var "y") (Num 1)))
testStmt' = If (Lt (Var "y") (Num 5))  (Assign "y" (Plus (Var "y") (Num 2))) Skip

evalA :: Store -> AExp -> Int
evalA st (Var x) = Map.findWithDefault 0 x st
evalA st (Num a) = a
evalA st (Plus l r) = (evalA st l) + (evalA st r)
evalA st (Times l r) = (evalA st l) * (evalA st r)
evalA st (Neg l) = negate (evalA st l)

data BExp a =
    Bool Bool
  | Equal a a
  | Lt a a
  | Not (BExp a)
  | Or (BExp a) (BExp a)
  | And (BExp a) (BExp a)
  deriving (Show, Eq, Ord)

evalB :: Store -> BExp AExp -> Bool
evalB _ (Bool b) = b
evalB st (Equal l r) = (evalA st l) == (evalA st r)
evalB st (Lt l r) = (evalA st l) < (evalA st r)
evalB st (Not b) = not (evalB st b)
evalB st (Or l r) = (evalB st l) || (evalB st r)
evalB st (And l r) = (evalB st l) && (evalB st r)

data Stmt a b =
    Skip
  | Assign VarName a
  | Seq (Stmt a b) (Stmt a b)
  | If (b a) (Stmt a b) (Stmt a b)
  | While (b a) (Stmt a b)
  deriving (Show, Eq, Ord)


eval :: Store -> Stmt AExp BExp -> Store
eval st Skip = st
eval st (Assign n v) = Map.insert n (evalA st v) st
eval st (Seq l r) = eval (eval st l) r
eval st (If pred l r) | evalB st pred = eval st l
                      | otherwise = eval st r
eval st (While pred c) | evalB st pred = eval (eval st c) (While pred c)
                       | otherwise = st
                       
-- Problem 2

data AExp' =
    Var' VarName
  | Num' Int
  | Plus' AExp' AExp'
  | Times' AExp' AExp'
  | Neg' AExp'
  | Div' AExp' AExp'
  deriving (Show, Eq)

data Error = NoSuchVariable VarName | DivideByZero AExp' deriving Show

testExp' = Div' (Times' ((Var' "x") `Plus'` (Var' "z"))  (Num' 3))(Num' 3)

evalA' :: Store -> AExp' -> Either Error Int
evalA' st (Var' x) = case (Map.lookup x st) of
  Just res -> Right res
  Nothing -> Left (NoSuchVariable x)
evalA' st (Num' a) = Right a
evalA' st (Plus' l r) = (+) <$>  (evalA' st l) <*> (evalA' st r) 
evalA' st (Times' l r) =  (*) <$> (evalA' st l) <*> (evalA' st r) 
evalA' st (Neg' l) = negate <$> (evalA' st l)
evalA' st (Div' nExp dExp) = case ((evalA' st nExp), (evalA' st dExp)) of
  (Left e, _)  -> Left e
  (_, Left e)  -> Left e
  (_, Right 0) -> Left (DivideByZero (Div' nExp dExp))
  (Right num, Right denom) -> Right (quot num denom)

evalB' :: Store -> BExp AExp' -> Either Error Bool
evalB' _ (Bool b) = Right b
evalB' st (Equal l r) =  (==) <$> (evalA' st l) <*> (evalA' st r)
evalB' st (Lt l r) =  (<) <$> (evalA' st l) <*>  (evalA' st r)
evalB' st (Not b) = not <$> (evalB' st b)
evalB' st (Or l r) = (||) <$> (evalB' st l) <*> (evalB' st r)
evalB' st (And l r) = (&&) <$> (evalB' st l) <*> (evalB' st r)


eval' :: Store -> Stmt AExp' BExp -> Either Error Store
eval' st Skip = Right st
eval' st (Assign n v) =  fmap (\x ->  Map.insert n x st) (evalA' st v)
eval' st (Seq l r) = fmap eval' (eval' st l) r
eval' st (If pred l r) = fmap apply (evalB' st pred)
                    where
                      apply True  = eval' st l
                      apply False = eval' st r

{-
data Stmt a b =
    Skip
  | Assign VarName a
  | Seq (Stmt a b) (Stmt a b)
  | If (b a) (Stmt a b) (Stmt a b)
  | While (b a) (Stmt a b)
  deriving (Show, Eq, Ord)

eval :: Store -> Stmt AExp BExp -> Store
eval st Skip = st
eval st (Assign n v) = Map.insert n (evalA st v) st
eval st (Seq l r) = eval (eval st l) r
eval st (If pred l r) | evalB st pred = eval st l
                      | otherwise = eval st r
eval st (While pred c) | evalB st pred = eval (eval st c) (While pred c)
                       | otherwise = st

-}
