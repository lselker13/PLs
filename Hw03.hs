{-# OPTIONS_GHC -W #-}
module Hw03 where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
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

testStore = Map.insert "y" 2 (Map.singleton "x" 5) :: Store

testExp = Neg (Times ((Var "x") `Plus` (Var "y"))  (Num 3))

testStmt = While (Lt (Var "y") (Num 5)) (Assign "y" (Plus (Var "y") (Num 1)))
testStmt2 = If (Lt (Var "y") (Num 5))  (Assign "y" (Plus (Var "y") (Num 2))) Skip

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
eval' st (Assign n v) = case (evalA' st v) of
                       Left e -> Left e
                       Right a -> Right (Map.insert n a st)
eval' st (Seq l r) = case (eval' st l) of
  Left e -> Left e
  Right st' -> eval' st' r
eval' st (If pred l r) = case (evalB' st pred) of
  Left e -> Left e
  Right True -> eval' st l
  Right False -> eval' st r
eval' st (While pred c) = case (evalB' st pred) of
  Left e -> Left e
  Right True -> case (eval' st c) of 
              Left e -> Left e
              Right a -> eval' a (While pred c)
  Right False -> Right st


testStmt' = While (Lt (Var' "y") (Num' 5)) (Assign "y" (Plus' (Var' "y") (Num' 1)))
testStmt2' = If (Lt (Var' "y") (Num' 5))  (Assign "y" (Div' (Var' "y") (Num' 2))) Skip

-- Problem 3

{-
data AExp' =
    Var' VarName
  | Num' Int
  | Plus' AExp' AExp'
  | Times' AExp' AExp'
  | Neg' AExp'
  | Div' AExp' AExp'
  deriving (Show, Eq)

-}
varsA :: AExp' -> Set VarName
varsA (Num' _) = Set.empty
varsA (Var' v) = Set.singleton v
varsA (Plus' l r) = Set.union (varsA l) (varsA r)
varsA (Times' l r) = Set.union (varsA l) (varsA r)
varsA (Div' l r) = Set.union (varsA l) (varsA r)
varsA (Neg' l) = varsA l

{-
data BExp a =
    Bool Bool
  | Equal a a
  | Lt a a
  | Not (BExp a)
  | Or (BExp a) (BExp a)
  | And (BExp a) (BExp a)
  deriving (Show, Eq, Ord)
-}

varsB :: BExp AExp' -> Set VarName
varsB (Bool _) = Set.empty
varsB (Equal l r) = Set.union (varsA l) (varsA r)
varsB (Lt l r) = Set.union (varsA l) (varsA r)
varsB (Not l) = varsB l
varsB (Or l r) = Set.union (varsB l) (varsB r)
varsB (And l r) = Set.union (varsB l) (varsB r)

useBeforeDef :: Set VarName -> Stmt AExp' BExp -> (Set VarName, Set VarName)
useBeforeDef defs Skip = (defs, Set.empty)
useBeforeDef defs (Assign x a) = (Set.insert x defs, varsA a `Set.difference` defs)
useBeforeDef defs (Seq l r) = (secondD, firstU `Set.union` secondU)
  where
    (firstD, firstU) = useBeforeDef defs l
    (secondD, secondU) = useBeforeDef firstD r
useBeforeDef defs (If pred l r) =
  (lD `Set.intersection` rD, predU `Set.union` lU `Set.union` rU)
  where
    predU = varsB pred `Set.difference` defs
    (lD, lU) = useBeforeDef defs l
    (rD, rU) = useBeforeDef defs r
useBeforeDef defs (While pred c) = (cD, predU `Set.union` cU)
  where
    predU = varsB pred `Set.difference` defs
    (cD, cU)  = useBeforeDef defs c

unboundY = Assign "x" (Var' "y")
ambiguous b = Seq (If b (Assign "y" (Num' 0)) Skip) unboundY

testUnbound, testAmbiguous :: Bool
testUnbound = useBeforeDef Set.empty unboundY ==
              (Set.singleton "x", Set.singleton "y")

testAmbiguous = useBeforeDef Set.empty (ambiguous (Bool True)) ==
                (Set.singleton "x", Set.singleton "y")

-- Problem 4

type Config = (Store, Stmt AExp BExp)

step :: Config -> Maybe Config
step (_,Skip) = Nothing
step (st,Assign x a) = Just (Map.insert x (evalA st a) st,Skip)
step (st,Seq Skip s2) = Just (st,s2)
step (st,Seq s1 s2) = case step (st, s1) of
         Just (st',b) -> Just (st', (Seq b s2))
         Nothing -> Just (st, s2)
     
step (st,If b s1 s2) | evalB st b = Just (st, s1)
                     | otherwise = Just (st, s2) 
step (st,While b s) | evalB st b = Just (st, Seq s (While b s))
                    | otherwise = Nothing


trace :: (a -> Maybe a) -> a -> [a]
trace f v =
  case f v of
    Nothing -> [v]
    Just v' -> v:trace f v'


data TVL = No | Maybe | Yes deriving (Show, Eq, Ord)

diverges :: Ord a => Int -> [a] -> TVL
diverges limit = divAux limit Set.empty 
  where
    divAux 0 _ _ = Maybe
    divAux _ _ [] = No
    divAux limit soFar (l:ls) = if (not (Set.member l soFar))
      then (divAux (limit - 1) (Set.insert l soFar) ls)
      else Yes

haltsIn :: Stmt AExp BExp -> Int -> TVL
haltsIn s limit = case diverges limit l of
  Yes -> No
  Maybe -> Maybe
  No -> Yes
  where
    l = trace step (Map.empty, s)

loop :: Stmt AExp BExp
loop = While (Bool True) Skip

long :: Stmt AExp BExp
long = Seq (Assign "x" (Num 0)) (While (Lt (Var "x") (Num 1000)) (Assign "x" (Plus (Var "x") (Num 1))))

tricky :: Stmt AExp BExp
tricky = Seq (Assign "x" (Num 0)) (While (Bool True) (Assign "x" (Plus (Var "x") (Num 1))))
