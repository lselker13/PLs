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
subst (Lambda x t e1) y e | x == y = Lambda x e1
                        | otherwise = Lambda x t (subst e1 y e)
subst Succ _ _ = Succ
subst (Num n) _ _ = (Num n)

eval :: LExp -> Either error LExp
eval (Var x) = Left $ error $ "Unbound variable " ++ x
eval (Lambda v e) = Right $ Lambda v e
eval (Num n) = Right $ Num n
eval Succ = Right $ Succ
eval (Ap (Lambda v1 e1) e@(Lambda v2 e2)) = eval (subst e1 v1 e)
eval (Ap (Lambda v1 e1) Succ) = eval (subst e1 v1 Succ)
eval (Ap (Lambda v1 e1) e@(Num x)) = eval (subst e1 v1 e)
eval (Ap Succ (Num n)) = Right $ Num (n+1)
eval e@(Ap (Num n) (Num m)) = Left $ error $ "Not a number: " ++ (show e)
eval (Ap Succ e2) = case eval e2 of
  Right (Num n) -> Right $ Num (n+1)
  Right _ -> Left $ error $ "Not a number: " ++ (show e2)
  Left e -> Left e
eval (Ap e1 e2) = do
  ee1 <- eval e1
  ee2 <- eval e2
  eval (Ap ee1 ee2)

free :: LExp -> Set VarName
free (Var x) = Data.Set.singleton x
free (Lambda v e) = Data.Set.delete v (free e)
free (Ap e1 e2) = Data.Set.union (free e1) (free e2)
free _ = error $ "Running free on nums/succs"
-- If Church or Check are in the map at all they are true


processArgs :: [String] -> Map String String
processArgs [] = Data.Map.empty
processArgs ("-c":ss) = Data.Map.insert "Check" "" (processArgs ss)
processArgs ("-n":ss) = Data.Map.insert "Church" "" (processArgs ss)
processArgs ("-cn":ss)= Data.Map.insert "Check" "" inserted
  where
    inserted = Data.Map.insert "Church" "" (processArgs ss)
processArgs ("-nc":ss)= Data.Map.insert "Check" "" inserted
  where
    inserted = Data.Map.insert "Church" "" (processArgs ss)
processArgs ("-":ss) = processArgs ss
processArgs (s:ss) = Data.Map.insert "Filename" s (processArgs ss)

getInput :: Map String String -> IO String
getInput m = case Data.Map.lookup "Filename" m of
  Nothing -> getContents
  Just fn -> do
     handle <- openFile fn ReadMode
     hGetContents handle

main :: IO ()
main = do
  args <- getArgs
  let pArgs = processArgs args
  input <- case Data.Map.lookup "Filename" pArgs of
    Nothing -> getContents
    Just fn -> readFile fn
  let parsed = case pLExps input of
        (Left e) -> e
        (Right exp) -> exp
  let evaluated = if Data.Map.member "Check" pArgs
                  then let f = free parsed in
                         if Data.Set.size f > 0
                         then error $ "Unbound variable(s) " ++ (show $ Data.Set.toList f)
                         else eval parsed
                  else eval parsed
  if Data.Map.member "Church" pArgs
    then case toNum evaluated of
           (Left e) -> e
           (Right n) -> putStr $ show n
    else case evaluated of
           (Left e) -> e
           (Right exp) -> putStr $ show exp

toNum :: Either e LExp -> Either e Int
toNum eexp = case eexp of
  Left e -> Left e
  Right ex -> ret
    where
      ret = case  eval (Ap (Ap ex Succ) (Num 0)) of
        Right (Num n) -> Right n
        Right exp -> Left $ error $ "Could not find a number in " ++ (show exp)
        Left e -> Left e

te1, te2, te3, te4, te5 :: String
te1 = "lambda x . x"
te2 = "(lambda y z a . a y z) (lambda c.c)"
te3 = "lambda y . y"
te4 = "lambda a b . a"
te5 = "(lambda x . x) z"
three = "let zero = lambda s z. z in let succ = lambda n. lambda s z. s (n s z) in succ (succ (succ zero))"
