module Hw06 where

import Control.Applicative
import Test.QuickCheck

import Data.Char
import Data.Map hiding (null,foldr, foldl)

import Data.Maybe
import Data.Set hiding (null, foldr, foldl)

import System.Environment
import System.Exit
import System.IO

newtype Parser a = Parser {parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  f <*> a = Parser $ \s ->
    case parse f s of
      Just (g,s') -> parse (fmap g a) s'
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  l <|> r = Parser $ \s -> parse l s <|> parse r s

ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

lookahead :: Parser (Maybe Char)
lookahead = Parser f
   where f [] = Just (Nothing,[])
         f (c:s) = Just (Just c,c:s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

eof' :: Parser ()
eof' = const () <$> ensure isNothing lookahead

ws :: Parser ()
ws = pure () <* many (satisfy isSpace)

char :: Char -> Parser Char
char c = ws *> satisfy (==c)

str :: String -> Parser String
str s = ws *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

keywords :: [String]
keywords = ["lambda", "let", "in"]

isKeyword :: String -> Bool
isKeyword = (`elem` keywords)

kw :: String -> Parser String
kw s = str s <* ensure (maybe True (not . isAlphaNum)) lookahead

isQuote :: Char -> Bool
isQuote = (== '\'')

var :: Parser String
var =  ensure (not . isKeyword) $ (:) <$> (ws *> satisfy isAlpha) <*> many (satisfy (\c -> isAlphaNum c ||  isQuote c))

num :: Parser Int
num = ws *> (read <$> some (satisfy isDigit))

type VarName = String

data LExp =
      Var VarName
    | Ap LExp LExp
    | Lambda VarName LExp
    | Num Int
    | Succ
    
par :: String -> String
par x = "(" ++ x ++ ")"

instance Show LExp where
  show (Var x) = x
  show (Ap e1 e2) = (par $  show e1) ++" "++ (par $  show e2)
  show (Lambda v e) = "lambda " ++ v ++ " . " ++ (show e)
  show (Num x) = show x
  show (Succ) = "Succ"

lambda :: Parser ()
lambda = str "lambda" *> pure ()

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = foldl (\acc (op,v) -> op acc v) <$> 
                p <*> many ((\op v -> (op,v)) <$> sep <*> p)

dot :: Parser ()
dot = ws *> char '.' *> ws *> pure ()

formLambda :: [VarName] -> LExp -> LExp
formLambda = flip (foldr Lambda)

formAp :: Parser (LExp -> LExp -> LExp)
formAp = ws *> pure Ap

lExp, lExps :: Parser LExp
lExp = formLambda <$> (lambda *> some (ws *> var)) <*> (dot *> lExps)
       <|> Var <$> var <* ws
       <|> ws *> parens lExps <*ws
       <|> pLet <$> (str "let" *> var) <*> (char '=' *> ws *> lExps) <*> (ws *> str "in" *> lExps)
  where
    pLet v e1 e2 = Ap (Lambda v e2) e1
lExps = lExp `chainl1` formAp

pLExps :: String -> Either error LExp
pLExps s = case parse lExps s of
  Just (e, []) -> Right e
  Just (_, r) -> Left $ error $ "Parse error: Expected EOF, found " ++ r
  Nothing -> Left $ error $ "Parse error"

  
subst :: LExp -> VarName -> LExp -> LExp
subst (Var x) y e | x == y    = e
                  | otherwise = Var x
subst (Ap e1 e2) y e         = Ap (subst e1 y e) (subst e2 y e)
subst (Lambda x e1) y e | x == y = Lambda x e1
                        | otherwise = Lambda x (subst e1 y e)
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

{-
ae :: Int -> Gen LExp
ae n | n <= 0    =  Lambda "x" (Var "x")
     | otherwise = oneof [Num <$> arbitrary,
                          Plus <$> ae (n `div` 2) <*> ae (n `div` 2),
                          Times <$> ae (n `div` 2) <*> ae (n `div` 2),
                          Neg <$> ae(n `div` 2)]
-}
