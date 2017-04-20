module Parse where

import Control.Applicative
import Test.QuickCheck

import Data.Char
import Data.Map hiding (null,foldr, foldl)
import Data.List

import Data.Maybe
import Data.Set hiding (null, foldr, foldl)

import System.Environment
import System.Exit
import System.IO
type VarName = String


data LExp =
      Var VarName
    | Ap LExp LExp
    | Lambda VarName Type LExp
    | Num Int
    | Bool Bool
    | Succ
    | Let VarName LExp LExp
    | If LExp LExp LExp
    | Typed LExp Type
    | Pair LExp LExp
    | Unop LExp
    | Binop LExp LExp
    
{-t ::= 
e ::=  let rec x : t = e1 in e2 |
unop ::=
binop ::= 
-}
data Type =
      TInt
    | TBool
    | TFun Type Type
    | TPair Type Type
    deriving Eq

data Unop =
  | Neg
  | Not
  | Fst
  | Snd

data Binop =
  | Plus
  | Minus
  | Times
  | Div
  | And
  | Or
  | Eqq
  | Lt 


    
par :: String -> String
par x = "(" ++ x ++ ")"

instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TFun t1 t2) = show t1 ++ "->" ++ show t2
  show (TPair t1 t2) = par $ show t1 ++ "," ++ show t2

instance Show LExp where
  show (Var x) = x
  show (Ap e1 e2) = (par $  show e1) ++" "++ (par $  show e2)
  show (Lambda v t e) = "lambda " ++ v ++ " : " ++ (show t) ++  " . " ++ (show e)
  show (Num x) = show x
  show (Succ) = "Succ"



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
keywords = ["lambda", "let", "in", "int", "bool"]

types :: Map String Type
types = Data.Map.insert "int" TInt (Data.Map.insert "bool" TBool (Data.Map.empty))

isKeyword :: String -> Bool
isKeyword = (`elem` keywords)

kw :: String -> Parser String
kw s = str s <* ensure (maybe True (not . isAlphaNum)) lookahead

typ :: Parser Type
typ = Parser $ \s -> case parse (ws *> some (satisfy isAlpha))  s of
  Nothing -> Nothing
  Just (s, c) -> (\t -> (t, c)) <$> Data.Map.lookup s types


isQuote :: Char -> Bool
isQuote = (== '\'')

var :: Parser String
var =  ensure (not . isKeyword) $ (:) <$> (ws *> satisfy isAlpha) <*> many (satisfy (\c -> isAlphaNum c ||  isQuote c))

num :: Parser Int
num = ws *> (read <$> some (satisfy isDigit))


lambda :: Parser ()
lambda = str "lambda" *> pure ()

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = foldl (\acc (op,v) -> op acc v) <$> 
                p <*> many ((\op v -> (op,v)) <$> sep <*> p)

dot :: Parser ()
dot = ws *> char '.' *> ws *> pure ()

formLambda :: [(VarName, Type)] -> LExp -> LExp
formLambda = flip (foldr toLambda)
  where
    toLambda (v, t) = Lambda v t

formAp :: Parser (LExp -> LExp -> LExp)
formAp = ws *> pure Ap

pair a b = (a,b)

lExp, lExps :: Parser LExp
lExp = 
  formLambda <$>
  (
    lambda *>
    (
      ((:[]) <$> (pair <$> (var) <*> (char ':' *> typ)) )
      <|> (some $ parens (pair <$> (var) <*> (char ':' *> typ)))
    )
  )
  <*> (dot *> lExps)
  <|> Var <$> var <* ws
  <|> ws *> parens lExps <*ws
  <|> pLet <$> (str "let" *> var) <*> (char ':' *> typ) <*> (char '=' *> ws *> lExps) <*> (str "in" *> lExps)
  where
    pLet v t e1 e2 = Ap (Lambda v t e2) e1
    
lExps = lExp `chainl1` formAp

pLExps :: String -> Either error LExp
pLExps s = case parse lExps s of
  Just (e, []) -> Right e
  Just (_, r) -> Left $ error $ "Parse error: Expected EOF, found " ++ r
  Nothing -> Left $ error $ "Parse error"
