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
    | Let VarName LExp LExp
    | LetRec VarName Type LExp LExp
    | If LExp LExp LExp
    | Typed LExp Type
    | Pair LExp LExp
    | Neg LExp
    | Not LExp
    | Fst LExp
    | Snd LExp
    | Plus LExp LExp
    | Minus LExp LExp
    | Times LExp LExp
    | Divide LExp LExp
    | And LExp LExp
    | Or LExp LExp
    | Lt LExp LExp
    | Eqq LExp LExp
--    deriving Show

instance Show LExp where
  show = lShow

show' :: LExp -> String
show' (Lambda _ _ _) = "<function>"
show' e = show e

lShow :: LExp -> String
lShow (Var x) = x
lShow (Ap e1 e2) = (par $  lShow e1) ++" "++ (par $  lShow e2)
lShow (Lambda v t e) = "lambda " ++ v ++ " : " ++ (show t) ++  " . " ++ (lShow e)
lShow (Num x) = show x
lShow (Bool True) = "true"
lShow (Bool False) = "false"
lShow (Let v e1 e2) = "let " ++ v ++ " = " ++ (lShow e1) ++ " in " ++ (lShow e2)
lShow (LetRec v t e1 e2) = "let rec " ++ v ++ " : " ++ (show t) ++ " = " ++ (lShow e1) ++ " in " ++ (lShow e2)
lShow (If c e1 e2) = "if " ++ (lShow c) ++ " then " ++ (lShow e1) ++ " else " ++ (lShow e2)
lShow (Typed e t) = par $ (lShow e) ++ " : " ++ (show t)
lShow (Pair e1 e2) = par $ (lShow e1) ++ ", " ++ (lShow e2)
lShow (Neg e) = "-" ++ (par $ lShow e)
lShow (Not e) = "not " ++ (par $ lShow e)
lShow (Fst e) = "fst " ++ (par $ (lShow e))
lShow (Snd e) = "snd " ++ (par $ (lShow e))
lShow (Plus e1 e2) = (lShow e1) ++ " + " ++ (lShow e2)
lShow (Minus e1 e2) = (lShow e1) ++ " - " ++ (lShow e2)
lShow (Times e1 e2) = (lShow e1) ++ " * " ++ (lShow e2)
lShow (Divide e1 e2) = (lShow e1) ++ " / " ++ (lShow e2)
lShow (And e1 e2) = (lShow e1) ++ " and " ++ (lShow e2)
lShow (Or e1 e2) = (lShow e1) ++ " or " ++ (lShow e2)
lShow (Lt e1 e2) = (lShow e1) ++ " < " ++ (lShow e2)
lShow (Eqq e1 e2) = (lShow e1) ++ " == " ++ (lShow e2)
  
par :: String -> String
par x = "(" ++ x ++ ")"

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


instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TFun t1 t2) = show t1 ++ " -> " ++ show t2
  show (TPair t1 t2) = par $ show t1 ++ "," ++ show t2


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

instance Monad Parser where
  p >>= k = Parser $ \s ->
    case parse p s of
      Nothing -> Nothing
      Just (v,s') -> parse (k v) s'
  return = pure


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
keywords = ["if","then", "else",  "lambda", "let", "in", "int", "bool", "not", "fst", "snd", "and", "or"]

types :: Map String Type
types = Data.Map.insert "int" TInt (Data.Map.insert "bool" TBool (Data.Map.empty))


isKeyword :: String -> Bool
isKeyword = (`elem` keywords)

kw :: String -> Parser String
kw s = str s <* ensure (maybe True (not . isAlphaNum)) lookahead

typ, tAtom :: Parser Type
tAtom' = Parser $ \s -> case parse (ws *> some (satisfy isAlpha))  s of
  Nothing -> Nothing
  Just (s, c) -> (\t -> (t, c)) <$> Data.Map.lookup s types
tAtom = tAtom'
  <|> parens typ
  <|> parens (TPair <$> typ <*> (char ',' *> ws *> typ))
typ = TFun <$> tAtom <*> (str "->" *> ws *> typ)
  <|> tAtom

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

nextNot :: Char -> Parser ()
nextNot a = ensure (maybe True (not . (== a))) (ws *> lookahead) *> pure ()

unop :: Parser (LExp -> LExp)
unop = ws *> char '-' *> pure Neg
  <|> ws *> kw "not" *> pure Not
  <|> ws *> kw "fst" *> pure Fst
  <|> ws *> kw "snd" *> pure Snd


binop1 :: Parser (LExp -> LExp -> LExp)
binop1 = ws *> char '<' *> nextNot '=' *> pure Lt
  <|> ws *> str "==" *> pure Eqq
  <|> ws *> char '>' *> nextNot '=' *> (pure $ flip Lt)
  <|> ws *> str ">=" *> pure (\le1 le2 -> Not $ Lt le1 le2)
  <|> ws *> str "<=" *> pure (\le1 le2 -> Or (Lt le1 le2) (Eqq le1 le2))
  
binop2 :: Parser (LExp -> LExp -> LExp)
binop2 = ws *> char '+' *> pure Plus
  <|> ws *> char '-' *> pure Minus
  <|> ws *> kw "or" *> pure Or

binop3 :: Parser (LExp -> LExp -> LExp)
binop3 = ws *> char '*' *> pure Times
  <|> ws *> char '/' *> pure Divide
  <|> ws *> kw "and" *> pure And
  <|> nextNot '-' *> pure Ap


atom, factor, term, comparable, lExp :: Parser LExp
atom =
  Num <$> num

  <|> kw "true" *> pure (Bool True)

  <|> kw "false" *> pure (Bool False)

  <|> If <$> (kw "if" *> ws *> lExp) 
  <*> (kw "then" *> ws *> lExp)
  <*> (kw "else" *> ws *> lExp) 

  <|> (parens $ Typed <$> (ws *> lExp) <*> (char ':' *> typ))

  <|> (parens $ Pair <$> (ws *> lExp) <*> (char ',' *> lExp))  
  
  <|> formLambda <$>
  (
    lambda *>
    (
      ((:[]) <$> (pair <$> (var) <*> (char ':' *> typ)) )
      <|> (some $ parens (pair <$> (var) <*> (char ':' *> typ)))
    )
  )
  <*> (dot *> lExp)
  
  <|> Var <$> var <* ws
  
  <|> ws *> parens lExp <*ws

  <|> LetRec <$> (str "let"*> ws *> str "rec" *> var)
  <*> (char ':' *> typ)
  <*> (char '=' *> ws *> lExp)
  <*> (str "in" *> lExp)
  
  <|> Let <$> (str "let" *> var)
  <*> (char '=' *> ws *> lExp)
  <*> (str "in" *> lExp)
  
  <|> pLet <$> (str "let" *> var)
  <*> (char ':' *> typ)
  <*> (char '=' *> ws *> lExp)
  <*> (str "in" *> lExp)
  where
    pLet v t e1 e2 = Let v (Typed e1 t) e2
    
factor = atom <|> (unop >>= (<$> atom))

term = factor `chainl1` binop3

comparable = term `chainl1` binop2

lExp = (comparable `chainl1` binop1) <* ws

pLExp :: String -> Either error LExp
pLExp s = case parse lExp s of
  Just (e, []) -> Right e
  Just (_, r) -> Left $ error $ "Parse error: Expected EOF, found " ++ r
  Nothing -> Left $ error $ "Parse error"

