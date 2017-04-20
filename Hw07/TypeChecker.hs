module TypeChecker where

import Parse
import Data.Map
import Control.Applicative


type Context = Map VarName Type

data TypeError = ExpectedFunction LExp Type
               | Mismatch LExp Type Type {- expression, got, expected -}
               | UnboundVariable VarName deriving Show

typeOf :: Context -> LExp -> Either TypeError Type
typeOf g (Var x) =
  case Data.Map.lookup x g of
    Nothing -> Left $ UnboundVariable x
    Just t -> pure t
typeOf g (Lambda x t1 e) = do
  t2 <- typeOf (Data.Map.insert x t1 g) e
  pure $ TFun t1 t2
typeOf g e@(Ap e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case t1 of
    TFun t11 t12 | t11 == t2 -> pure t12
    TFun t11 t12 -> Left $ Mismatch e t2 t11
    _ -> Left $ ExpectedFunction e1 t1
typeOf _ (Bool _) = pure TBool
typeOf g (If e1 e2 e3) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  t3 <- typeOf g e3
  case t1 of
    TBool | t2 == t3 -> pure t2
    TBool -> Left $ Mismatch e3 {- arbitrary! -} t3 t2
    _ -> Left $ Mismatch e1 t1 TBool
