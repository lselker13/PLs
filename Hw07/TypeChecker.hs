module TypeChecker where

import Parse
import Data.Map
import Control.Applicative


type Context = Map VarName Type

data TypeError = ExpectedFunction LExp Type
               | Mismatch LExp Type Type {- expression, got, expected -}
               | NotPair LExp Type
               | FunctionEqualityException LExp 
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
    TFun t11 _  -> Left $ Mismatch e t2 t11
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
typeOf _ (Num _) = pure TInt
typeOf g (Let v e1 e2) = do
  t <- typeOf g e1
  let g' = Data.Map.insert v t g
  typeOf g' e2
typeOf g e@(Typed e' t) = do
  t' <- typeOf g e'
  if t' == t
    then return t
    else Left $ Mismatch e t' t
typeOf g (Pair e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  return $ TPair t1 t2
typeOf g e@(Neg e') = do
  t <- typeOf g e'
  if t == TInt
    then return TInt
    else Left $ Mismatch e t TBool
typeOf g e@(Not e') = do
  t <- typeOf g e'
  if t == TBool
    then return TBool
    else Left $ Mismatch e t TBool
typeOf g e@(Fst e') = do
  t <- typeOf g e'
  case t of
    TPair t1 _ -> return t1
    t -> Left $ NotPair e t
typeOf g e@(Snd e') = do
  t <- typeOf g e'
  case t of
    TPair _ t2 -> return t2
    t -> Left $ NotPair e t
typeOf g e@(Plus e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TInt, TInt) -> return TInt
    (TInt, t) -> Left $ Mismatch e t TInt
    (t, _) -> Left $ Mismatch e t TInt
typeOf g e@(Minus e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TInt, TInt) -> return TInt
    (TInt, t) -> Left $ Mismatch e t TInt
    (t, _) -> Left $ Mismatch e t TInt
typeOf g e@(Times e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TInt, TInt) -> return TInt
    (TInt, t) -> Left $ Mismatch e t TInt
    (t, _) -> Left $ Mismatch e t TInt
typeOf g e@(Divide e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TInt, TInt) -> return TInt
    (TInt, t) -> Left $ Mismatch e t TInt
    (t, _) -> Left $ Mismatch e t TInt
typeOf g e@(Lt e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TInt, TInt) -> return TInt
    (TInt, t) -> Left $ Mismatch e t TInt
    (t, _) -> Left $ Mismatch e t TInt
typeOf g e@(And e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TBool, TBool) -> return TBool
    (TBool, t) -> Left $ Mismatch e t TBool
    (t, _) -> Left $ Mismatch e t TBool
typeOf g e@(Or e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case (t1, t2) of
    (TBool, TBool) -> return TBool
    (TBool, t) -> Left $ Mismatch e t TBool
    (t, _) -> Left $ Mismatch e t TBool
typeOf g e@(Eqq e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  if t1 == t2
    then case t1 of
           TFun _ _  -> Left $ FunctionEqualityException e
           _ -> return TBool
    else Left $ Mismatch e t2 t1


