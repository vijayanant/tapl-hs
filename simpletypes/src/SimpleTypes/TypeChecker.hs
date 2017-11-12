module SimpleTypes.TypeChecker
 (typeOf) 
where

import SimpleTypes.Types
import SimpleTypes.Syntax

import Control.Applicative

data Binding = VarBind Type | UnknownBinding
type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bnd = (x, bnd):ctx

getType :: Context -> Int -> Either String Type
getType ctx i = case getBinding ctx i of
  VarBind ty -> Right ty
  _          -> Left "getType: wrong binding"

getBinding :: Context -> Int -> Binding
getBinding ctx i = (snd . head . drop i) ctx

typeOf :: Context -> Term -> Either String Type
typeOf _ (T _) = Right TBool
typeOf _ (F _) = Right TBool
typeOf _ (Unit _) = Right TUnit
typeOf ctx (Var loc x ) = Left $ prettyprint loc ++ ": undefined variable " ++ x
typeOf ctx (VarI loc x k) = case  getType ctx k  of
  Right ty -> Right ty
  Left err -> Left $ prettyprint loc ++ ": undefined variable " ++ x
typeOf ctx (Cond loc t1 t2 t3)  = do 
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  ty3 <- typeOf ctx t3
  case (ty1, ty2, ty3) of
    (TBool, p, q) | p == q -> return p
    (TBool, _, _)         -> Left $ prettyprint loc ++ ": branches of conditionals have different types"
    _                     -> Left $ prettyprint loc ++  ": gaurd of conditional is not a boolean"
typeOf ctx (Abs _ x ty t) = do
  let ctx' = addBinding ctx x (VarBind ty)
  ty2 <- typeOf ctx' t
  return $  TArr ty ty2
typeOf ctx (App _ (Abs _ "_" ty t) _)  = typeOf ctx t
typeOf ctx (App loc t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case ty1 of
    ( TArr ty11 ty12 ) | ty11 == ty2 -> return ty12
    ( TArr ty11 ty12 )              -> Left $ prettyprint loc ++ ": parameter type mismatch"
    _                               -> Left $ prettyprint loc ++ ": arrow type expected"
typeOf ctx (Let _ x t1 t2) = do 
  ty1 <- typeOf ctx t1 
  let ctx' =  addBinding ctx x (VarBind ty1)
  typeOf ctx' t2 
