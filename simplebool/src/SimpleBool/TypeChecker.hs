module SimpleBool.TypeChecker
 (typeOf) 
where

import SimpleBool.Types
import SimpleBool.Syntax


data Binding = VarBind Type
type Context = [(String, Binding)]

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bnd = (x, bnd):ctx

getType :: Context -> Int -> Type
getType ctx i = case getBinding ctx i of
  VarBind ty -> ty
  _          -> error "getType: wrong binding"

getBinding :: Context -> Int -> Binding
getBinding ctx i = (snd . head . drop i) ctx

typeOf :: Context -> Term -> Type
typeOf _ (T) = TBool
typeOf _ (F) = TBool
typeOf ctx (VarI x k) = getType ctx k -- context lookup will not fail for out simple system 
typeOf ctx (If t1 t2 t3) 
  |  TBool == typeOf ctx t1 = 
         let ty = typeOf ctx t2 in
         if ty == typeOf ctx t3 
         then ty
         else error "branches of conditionals have different types"
  | otherwise = error "gaurd of conditional is not a boolean"
typeOf ctx (Abs x ty t) = 
  let ctx' = addBinding ctx x (VarBind ty)
      ty2 = typeOf ctx' t
  in  TArr ty ty2
typeOf ctx (App t1 t2) =
  let ty1 = typeOf ctx t1
      ty2 = typeOf ctx t2
  in case ty1 of
     TArr ty11 ty12 -> if ty11 == ty2 then ty12
                     else error "parameter type mismatch"
     _ -> error "Arrow type expected"



