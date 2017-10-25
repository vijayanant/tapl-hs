module Arith.Evaluator 
(
  eval
) where 

import Control.Applicative

import Arith.Syntax

-- We eval'uate to 'Nothing' whenever we cannot further reduce (Normal form)
-- Note we do not handle cases where expressions are invalid 
-- for example `TIsZero TTrue`
eval' :: Term -> Maybe Term
eval' (TTrue)               = Nothing
eval' (TFalse)              = Nothing
eval' (TZero)               = Nothing
eval' (TIf TTrue t _)       = return t
eval' (TIf TFalse _ t)      = return t
eval' (TIf t1 t2 t3)        = liftA3 TIf (eval' t1) (return t2 ) (return t3)
eval' (TPred TZero )        = return TZero
eval' (TSucc t)             = liftA TSucc ( eval' t )
eval' (TPred (TSucc t))     = return t
eval' (TPred t)             = liftA TPred ( eval' t )
eval' (TIsZero TZero)       = return TTrue
eval' (TIsZero ( TSucc t )) = return TFalse
eval' (TIsZero t)           = liftA TIsZero (eval' t)

eval :: Term -> Term
eval t =  case eval' t of
  Just t' -> eval t'
  Nothing -> t 

