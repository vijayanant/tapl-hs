module SimpleBool.Evaluator 
(run)
where 

import SimpleBool.TypeChecker
import SimpleBool.Syntax
import SimpleBool.Types
import Control.Arrow

deBruijn :: Term -> Term
deBruijn = deBruijn' []

deBruijn' :: [(String, Int)] -> Term -> Term
deBruijn' ctx (Var str)       = case lookup str ctx of
                                  Just m -> VarI str m
                                  _      -> VarI str 0 --undefined    -- s is a free variable
deBruijn' ctx (Abs str ty t ) = Abs str ty (deBruijn' ((str,0):map (second succ) ctx) t)
deBruijn' ctx (App  t1 t2 )   = App (deBruijn' ctx t1) (deBruijn' ctx t2)
deBruijn' ctx (If t1 t2 t3)   = If (deBruijn' ctx t1) (deBruijn' ctx t2) (deBruijn' ctx t3)
deBruijn' ctx t               = t

shift :: Int -> Int -> Term -> Term
shift c d (VarI str k)   = VarI str (if k<c then k else k+d)
shift c d (Abs str ty t) = Abs str ty (shift (c+1) d t)
shift c d (App t1 t2)    = App (shift c d t1) (shift c d t2)
shift c d t              = t

-- [j -> s] t 
subst :: Int -> Term -> Term -> Term
subst j s (VarI str k)   = if j == k then s else VarI str j
subst j s (Abs str ty t) = Abs str ty (subst (j+1) (shift 0 1 s) t)
subst j s (App t1 t2)    = App (subst j s t1) (subst j s t2)
subst j s (If t1 t2 t3)  = If (subst j s t1) (subst j s t2) (subst j s t3)
subst s j t              = t

{--
  beta-reduction rule: : the term being substituted for the bound variable is
    * first shifted up by one,
    * then the substitution is made,
    * and then the whole result is shifted down by one to account for the fact
      that the bound variable has been used up.
--} 
beta :: Term -> Term -> Term
beta s t = shift 0 (-1) (subst 0 (shift 0 1 s) t)

{--
Reduce single step
  Left  for reducible term
  Right for irreducible term (value?)
--}
eval' :: Term -> Either Term Term 
eval' (App (Abs _ ty t) v) = Left $ beta (eval1 v) t   -- E-AppAbs
eval' (App t1 t2)       = case eval' t1 of
                            Left t' -> Left $ App t' t2 -- E-App2
                            Right t' -> Left $ App t1 (eval1 t2) -- E-App1
eval' (If t1 t2 t3)    = case eval1 t1 of
                           T -> Left t2 
                           F -> Left t3
                           _ -> Left $ If (eval1 t1) t2 t3
eval' t                 = Right t

eval1 :: Term -> Term
eval1 = either id id . eval'

{--
  Reduce term in n single step evaluations
--}
eval :: Term -> Term
eval t = case eval' t of
  Left t' -> eval t' -- reducible
  Right t' -> t'     -- irreducible


run :: Term -> Term
run t = case typeOf [] t  of
  TBool  ->  eval $ deBruijn t
  (TArr t1 t2 )  ->  eval $ deBruijn t
