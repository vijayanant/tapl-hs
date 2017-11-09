module ULambda.Evaluator 
(run)
where 

import ULambda.Types
import Control.Arrow

deBruijn :: Term -> Term
deBruijn = deBruijn' []

deBruijn' :: [(String, Int)] -> Term -> Term
deBruijn' ctx (Var loc str) = case lookup str ctx of
                            Just m -> VarI loc str m
                            _      -> VarI loc str 0 --undefined    -- s is a free variable
deBruijn' ctx (Abs loc str t ) = Abs loc str (deBruijn' ((str,0):map (second succ) ctx) t)
deBruijn' ctx (App loc  t1 t2 ) = App loc (deBruijn' ctx t1) (deBruijn' ctx t2)
deBruijn' ctx t         = t

shift :: Int -> Int -> Term -> Term
shift c d (VarI loc str k) = VarI loc str (if k<c then k else k+d)
shift c d (Abs loc str t) = Abs loc str (shift (c+1) d t)
shift c d (App loc t1 t2) = App loc (shift c d t1) (shift c d t2)
shift c d t           = t

-- [j -> s] t 
subst :: Int -> Term -> Term -> Term
subst j s (VarI loc str k) = if j == k then s else VarI loc str j
subst j s (Abs loc str t)  = Abs loc str (subst (j+1) (shift 0 1 s) t)
subst j s (App loc t1 t2)  = App loc (subst j s t1) (subst j s t2)
subst s j t            = t

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
eval' (App loc (Abs _ _ t) v) = Left $ beta (eval1 v) t   -- E-AppAbs
eval' (App loc t1 t2)       = case eval' t1 of
                            Left t' -> Left $ App loc t' t2 -- E-App2
                            Right t' -> Left $ App loc t1 (eval1 t2) -- E-App1
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
run t = eval $ deBruijn t
