module SimpleTypes.Evaluator 
(run)
where 

import SimpleTypes.TypeChecker
import SimpleTypes.Syntax
import SimpleTypes.Types
import Control.Arrow

deBruijn :: Term -> Term
deBruijn = deBruijn' []

deBruijn' :: [(String, Int)] -> Term -> Term
deBruijn' ctx (Var inf str)       = case lookup str ctx of
                                  Just m -> VarI inf str m
                                  _      -> VarI inf str 0 --undefined    -- s is a free variable
deBruijn' ctx (Abs inf str ty t ) = Abs inf str ty (deBruijn' ((str,0):map (second succ) ctx) t)
deBruijn' ctx (App inf  t1 t2 )   = App inf (deBruijn' ctx t1) (deBruijn' ctx t2)
deBruijn' ctx (Cond inf t1 t2 t3) = Cond inf (deBruijn' ctx t1) (deBruijn' ctx t2) (deBruijn' ctx t3)
deBruijn' ctx (Let inf str t1 t2) = Let inf str (deBruijn' ctx' t1) (deBruijn' ctx' t2) 
                                where ctx' = (str,0):map(second succ) ctx
deBruijn' _   t               = t

shift :: Int -> Int -> Term -> Term
shift c d (VarI inf str k)   = VarI inf str (if k<c then k else k+d)
shift c d (Abs inf str ty t) = Abs inf str ty (shift (c+1) d t)
shift c d (App inf t1 t2)    = App inf (shift c d t1) (shift c d t2)
shift c d t              = t

-- [j -> s] t 
subst :: Int -> Term -> Term -> Term
subst j s (VarI inf str k)   = if j == k then s else VarI inf str j
subst j s (Abs inf str ty t) = Abs inf str ty (subst (j+1) (shift 0 1 s) t)
subst j s (App inf t1 t2)    = App inf (subst j s t1) (subst j s t2)
subst j s (Cond inf t1 t2 t3)  = Cond inf (subst j s t1) (subst j s t2) (subst j s t3)
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
eval' (App _ (Abs _ "_" _ t) _) = Left t                                -- E-WILDCARD
eval' (App _ (Abs _ _ ty t) v)  = Left $ beta (eval1 v) t               -- E-AppAbs
eval' (App inf t1 t2)           = case eval' t1 of
                                Left t' -> Left $ App inf t' t2          -- E-App2
                                Right t' -> Left $ App inf t1 (eval1 t2) -- E-App1
eval' (Cond inf t1 t2 t3)       = case eval1 t1 of
                                (Literal _ (LBool _))  -> Left t2
                                _ -> Left $ Cond inf (eval1 t1) t2 t3
eval' (Let inf x t1 t2)         = case eval' t1 of
                                Left  t' -> Left $ Let inf x t' t2
                                Right t' -> Left $ beta t' t2
eval' t                     = Right t

eval1 :: Term -> Term
eval1 = either id id . eval'

{--
  Reduce term in n single step evaluations
--}
eval :: Term -> Term
eval t = case eval' t of
  Left t' -> eval t' -- reducible
  Right t' -> t'     -- irreducible


run :: Term -> Either String Term
run t = do
  let t' = deBruijn t
  case typeOf [] t'  of
    Right ty -> return $ eval $ t'
    Left e   -> Left $ "TypeChecker: " ++  e

