-- ASSIGNMENT 1 
-- dat326

-- Part 1

data TERM v = Empty
            | Singleton    (TERM v)
            | Union        (TERM v) (TERM v)
            | Intersection (TERM v) (TERM v)
            | Var          v

data PRED v = Elem     v        (TERM v)
            | Subset   (TERM v) (TERM v)
            | Implies  (PRED v) (PRED v)
            | And      (PRED v) (PRED v)
            | Or       (PRED v) (PRED v)
            | Not      (PRED v)

-- Part 2

newtype Set = S [Set]

type Env var dom = [(var,dom)]

lift :: Set -> [Set]
lift (S x) = x

-- eval 
eval :: Eq v => Env v Set -> TERM v -> Set
eval env Empty                = S []
eval env (Singleton t)        = S [eval env t]
eval env (Union t1 t2)        = S [] -- make v1 v2 into lists and concat? 
eval env (Intersection t1 t2) = S [] -- TODO
eval env (Var v)              = S [] -- TODO

-- check
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p

check :: Eq v => Env v Set -> PRED v -> Bool 
check env (Elem v t)      = True  -- TODO
check env (Subset t1 t2)  = True  -- TODO
check env (Implies p1 p2) = check env p1 ==> check env p2
check env (And p1 p2)     = check env p1  && check env p2
check env (Or p1 p2)      = check env p1  || check env p2
check env (Not p)         = not (check env p)

-- Part 3
