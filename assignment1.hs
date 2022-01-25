-- ASSIGNMENT 1 
-- dat326


-- Part 1

data TERM v = Empty
            | Singleton    (TERM v)
            | Union        (TERM v) (TERM v)
            | Intersection (TERM v) (TERM v)
            | Powerset     (TERM v)
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

-- eval 
eval :: Eq v => Env v Set -> TERM v -> Set
eval env Empty         = S []
eval env (Singleton v) = S [eval env v]
eval env (Union v1 v2) = S [] -- make v1 v2 into lists and concat? 