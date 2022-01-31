import Data.List ( intersect, nub, union )
import Data.Maybe (fromJust)
-- ASSIGNMENT 1 
-- dat326

-- Part 1

data TERM v = Empty
            | Singleton    (TERM v)
            | Union        (TERM v) (TERM v)
            | Intersection (TERM v) (TERM v)
            | Var          v
    deriving (Eq,Show)

data PRED v = Elem     (TERM v) (TERM v)
            | Subset   (TERM v) (TERM v)
            | Implies  (PRED v) (PRED v)
            | And      (PRED v) (PRED v)
            | Or       (PRED v) (PRED v)
            | Not      (PRED v)
    deriving (Eq,Show)

-- Part 2

newtype Set = S [Set]
    deriving (Eq,Show)


type Env var dom = [(var,dom)]

{-- 
rmDupl :: (Eq a, Ord a) => [a] -> [a]
rmDupl = map head . group . sort
--}


-- helpers
lift :: Set -> [Set]
lift (S x) = x

-- eval 
eval :: Eq v => Env v Set -> TERM v -> Set
eval env Empty                = S []
eval env (Singleton t)        = S [eval env t]
eval env (Union t1 t2)        = S $ nub $ lift (eval env t1) `union` lift (eval env t2)
eval env (Intersection t1 t2) = S $ nub $ lift (eval env t1) `intersect` lift (eval env t2)
-- $ filter (\x -> x `elem` lift (eval env t1)) (lift(eval env t2))
eval env (Var v)              = fromJust $ lookup v env

-- check
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p

contains :: Set -> Set -> Bool
contains e (S (x : xs))
  | e == x    = True
  | otherwise = contains e (S xs)
contains e (S []) = False

check :: Eq v => Env v Set -> PRED v -> Bool
check env (Elem v t)      = contains (eval env v) (eval env t)
check env (Subset t1 t2)  = all (\x -> contains x $ eval env t2) $ lift $ eval env t1
check env (Implies p1 p2) = check env p1 ==> check env p2
check env (And p1 p2)     = check env p1  && check env p2
check env (Or p1 p2)      = check env p1  || check env p2
check env (Not p)         = not $ check env p

-- Part 3

vonNeumann :: Integer -> TERM Integer
vonNeumann 0 = Empty
vonNeumann n = Union (vonNeumann (n-1)) (Singleton (vonNeumann (n-1)))


claim1 :: Integer -> Integer -> Bool
claim1 n1 n2 = (n1 <= n2) ==> check [] (vonNeumann n1 `Subset` vonNeumann n2)

claim2 :: Integer -> Bool
claim2 n = eval [] (vonNeumann n) == S (map (eval [] . vonNeumann) [0..(n-1)])