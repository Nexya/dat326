{--
  Group: a1.07
  Group member: Theres Wallinder, Pavlos Stampoulis, Jesper FÃ¼hr
--}

import Data.List
import Data.Maybe (fromJust)

-------- PART 1
data TERM a
  = Empty
  | Singleton (TERM a)
  | Union (TERM a) (TERM a)
  | Intersection (TERM a) (TERM a)
  | TV a
  deriving (Show)

data PRED a
  = Elem (TERM a) (TERM a)
  | Subset (TERM a) (TERM a)
  | And (PRED a) (PRED a)
  | Or (PRED a) (PRED a)
  | Implies (PRED a) (PRED a)
  | Not (PRED a)
  deriving (Show)

-------- PART 2

newtype Set = S [Set]
  deriving (Show)
instance Eq Set where
  (==) (S []) (S []) = True
  (==)  _     (S []) = False
  (==) (S [])  _     = False
  (==) (S a)  (S b)  =
    all (`elem` b) a
      && all (`elem` a) b

type Env var dom = [(var, dom)]

eval :: Eq v => Env v Set -> TERM v -> Set
eval env Empty = S []
eval env (Singleton a) = S [eval env a]
eval env (TV a) = fromJust $ lookup a env
eval env (Union a b) = S $ nub $ lift (eval env a) `union` lift (eval env b) -- was incorrect due to eq
eval env (Intersection a b) = S $ nub $ lift (eval env a) `intersect` lift (eval env b) -- was incorrect due to eq

check :: Eq v => Env v Set -> PRED v -> Bool
check env (Elem a b) = contains (eval env a) (eval env b)
check env (Subset a b) = all (\x -> contains x $ eval env b) $ lift $ eval env a
check env (And a b) = check env a && check env b
check env (Or a b) = check env a || check env b
check env (Implies a b) = check env a `implies` check env b
check env (Not a) = not $ check env a

-------- PART 3

vonNeumann :: Integer -> TERM a
vonNeumann 0 = Empty
vonNeumann n = Union a (Singleton a)
  where
    a = vonNeumann (n - 1)

claim1 :: Integer -> Integer -> Bool
claim1 n1 n2 = (n1 <= n2) `implies` check [("h", S [])] (Subset a b)
  where
    a = vonNeumann n1
    b = vonNeumann n2

claim2 :: Integer -> Bool
claim2 n = eval a (vonNeumann n) == S (map (eval a . vonNeumann) [0 .. (n -1)])
  where
    a = [("h", S [])]

---- Help thingies
lift :: Set -> [Set]
lift (S x) = x


contains :: Set -> Set -> Bool
contains e (S x) = e `elem` x

implies :: Bool -> Bool -> Bool
True `implies` x = x
False `implies` _ = True

cardinality :: Set -> Integer
cardinality (S x) = toInteger $ length x

{--
  Used for tests
--}

big :: (Eq t, Num t) => t -> TERM a
big 0 = Empty
big n = Singleton (big (n -1))

-- Should return a set of sets of depth 0 to n-1
uni :: (Eq t, Num t) => t -> TERM a
uni 0 = Empty
uni n = Union (big n) (uni (n -1))

uni' :: (Eq t, Num t) => t -> TERM a
uni' 0 = Empty
uni' n = Union (uni (n -1)) (big n)

int :: (Eq t, Num t) => t -> TERM a
int 0 = Empty
int n = Intersection (big n) (int (n -1))

-- should evaluate to Singleton Empty represented in Set
test :: TERM a
test = Intersection (Intersection (Singleton Empty) (Union (Singleton Empty) Empty)) (Singleton Empty)

a = S [S [], S []]

b = S [S []]

c = S [a, a, b]

d = S [a, b, a]

e = S [a, b, b]

-- Should return true, at all times
testeroo = a == b