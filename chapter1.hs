import Numeric.Natural
import Data.Ratio

-- SYNTAX: what it looks like
-- SEMANTICS: what it means

--example
--the semantic of the type [Bool]
--is the infinite set {[],[F],[T],[FF],[FT],...}

--the semantics of Bool -> Bool is
-- {const True, id, not, const False}

-- identity function
id :: a -> a 
id x = x


-- constant fumction
const :: a -> b -> a
const x _ = x

-- higher-order functions
--  functions that manipulate functions
flip :: (a -> b -> c) -> (b -> a -> c)
flip op x y = op y x 

-- lambda expressions
id' :: p -> p
id' = \x  -> x

-- function composition fog = f(g(x))
-- f . g

-- The type Env v s is the syntax for the type of partial functions from v to s
type Env v s = [(v,s)]
-- example of this type
env1 :: Env String Int
env1 = [("x", 17),("y",38)]
-- meaning that x is mapped to 17 and y to 38
-- the somantic domain is the set of partial functions and can be represented as v -> Maybe s

-- the evaluation maps the syntax to the semantics, and therefore has the following type
evalEnv :: Eq v => Env v s -> (v -> Maybe s)
-- Eq v => meaning that the function only works for types who support a Boolean equality check
evalEnv vss x = findFst vss
    where findFst ((v,s):vss) | x == v    = Just s      -- search for the first occurance of x in the list of (v,s)
                              | otherwise = findFst vss
          findFst [] = Nothing 


-- TYPE, NEWTYPE AND DATA

-- type: creates a type synonym, only adds readability
--      if declared with parameters as Env, it is a type constructor (a function at the type level)

-- newtype: stronger version of type, makes sure that values kan be kept apart
-- example
newtype Count    = Cou   Int
newtype DistFeet = DisFt Int 
newtype Year     = Yea   Int

pop :: Count
pop = Cou 562;

hei:: DistFeet
hei = DisFt 2150;

est :: Year
est = Yea 1951;

-- this makes sure that the values 562, 2150 and 1951 cannot be mixed together since they are of different types

-- data: used for syntax trees
data Exp = Con Integer
    | Var String
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    deriving (Eq, Show)

data Maybe' a = Just' a | Nothing' 
-- a new type Maybe for every type a 
-- a constrtuctor Just :: a -> Maybe a to represent "just a value"
-- a constructor Nothing :: Maybe a to represent "no value"
--example

inv :: (Eq a, Num a, Fractional a) => a -> Maybe' a
inv 0 = Nothing'
inv r = Just' (1 / r)

-- sequences
type N     = Natural
type Qp    = Ratio N 
type Seq a = N -> a

idSeq :: Seq N
idSeq i = i             -- {1,2,3,4,...}

invSeq :: Seq Qp
invSeq i = 1 % (1 + i)  -- {1/1, 1/2, 1/3,...}

pow2 :: Num r => Seq r
pow2 = (2^)             -- {1,2,4,8,...}

conSeq :: a -> Seq a
conSeq c i = c          -- {c,c,c,c,...}

addSeq :: Num a => Seq a -> Seq a -> Seq a
addSeq f g i = f i + g i 

liftSeq2 :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
liftSeq2 op f g i = op (f i) (g i)  -- {op (f 0) (g0), op (f 1) (g 1),...}

liftSeq1 :: (a -> b) -> Seq a -> Seq b
liftSeq1 h f i = h (f i)

liftSeq0 :: a -> Seq a
liftSeq0 c i = c

