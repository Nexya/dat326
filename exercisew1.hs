--Exercise 1.1

data Exp = Con Integer
    | Var String
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    deriving (Eq, Show)

-- 1.
--(a)
a1 :: Exp
a1 = Plus (Con 2) (Con 2)
-- (b)
a2 :: Exp
a2 = Plus a1 (Times (Con 7) (Con 9))
-- (c)
a3 :: Exp
a3 = Minus part1 part2
    where
        part1 = Times (Con 8) (Plus (Con 2) (Con 11))       -- 8 * (2 + 11)
        part2 = Times (Plus (Con 3) (Con 7)) (Plus a1 a2)   -- (3 + 7)  * (a1 + a2)

--2.
eval :: Exp -> Integer
eval (Con c) = c
eval (Var name)    = varVal name -- added later for error fix
eval (Plus e1 e2)  = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Minus e1 e2) = eval e1 - eval e2

--3.
--(a)
c1 :: Exp
c1 = Times  (Minus  (Var "x")
                    (Con 15))
            (Times  (Plus   (Var "y")
                            (Con 12))
                    (Var "z") )

--(b)
varVal :: String -> Integer
varVal name
    | name == "x" = 5
    | name == "y" = 8
    | name == "z" = 13
    | otherwise  = error ("Variable " ++ name ++ " not found")

--(c)
eval' :: Exp -> Integer
eval' (Con c)       = c
eval' (Var name)    = varVal name
eval' (Plus e1 e2)  = eval' e1 + eval' e2
eval' (Times e1 e2) = eval' e1 * eval' e2
eval' (Minus e1 e2) = eval' e1 - eval' e2


--Exercise 1.2
data E2 a = Con' a
        | Var' String
        | Plus'  (E2 a) (E2 a)
        | Minus' (E2 a) (E2 a)
        | Times' (E2 a) (E2 a)
    deriving (Eq, Show)

--1.
--(a)
a1' :: E2 Double
a1' = Plus' (Con' 2.0) (Var' "a")

--(b)
a2' :: E2 Double
a2' = Plus' (Con' 5.3) (Times' (Var' "a") (Var' "b")) 

--(c)
a3' :: E2 a
a3' =   Minus'
        (Times' (Var' "a") (Plus' (Var' "b") (Var' "c")))
        (Times' (Plus' (Var' "d") (Var' "e")) (Plus' (Var' "f") (Var' "a")))

--2.
type Env v s = [(v,s)]
type Table a = Env String  a

--(a)
vars :: Table Double 
vars = [("a",1.5),("b",4.8),("c",2.4),("d",7.4),("e",5.8),("f",1.7)]

--(b)
varVal' :: Table a -> String -> a
varVal' t s | fst (head t) == s = snd (head t)
            | otherwise         = varVal' (tail t) s

--(c)
eval'' :: Num a => Table a -> E2 a -> a
eval'' t (Con' c)       = c
eval'' t (Var' name)    = varVal' t name
eval'' t (Plus' e1 e2)  = eval'' t e1 + eval'' t e2
eval'' t (Minus' e1 e2) = eval'' t e1 - eval'' t e2
eval'' t (Times' e1 e2) = eval'' t e1 * eval'' t e2

 
--Exercise 1.3
--Cardinality = number of item in set
--Either a b has cardinality A + B
--(a,b)      has cardinality A * B
--a -> b     has cardinality B^A 

--Exercise 1.4
--1.
-- Bool -> Maybe Bool

-- Bool = {True, False}
-- Maybe Bool = {Just True, Just False, Nothing}
-- Cardinality: 3^2 = 9

--2. 
-- Maybe Bool -> Bool

-- Maybe Bool = {Just True, Just False, Nothing}
-- Bool = {True, False}
-- Cardinality: 2^3 = 8

--3.
-- Maybe(Bool, Maybe(Bool, Maybe Bool))
-- (a,b) har cardinality a*b 
-- Maybe(a.b) has cardinality 1+(a*b) where +1 is Nothing
-- Cardinality:  1+(2*(1+(2*3))) = 15

--Exercise 1.5
isoR :: (Bool -> t) -> (t, t)
isoR f = (f True, f False )

isoL :: (t, t) -> (Bool -> t)
isoL (x,y) = \z -> if z then x else y
    -- True -> x
    -- False  -> y

-- now show isoL o isoR nad isoR o isoL = id
--  or: isoL and isoR are bijections

--isoL o isoR = id
--in Haskell: (isoL . isoR) f = id f (for all f)
--        <=> (isoL (isoR)) = f
--        <=> (isoL (f True, f False)) = f
--        <=> \z -> if z then (f True) else (f False) = f
--        <=> \z -> case z of 
--                    True -> f True
--                    False -> f False

-- This is true for any  function from Bool to any type t

--Exercise 1.8
-- for a sequence a what is a◦(1+)? What is (1+)◦a?
-- since
--      f◦g = \x -> f(g(x))
-- then
--      a◦(+1) = \x -> a(1+x)
--      (+1)◦a = \x -> 1+ (a(x)) 


--1.6
f2p :: (a -> (b,c)) -> (a -> b, a -> c)
f2p = error "TODO"
 
