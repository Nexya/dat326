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


 

