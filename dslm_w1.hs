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