-- ASSIGNMENT 2 
-- DAT326

-- GROUP A2.18
--  Theres Wallinder
--  Theo Koraag
--  Sofia Alowersson

-- Recommended skeleton code

{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module A2 where
import Prelude hiding ((+),(-),(*),(/),negate,recip,(^),
                        pi, sin,cos,exp,fromInteger,fromRational)
import DSLsofMath.FunExp ( FunExp, derive, eval' )
import DSLsofMath.Algebra
    ( Transcendental(..),
      MulGroup(recip),
      Multiplicative(..),
      AddGroup(..),
      Additive(..) )


type Tri a    = (a,a,a)
type TriFun a = Tri (a -> a) -- = (a -> a, a -> a, a -> a)
type FunTri a = a -> Tri a   -- = a -> (a,a,a)


instance Additive a => Additive (Tri a) where
    (+) = addTri; zero = zeroTri
instance (Additive a, Multiplicative a) => Multiplicative (Tri a) where
    (*) = mulTri; one = oneTri
instance AddGroup a => AddGroup (Tri a) where
    negate = negateTri
instance (AddGroup a, MulGroup a) => MulGroup (Tri a) where
    recip = recipTri
(oneTri, negateTri, recipTri) = undefined

instance Transcendental a => Transcendental (Tri a) where
    pi = piTri; sin = sinTri; cos = cosTri; exp = expTri;
(piTri, sinTri, cosTri, expTri) = undefined;


-- eval and derive imported from FunExp 
-- Part 1

eval'' :: Transcendental a => FunExp -> (a -> a)
eval'' = eval' . derive


-- part (a)

-- example (exponential function): 
--      exp (a + b) == exp a * exp b

-- following test should hold (example from lecture 4.1.1) if homomorphism
h2 :: (Transcendental b, Eq b) => (a -> b, a -> a -> a, b -> b -> b) -> a -> a -> Bool
h2 (h, op1, op2) x y = h (op1 x y) == op2 (h x) (h y)

-- to prove that eval'' (second derivative) isn't a homomorphism then we need to find a case where 
--      eval'' (f op1 g) \= (eval'' f) op2 (eval'' g)
-- where op1 (syntactic domain) directly correstponds to op2 (semantic domain)




-- part (b)
-- tri instances

addTri :: (Additive a) => Tri a -> Tri a -> Tri a
addTri (a,b,c) (a',b',c') = (a + a', b + b', c + c') 

zeroTri :: (Additive  a) => Tri a
zeroTri = (zero, zero, zero)

mulTri = undefined 