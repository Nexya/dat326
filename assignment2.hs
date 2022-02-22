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
import DSLsofMath.FunExp ( FunExp, derive, eval', eval )
import DSLsofMath.Algebra
    ( Transcendental(..),
      MulGroup(recip),
      Multiplicative(..),
      AddGroup(..),
      Additive(..) )


type Tri a    = (a,a,a)      -- (f, f', f'')
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

instance Transcendental a => Transcendental (Tri a) where
    pi = piTri; sin = sinTri; cos = cosTri; exp = expTri;



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

-- TODO: formulate proof here 

-- TODO: example that should return false?  
ex = undefined


-- part (b)
-- tri instances

addTri :: (Additive a) => Tri a -> Tri a -> Tri a
addTri (f,f',f'') (g,g',g'') = (f + g, f' + g', f'' + g'')

zeroTri :: (Additive  a) => Tri a
zeroTri = (zero, zero, zero)

mulTri :: (Multiplicative a, Additive a) => Tri a -> Tri a -> Tri a
mulTri (f,f',f'') (g,g',g'') = ( f * g,
                                 f' * g + f * g',
                                 f'' * g + f' * g' + f' * g' + f * g'')


oneTri :: (Additive a, Multiplicative a) => Tri a
oneTri = (one, zero, zero)

negateTri :: (AddGroup  a) => Tri a -> Tri a
negateTri (a,b,c) = (negate a, negate b, negate c)

recipTri :: (AddGroup a, MulGroup a) => Tri a -> Tri a
recipTri (f,f',f'') = (recip f,
                       negate (recip (f*f)) * f',
                       (f' + f') * recip (f*f*f) * f' + negate (recip(f*f)) * f'')


piTri :: Transcendental a => Tri a
piTri = (pi, zero, zero)

expTri :: Transcendental a => Tri a -> Tri a
expTri (f,f',f'') =(exp f,
                    exp f * f',
                    exp f * f'' + exp f * f' * f')

sinTri :: Transcendental a => Tri a -> Tri a
sinTri (f,f',f'') = (sin f,
                     cos f * f',
                     negate (sin f) * f' * f' + cos f * f'')

cosTri :: Transcendental a => Tri a -> Tri a
cosTri (f,f',f'') = (cos f,
                    negate(sin f) * f', 
                    negate (cos f) * f' * f' + negate (sin f) * f'') 

dd :: FunExp -> FunExp
dd = derive . derive

evalDD :: (Transcendental a) => FunExp -> FunTri a
evalDD f a = (eval f a, eval (derive f) a, eval (dd f) a) -- eval' ?? 