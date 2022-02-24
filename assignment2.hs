-- ASSIGNMENT 2 
-- DAT326

-- GROUP A2.18
--  Theres Wallinder
--  Theo Koraag
--  Sofia Alowersson

-- Recommended skeleton code

{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, StandaloneDeriving, ConstraintKinds #-}
module A2 where
import Prelude hiding ((+),(-),(*),(/),negate,recip,(^),
                        pi, sin,cos,exp,fromInteger,fromRational)
import DSLsofMath.FunExp
import DSLsofMath.Algebra
    ( Transcendental(..),
      MulGroup(recip),
      Multiplicative(..),
      AddGroup(..),
      Additive(..), (^) )


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



-- part (a)

eval'' :: Transcendental a => FunExp -> (a -> a)
eval'' = eval' . derive


-- example (exponential function): 
--      exp (a + b) == exp a * exp b

-- following test should hold (example from lecture 4.1.1) if homomorphism
--h2 (h, op1, op2) x y = h (op1 x y) == op2 (h x) (h y)
--For all x and y should satisfy h2 to be a homomorphism

--      eval'' (f op1 g) \= (eval'' f) op2 (eval'' g)
-- where op1 (syntactic domain) directly correstponds to op2 (semantic domain)


-- TODO: formulate proof here 
{-
p(eval'') = exist (c1,c2,...cn) & (i=1 -> n) H_n(eval'',Ci, ci)
    where
        Ci : Syn -> Syn ->...-> Syn
        ci : Sem -> Sem ->...-> Sem 
not p(eval'') = forall (c1,c2,..cn) or (i=1 -> n) not H_n(eval'',Ci,ci)
    where
        Ci : Syn -> Syn ->...-> Syn
        ci : Sem -> Sem ->...-> Sem

to prove that eval'' (second derivative) isn't a homomorphism then we need to find a case where not H_n(eval'',Ci,ci) is true 
Because of the or notation. If we show that it is true then not P(Eval'') = true

let H_2(eval'',mul,(*)) = forall x, y :Syn. eval''( mul x y) == (eval'' x) * (eval'' y)
    where
            mul : syn -> syn -> syn
            (*) : Sem -> Sem -> Sem
    
    not H_2(eval'',mul,(*)) = exists x, y :Syn. eval''( mul x y) =/= (eval'' x) * (eval'' y)
    
    def : eval'' = eval'.derive = (eval.derive).derive
                eval' = eval.derive

    eval''(mul x y) = 
    (eval'.derive)(mul x y) = 
    (eval.derive).derive(mul x y)
    (eval.derive).(x * y' + x' * y)
        where
            y'=deriv y
            x'=deriv x
    eval(x*y''+ y'*x' +x'' *y + x'*y')
        where
            y'=deriv y
            x'=deriv x
            y''= derive.derive y
            x''= derive.derive x
    
    (eval x) * (eval y'') + (eval y') * (eval x') + (eval x'') * (eval y) + (eval x')*(eval y'))
    (eval x) * (eval'' y) + (eval' y) * (eval' x) + (eval'' x) * (eval y) + (eval' x)*(eval' y))
    =/= (eval'' x) * (eval'' y)

    => not H_2() is true
    => not p()   is true
      -- 
    
        -} 

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
                       (f' + f') * recip (f*f*f) * f' + negate (recip(f*f)) * f'') -- dubbel kolla senare


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

--part c
evalDD :: (Transcendental a) => FunExp -> FunTri a
evalDD f = \a -> (eval f a, eval (derive f) a, eval (dd f) a) -- eval' ?? 

f1 = X
f2 = X :+: (Const 1)


mulLeft f1 f2 a = evalDD exExp a
    where exExp = f1 :*: f2

mulRight f1 f2 a = mulTri (evalDD f1 a) (evalDD f2 a)

mulTest f1 f2 a = mulLeft f1 f2 a == mulRight f1 f2 a

--mulProof a b = h2 (evalDD, mul, mulDD) a b

--h2 (h, op1, op2) x y = h (op1 x y) == op2 (h x) (h y)

{-

newton :: (R → R) → R → R → R
newton f e x = if abs fx < e then x
else if fx0 6 0 then newton f e next
else newton f e (x + e)
where fx = f x
fx0 = undefined -- should be f
0 x (derivative of f at x)
next = x − (fx / fx0
)

-}

newtonTri :: (Tri REAL -> Tri REAL) -> REAL -> REAL -> REAL 
newtonTri f e x 
  | abs fx < e = x       
  | fx' /= zero = newtonTri f e next
  | otherwise = newtonTri f e (x + e)
    where 
        (fx, fx', fx'') = f (constTri x)
        next = x + negate (fx * recip fx')

test0 x = x*x
test1 x = x*x + negate one
test2 x = sin x
test3:: Int -> REAL -> Tri REAL -> Tri REAL
test3 n x y = y^n + negate (constTri x)
constTri x = (x, one, zero)

intervaltest test = map  (newtonTri test 0.001) [-2.0, -1.5..2.0]

intervalround test = map (round . (newtonTri test 0.001)) [-2.0, -1.5..2.0]

ex:: REAL -> [REAL]
ex x = newtonList test2 0.001 [x] 

ex2 n x y = newtonList (test3 n x) 0.001 [y]


newtonList :: (Tri REAL -> Tri REAL) -> REAL -> [REAL] -> [REAL] 
newtonList f e x
  | abs fx < e = x    
  | fx' /= zero = newtonList f e (x ++ [next])
  | otherwise = newtonList f e (x ++ [(last x) + e])
    where 
        (fx, fx', fx'') = f (constTri (last x))
        next = (last x) + negate (fx * recip fx')