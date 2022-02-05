-- extra lectures for Haskell-introduction/repetition
{-# LANGUAGE GADTs #-}
module Either where
import Prelude hiding (id, const, (.), fst, snd,swap, either, Either(Left,Right)) -- hide to avoid clashes


-- PART 1

{--
By type driven development we mean using parametrically polymorphic types to guide the implementation.
Will be used to study function definitions of mathematics during the course.
--}

-- FUNCTIONS
{-- what is a? 
a is just a name of some unknown type 
a type variable (unconstrained)
returns a value of same type
can be written as:
    id :: forall a. a->a
which can mean: 
    id :: Int -> Int
    id :: Bool -> Bool
    id :: (Int->Bool) -> (Int->Bool)
--}
id :: a -> a
id x = x
    -- x :: a, rhs :: a
id' :: a -> a
id' = \x -> x  -- x :: a, x/=a
    -- lambda expression

const :: a -> (b -> a) -- two arguments
const x y = x
    -- x :: a, y :: b
const' :: a -> (b -> a) -- type variables 
const' = \x -> \y -> x  -- value variables

-- FUNCTION COMPOSITION
{--
(.) :: (b->c) -> (a->b) -> (a->c)
f . g = rhs
    where rhs x = f hi
           where hi = g ho
              -- hi :: b
                  where ho = x -- referentially transparent
                               -- meaning we can replace ho with x
                               -- hi = g x and so on 
                     -- ho :: a            
        -- rhs :: a -> c
        -- f   :: b -> c
        -- g   :: a -> b
        -- x   :: a
--}
-- simplified 
(.) :: (b->c) -> (a->b) -> (a->c)
(f . g) x = f (g x)

-- test functions
f1 ::           Int -> Bool
f2 :: Double -> Int
f3 :: Double        -> Bool
f1 = even
f2 = round
f3 = f1 . f2 -- check if nearest integer is even

-- PART 2

{-- Pairs
The type (a,b) has values of the form 
         (x,y) where x :: a and y :: b
--}

fst :: (a,b) -> a
fst    (x,_) =  x -- _ shows that parameter is not needed
                  -- a "don't care"/wildcard 
snd :: (a,b) -> b
snd    (_,y) =  y

swap :: (a,b) -> (b,a)
swap    (x,y) =  (y,x)

-- swap using fst and snd
swap' :: (a,b) -> (b,a)
swap' p =  (snd p, fst p)

-- swap using lmabda expressions
swap'' :: (a,b) -> (b,a)
swap'' = \p -> (snd p, fst p)
swap''' :: (a,b) -> (b,a)
swap''' = \(x,y) -> (y, x)

assoc :: (a,(b,c)) -> ((a,b),c)
assoc    (x,(y,z)) =  ((x,y),z)

-- w1 bonus , function to pair
f2p :: (a -> (b,c)) -> (a -> b, a -> c)
--  :: (          ) -> (      ,       )   -- one argument, returns a pair
f2p f = ( fst . f, snd . f)
    -- f :: a -> (b, c)

p2f ::  (a -> b, a -> c) -> (a -> (b,c))
--  ::  (      ,       ) -> (          )  
p2f (f, g) x = (f x, g x)
    -- f :: a -> b
    -- g :: a -> c 
    -- rhs :: a -> (b, c)

f4 :: Double -> (Int, Bool)
f4 x = (round x, f3 x)

mypair :: (Double -> Int, Double -> Bool)
mypair = f2p f4
onefun :: Double -> Int
onefun = fst mypair
otherfun :: Double -> Bool
otherfun = snd mypair
mynewfun :: Double -> (Int, Bool)
mynewfun = p2f mypair

what :: (a -> b, a -> c) -> (a -> b, a -> c)
what = f2p . p2f
-- what == id

ex1 :: (Integer -> Integer, Integer -> Bool)
ex1 = ((+1), even)
ex2 :: (Integer -> Integer, Integer -> Bool)
ex2 = what ex1

-- Sum types (Either)
-- like disjoint union, either the one or the other
data Either a b where
    Left  :: a -> Either a b
    Right :: b -> Either a b
    deriving Show

either :: (a -> r) -> (b -> r) -> (Either a b -> r)
--     :: (      ) -> (      ) -> (               )
--     :: (      ) -> (      ) -> (         ) -> r
either f _ (Left  x) = f x
either _ g (Right y) = g y
    -- f        :: a -> r
    -- Left x   :: Either a b
    --      x   ::        a 
    -- g        :: b -> r
    -- Right y  :: Either a b
    --       y  ::          b

ei1 :: Either String b
ei1 = Left "hi"
ei2 :: Either a Bool
ei2 = Right False

li1 :: [Either String Bool]
li1 = [ei1, ei2]

showB :: Bool   -> String
showB = show

d       :: String -> String 
d s = s++s  
-- not  :: Bool   -> Bool

te1 :: String
te1 = either d showB ei1

te2 :: String
te2 = either d showB ei2