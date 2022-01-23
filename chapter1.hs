
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