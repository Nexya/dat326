import PropositionalLogic
-- Exercise 2.14

(==>) :: Bool -> Bool -> Bool
False  ==> _ {-"\quad"-}  = True
True   ==> p              = p

dnf :: Prop -> Prop 
-- given a proposition p, find another proposition "dnf p" equivalent to p
-- such that dnf is an AND of ORs
-- For example if you have names "a" and "b"
-- (a and b), (a and !b), (!a and !b), (!a and b)

dnf p = dnfOfProp names (eval p)
    where names = namesInProp p

dnfOfProp :: [Name] -> (Env -> Bool) -> Prop
dnfOfProp names f = foldl Or (Con False) $ map propOfList trueCombinations
    where allCombinations  = combs names
          trueCombinations = filter (f . envOfList) allCombinations

propOfList :: [(Name,Bool)] -> Prop
propOfList ls = foldl And (Con True) $ map propOfPair ls

propOfPair :: (Name, Bool) -> Prop
propOfPair (n,b) = if b then Name n else Not (Name n)

namesInProp :: Prop -> [Name] 
namesInProp (And p q)      = namesInProp p ++ namesInProp q
namesInProp (Or p q)       = namesInProp p ++ namesInProp q
namesInProp (Implies p q)  = namesInProp p ++ namesInProp q
namesInProp (Not p)        = namesInProp p
namesInProp (Name n)       = [n]
namesInProp (Con _)        = []

combs :: [Name] -> [[(Name,Bool)]]
combs []            = [[]]
combs (name: names) = [(name,b): ls | b <- [False,True], ls <- combs names]

envOfList :: [(Name, Bool)] -> Env
envOfList []            n = True 
envOfList ((n', b): ls) n = if n' == n then b else envOfList ls n