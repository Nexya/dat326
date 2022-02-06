-- DSL for propositional calculus (or propositional logic)
--  False, True, Not, And, Or, Implies

-- we model the abstract syntax of propositions as a datatype
data Prop = Implies Prop Prop | And  Prop Prop | Or  Prop Prop
          | Not     Prop      | Name Name      | Con Bool

type Name = String

-- evaluation (check) for prop
type Env = Name -> Bool

eval :: Prop -> Env -> Bool
eval (Implies p q) env = eval p env ==> eval q env
eval (And p q)     env = eval p env  &&  eval q env
eval (Or p q)      env = eval p env  ||  eval q env
eval (Not p)       env = not (eval p env)
eval (Name n)      env = env n
eval (Con t)       env = t

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p

-- tautology-tester
isTautology :: Prop -> Bool
isTautology p = all (eval p) (envs (freeNames p))

envs :: [Name] -> [Env]
envs []     = [error "envs: never used"]
envs (n:ns) = [\n' -> if n == n' then b else e n'
              | b <- [False, True]
              , e <- envs ns]

freeNames :: Prop -> [Name]
freeNames = error "exercise"

-- proofs for propositional logic 
data Proof  =  TruthIntro                   |  FalseElim Proof
            |  AndIntro  Proof  Proof
            |  AndElimL  Prop   Proof       |  AndElimR  Prop  Proof
            |  OrIntroL  Proof              |  OrIntroR  Proof
            |  OrElim Prop Prop Proof Proof Proof
            |  NotIntro Prop Proof Proof    |  NotElim Proof
            |  Assume Prop
            |  ImplyIntro (Proof -> Proof)  |  ImplyElim  Prop  Proof Proof


checkProof :: Proof -> Prop -> Bool
checkProof = undefined 