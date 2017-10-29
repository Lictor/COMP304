\begin{code}

-- Initial code for COMP304 Assignment 3, 2017.

-- Author: Lindsay Groves, VUW, 2017.

-- This is an interpreter for a simple while program language, as presented in
-- lectures.  The assignment asks you to make several extensions to the language.

-- You run a program using the run function, which takes a program and an
-- initial store and returns the store resulting from executing the program, if
-- it executes successfully.

-- There are some example programs and example stores at the end of this file, so
-- you can try running some simple tests, e.g. run p1 s1 runs program p1 with
-- store s1 (which doesn't do much!).


-- Map is used to implement the store.
import Map
-- Variable names are assumed to be single characters.
type Var = Char
type Prog = [Stmt]

type Array = [Var]

data Val = Int Int | Bool Bool
           deriving (Show)
-- A statement can be a skip, assignment, if or do statement.

-- data Type = IntT | BoolT
--             deriving (Show)
-- type SymTab = Map Var Type
data Stmt = Skip 
          | Asgn Var Exp 
          | If Exp [Stmt] [Stmt] 
          | Do Exp [Stmt]
            deriving (Show)
-- An expression can be a constant, a variable, or a binary operator applied to two expressions

data Exp = Const Val 
         | Var Var 
         | Bin Op Exp Exp 
         | Not Exp
           deriving (Show)

-- An operation is +, -, *, /, ^, =, /=, <, <=, > or >=

data Op = Plus 
        | Minus 
        | Times 
        | Div 
        | Power 
        | Eq 
        | Ne 
        | Lt 
        | Le 
        | Gt 
        | Ge 
        | And 
        | Or 
          deriving (Eq, Show)

-- A relational operation is 

-- A store is a map from variables to values
type Store = Map Var Val
-- To run a program with a given initial store, we just pass the program and
-- store to exec.
run :: Prog -> Store -> Store

run prog input = exec prog input
-- To execute a program, we just execute each statement in turn, passing the
-- resulting state to the next statement at each step.
exec :: Prog -> Store -> Store
exec prog [(_,_)] = error "Store is empty"
exec [] store = store
exec (stmt : rest) store = exec rest (exec' stmt store)

-- Execute a single statement, according to its semantics

exec' :: Stmt -> Store -> Store

exec' Skip store = store

exec' (Asgn var exp) store =
      setVal var (eval exp store) store

exec' (If cond thenPart elsePart) store =
      if b
      then exec thenPart store
      else exec elsePart store
      where (Bool b) = eval cond store

exec' (Do cond body) store =
      if not b 
      then store
      else exec' (Do cond body)
                 (exec body store)
      where (Bool b) = eval cond store
-- Evaluate an expression, according to its type

-- checkBool :: Val -> Bool
-- checkBool v 
--     | v = Bool == True
--     | otherwise = error "condition must be a boolean"

eval :: Exp -> Store -> Val
eval (Const n) _ = n
eval (Var v) s | hasKey v s = getVal v s
     | otherwise = error ("Undefined variable " ++ [v])
eval (Bin op x y) s = apply op (eval x s) (eval y s)
eval (Not x) s = applyNot (eval x s)

-- varsOk :: Prog -> Bool
-- varsOk (_, []) = True
-- varsOk (vars, stmt : stmts) =
--    varsOk' (vars, stmt) && varsOk (vars, stmts)

-- varsOk' :: (SymTab, Stmt) -> Bool
-- varsOk' (vars, Skip) = True
-- varsOk' (vars, Asgn v e) = hasKey v vars &&
--           varsOk'' (vars, e)
-- varsOk' (vars, If e s1 s2) = varsOk (vars,s1) && varsOk (vars,s2)
-- varsOk' (vars, Do e s) = varsOk (vars,s)

-- varsOk'' :: (SymTab, Exp) -> Bool
-- varsOk'' (vars, Const v) = True
-- varsOk'' (vars, Var v) 
--   | hasKey v vars = True
--   | otherwise = error ("Undeclared variable" ++ (show v))

-- varsUsed :: Prog -> [Var]
-- varsUsed (_, stmts) = varsUsed' stmts

-- varsUsed' :: [Stmt] -> [Var]
-- varsUsed' stmts = concat (map varsUsed'' stmts)

-- varsUsed'' :: Stmt -> [Var]
-- varsUsed'' Skip = []
-- varsUsed'' (Asgn var exp) = var : varsUsed''' exp
-- varsUsed'' (If cond s1 s2) = (varsUsed''' cond) ++ (varsUsed' s1) ++ (varsUsed' s2)
-- varsUsed'' (Do cond body) = (varsUsed''' cond) ++ (varsUsed' body)

-- varsUsed''' :: Exp -> [Var]
-- varsUsed''' (Const _) = []
-- varsUsed''' (Var v) = [v]
-- varsUsed''' (Not e) = varsUsed''' e
-- varsUsed''' (Bin _ e1 e2) = (varsUsed''' e1) ++ (varsUsed''' e2)

-- expType :: Exp -> SymTab -> Type
-- expType (Const _) _ = IntT
-- expType (Var v) t
--   | hasKey v t = getVal v t
--   | otherwise = error ("Undeclared variable " ++ (show v))
-- expType (Bin op x y) t =
--    opType op (expType x t) (expType y t)

-- opType :: Op -> Type -> Type -> Type
-- opType Plus IntT IntT = IntT
-- opType Minus IntT IntT = IntT
-- opType Times IntT IntT = IntT
-- opType Div IntT IntT = IntT
-- opType Power IntT IntT = IntT
-- opType Eq IntT IntT = BoolT
-- opType Ne IntT IntT = BoolT
-- opType Lt IntT IntT = BoolT
-- opType Le IntT IntT = BoolT
-- opType Gt IntT IntT = BoolT
-- opType Ge IntT IntT = BoolT
-- opType And BoolT BoolT = BoolT
-- opType Or BoolT BoolT = BoolT
-- opType op t1 t2 = error ("Illegal application " ++ "\nOperator: " ++ (show op) ++ "\ntype1: " ++ (show t1) ++ "\ntype2: " ++ (show t2))

-- Apply an operator and return its value with its correct type
apply :: Op -> Val -> Val -> Val
apply Plus (Int x) (Int y) = Int (x + y)
apply Minus (Int x) (Int y) = Int (x - y)
apply Times (Int x) (Int y) = Int (x * y)
apply Div (Int x) (Int y) = Int (x `div` y)
apply Power (Int x) (Int y) = Int (x ^ y)
apply Eq (Int x) (Int y) = Bool (x == y)
apply Ne (Int x) (Int y) = Bool (x /= y)
apply Lt (Int x) (Int y) = Bool (x < y)
apply Le (Int x) (Int y) = Bool (x <= y)
apply Gt (Int x) (Int y) = Bool (x > y)
apply Ge (Int x) (Int y) = Bool (x >= y)
apply And (Bool x) (Bool y) = Bool (x && y)
apply Or (Bool x) (Bool y) = Bool (x || y)
apply op x y =
  error ("Illegal arguments to" ++ "\nOperator: " ++ (show op) ++ " \nx: " ++ (show x) ++ " \ny: " ++ (show y))

-- Apply the not operator, I have done this separately because it only take 1 argument
applyNot :: Val -> Val
applyNot x = applyNot' x

applyNot' :: Val -> Val
applyNot' (Bool v) 
    | v == True = (Bool False)
    | otherwise = (Bool True) 
applyNot' (Int v) = error "Variable must be a boolean type"
-- > evalc :: Cond -> Store -> Bool
-- > evalc (Cond rel x y) s = applyc rel (eval x s) (eval y s)
-- applyc is similar to apply, but applies relations:

-- Some sample expressions

-- e0 = Const 0
-- e1 = Const 1
-- e2 = Const 2
-- e3 = Var 'x'
-- e4 = Bin Plus e3 e1
-- e5 = Bin Plus (Var 'i') e1

-- Some sample stores

-- s1 = []
-- s2 = [('x',1)]
-- s3 = [('x',1), ('y',2)]

-- Some sample programs

-- p1 = [Skip]
-- p2 = [Skip, Skip]
-- p3 = [Asgn 'x' e1]
-- p4 = [Asgn 'x' (Var 'x')]
-- p5 = [Asgn 'x' (Var 'y')]
-- p6 = [Asgn 'x' (Bin Plus (Var 'x') (Const 1))]
-- p7 = [Asgn 'x' e1, Asgn 'y' e2,
--   (If (Cond Eq (Var 'x') (Var 'y')) [Asgn 'z' e1] [Asgn 'z' e2])]
-- p8 = [Asgn 'i' e1, Asgn 's' e0,
--   (Do (Cond Lt (Var 'i') (Var 'n')) 
--        [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5])]

--Expressions for testing
ex0 = Const (Int 1)
ex1 = Const (Int 2)
ex4 = Const (Int 3)

ex2 = Const (Bool True)
ex3 = Const (Bool False)
-- Statments for testing
st0 = []
st1 = [('x',(Int 1))]
st2 = [('x',(Int 1)), ('y',(Int 2))]
st3 = [('x',(Bool True)), ('z', (Bool False))]
-- Program for testing
pr0 = []
pr1 = [Skip,Skip]
pr2 = [Asgn 'x' ex0, Asgn 'y' ex1]
pr3 = [Asgn 'z' ex2, Asgn 'x' ex3]
pr4 = [If (Not (Const (Bool False))) 
      [(Asgn 'x' ex3),(Asgn 'z' ex2)] 
      [(Asgn 'x' ex2),(Asgn 'z' ex3)]]
pr5 = [If (Const (Bool False)) 
      [(Asgn 'x' ex3),(Asgn 'z' ex2)] 
      [(Asgn 'x' ex2),(Asgn 'z' ex3)]]
pr6 = [If (Bin Or ex2 ex3) [Asgn 'x' ex1] [Asgn 'x' ex0]]
pr7 = [If (Bin And ex2 ex3) [Asgn 'x' (Bin Plus ex0 ex4)] 
      [Asgn 'x' (Bin Plus ex0 ex1)]]
pr8 = [If (Bin And ex2 ex3) [Asgn 'x' (Bin Times ex0 ex4)] 
      [Asgn 'x' (Bin Times ex1 ex4)]]

-- asgn_int :: Bool
-- asgn_int = run pr2 st2 == [('y',Int 2),('x',Int 1)]

checkTests :: Bool
checkTests = checkTests' []

checkTests' :: [Bool] -> Bool
checkTests' [x] = x
checkTests' (x:xs)
        | x == True = checkTests' xs
        | otherwise = False

\end{code}