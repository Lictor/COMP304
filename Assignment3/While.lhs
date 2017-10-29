Initial code for COMP304 Assignment 3, 2017.

Author: Lindsay Groves, VUW, 2017.

This is an interpreter for a simple while program language, as presented in
lectures.  The assignment asks you to make several extensions to the language.

You run a program using the run function, which takes a program and an
initial store and returns the store resulting from executing the program, if
it executes successfully.

There are some example programs and example stores at the end of this file, so
you can try running some simple tests, e.g. run p1 s1 runs program p1 with
store s1 (which doesn't do much!).


Map is used to implement the store.

> import Map

Variable names are assumed to be single characters.

> type Var = Char

A program is just a list of statements

> data Type = IntT | BoolT
> type SymTab = Map Var Type
> type Prog = (SymTab, [Stmt])

> data Val = conInt Int | conBool Bool
>            deriving (Show)

A statement can be a skip, assignment, if or do statement.

> data Stmt = Skip | Asgn Var Exp | If Exp [Stmt] [Stmt] |
>             Do Exp [Stmt]
>             deriving (Show)

An expression can be a constant, a variable, or a binary operator applied to two expressions

> data Exp = Const Val | Var Var | Bin Op Exp Exp
>            deriving (Show)

An operation is +, -, *, / or ^

> data Op = Plus | Minus | Times | Div | Power |
>           Eq | Ne | Lt | Le | Gt | Ge | And | Or | Not
>           deriving (Eq, Show)

A relational operation is =, /=, <, <=, > or >=

A store is a map from variables to values

> type Store = Map Var Val

To run a program with a given initial store, we just pass the program and
store to exec.

> run :: Prog -> Store -> Store

> run prog input = exec prog input

To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Prog -> Store -> Store

> exec ([],[]) store = store

> exec (var,(stmt : rest)) store =
>       if varsOk (var,(stmt:rest))
>       then exec (var,rest) (exec' (var,stmt) store)
>       else error ("variables are not ok")

Execute a single statement, according to its semantics

> exec' :: (SymTab, Stmt) -> Store -> Store

> exec' (vars,Skip) store = store

> exec' (vars,(Asgn var exp)) store =
>       setVal var (eval exp store) store

> exec' (vars, (If cond thenPart elsePart)) store =
>       if b
>       then exec (vars,thenPart) store
>       else exec (vars,elsePart) store
>       where (Bool b) = eval cond store

> exec' (vars,(Do cond body)) store =
>       if not b then store
>       else exec' (vars,(Do cond body))
>                  (exec (vars,body) store)
>       where (Bool b) = eval cond store

Evaluate an expression, according to its type

> eval :: Exp -> Store -> Val

> eval (Const n) _ = n

> eval (Var v) s | hasKey v s = getVal v s
>      | otherwise = error ("Undefined variable " ++ [v])

> eval (Bin op x y) s = apply op (eval x s) (eval y s)

> varsOk :: Prog -> Bool
> varsOk (_, []) = True
> varsOk (vars, stmt : stmts) =
>    varsOk' (vars, stmt) && varsOk (vars, stmts)

> varsOk' :: (SymTab, Stmt) -> Bool
> varsOk' (vars, Skip) = True
> varsOk' (vars, Asgn v e) = hasKey v vars &&
>           varsOk'' (vars, e)

> varsOk'' :: (SymTab, Exp) -> Bool
> varsOk'' (vars, Const v) = True
> varsOk'' (vars, Var v) 
>   | hasKey v vars = True
>   | otherwise = error ("Undeclared variable" ++ (show v))

> expType :: Exp -> SymTab -> Type
> expType (Const _) _ = IntT
> expType (Var v) t
>   | hasKey v t = getVal v t
>   | otherwise = error ("Undeclared variable " ++ (show v))
> expType (Bin op x y) t =
>    opType op (expType x t) (expType y t)

> opType :: Op -> Type -> Type -> Type
> opType Plus IntT IntT = IntT
> opType Minus IntT IntT = IntT
> opType Times IntT IntT = IntT
> opType Div IntT IntT = IntT
> opType Power IntT IntT = IntT
> opType Eq IntT IntT = BoolT
> opType Ne IntT IntT = BoolT
> opType Lt IntT IntT = BoolT
> opType Le IntT IntT = BoolT
> opType Gt IntT IntT = BoolT
> opType Ge IntT IntT = BoolT
> opType op t1 t2 = error "Illegal application"

Apply an arithmetic operator

> apply :: Op -> Val -> Val -> Val
> apply Plus (Int x) (Int y) = Int (x + y)
> apply Minus (Int x) (Int y) = Int (x - y)
> apply Times (Int x) (Int y) = Int (x * y)
> apply Div (Int x) (Int y) = Int (x `div` y)
> apply Eq (Int x) (Int y) = Bool (x == y)
> apply Ne (Int x) (Int y) = Bool (x /= y)
> apply Lt (Int x) (Int y) = Bool (x < y)
> apply Le (Int x) (Int y) = Bool (x <= y)
> apply Gt (Int x) (Int y) = Bool (x > y)
> apply Ge (Int x) (Int y) = Bool (x >= y)
> apply op x y =
>   error ("Illegal arguments to " ++ (show op) ++ "x: " ++ (show x) ++ "y: " ++ (show y))

% > evalc :: Cond -> Store -> Bool
% > evalc (Cond rel x y) s = applyc rel (eval x s) (eval y s)

applyc is similar to apply, but applies relations:

Some sample expressions

> e0 = Const (Bool True)
> e1 = Const (Int 1)
> e2 = Const (Int 2)
> e3 = Var 'x'
> e4 = Bin Plus e3 e1
> e5 = Bin Plus (Var 'i') e1

Some sample stores

> s0 = [('x',3)]
> s1 = []
> s2 = [('x',(Int 1))]
> s3 = [('x',(Int 1)), ('y',(Int 2))]
> s4 = [('x',(Int 9)), ('y',(Int 3)),('z',(Int 5))]

Some sample programs

> p1 = [Skip]
> p2 = [Skip, Skip]
> p3 = [Asgn 'x' e1]
> p4 = [Asgn 'x' (Var 'x')]
> p5 = [Asgn 'x' (Var 'y')]
> p6 = [Asgn 'x' (Bin Plus (Var 'x') (Const (Int 1)))]

% > p7 = [Asgn 'x' e1, Asgn 'y' e2,
% >   (If (Cond Eq (Var 'x') (Var 'y')) [Asgn 'z' e1] [Asgn 'z' e2])]
% > p8 = [Asgn 'i' e1, Asgn 's' e0,
% >   (Do (Cond Lt (Var 'i') (Var 'n')) 
% >       [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5])]
