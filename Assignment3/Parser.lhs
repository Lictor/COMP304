LL(1) parser for while programs

Author: Lindsay Groves, VUW, 2017.

To use the parser, call parseProg p, where p is a string containing the
program to be parsed (see exampes at the end).  Returns a list of parse trees
(because a program is a list of statements) if the input is aprsed
successfully; otherwise an error message showing what was parsed and the
remaining input.

Uses Scanner to turn a string into a list of symbols.

Grammar is:

Program -> Stmt+

Stmt -> "skip" | Var ":=" Exp | "if" Cond "then" Stmt+ "else" Stmt+ "fi" |
     	"while" Cond "do" Stmt+ "od"

Exp -> Number | Id | Exp AOp Exp | "(" Exp ")"

AOp -> "+" | "-" | "*" | "/" | "^"

Cond -> Exp ROp Exp

ROp -> "=" | "!=" | "<" | "<= | ">" | ">="

Actually, Stmt+ is treated as StmtList, where:

StmtList -> Stmt RestStmtList
RestStmtList -> ";" StmtList | <empty>

and, for simplicity, Exp is treated as:

Exp -> Term RestExp
Term -> Number | Id | "(" Exp ")"
RestExp -> AOp Exp | <empty>

This treats all operators as right associative, with the same precedence, but
is ok for the purposes of doing program checking.

> module WhileParser where

> import StringSym

> import Scanner

> type Var = String

We use a single tree for statements and expressions

> data Tree = Seq [Tree] |
> 	    Skip |
>      	    Asgn Sym Tree |
> 	    If Tree Tree Tree |
> 	    Do Tree Tree |
> 	    Const Int |
> 	    Var Sym |
> 	    Bin Sym Tree Tree |
> 	    Cond Sym Tree Tree |
>	    Sym Sym |
> 	    Error String
>      	    deriving (Show)

We define a parse function for each nonterminal which attempts to parse an
instance of that nonterminal as a prefix of the input, returning a Tree (or
list of Trees in the case of list parsers) representing the construct parsed
along with the remaining input.

parseProg takes a string as its input, and calls the scanner on it, turning it
into a list of symbols.  The others take a list of symbols as input.  Symbols
are just strings - the scanner doesn't identify what kinds of strings they
are.

When the rule for the nonterminal N has multiple options, the parse function
tests for each symbol in first(N).  If N can produce the empty string, the
parse function chooses this option when the next symbol is in follow(N).  The
grammar is LL(1), which means that these decisions will be the correct ones.

Where clauses are used extensively to call parser functions to recognise each
of the symnbols in the rihgt hand side of a grammar rule, passing the
remaining input returned from one as the input to the next, and giving names
to the partial trees returned by the variable parse fuctions, which are then
colleted to build the tree returned.

parseSym is used to parse terminals.

Error handling is quite simple.  An Error tree is returned when the parse
fails.  At the end we just check whether the tree contains an error node.
This avoids having to check the result of every parser call.  It also means we
can see where the error was detecteed.

Parse a program, i.e. a list of statements.

> parseProg :: String -> [Tree]

> parseProg s
>  | xx == [] = t
>  | otherwise = error ("Extra input! " ++ (show r))
>  where r@(t, xx) = (parseStmtList . scan) s

(Could handle this error case more nicely!)

Parse a list of statements

> parseStmtList :: [Sym] -> ([Tree], [Sym])

> parseStmtList [] = ([], [])

> parseStmtList xs = ((tree : trees), zs)
>   where (tree, ys) = parseStmt xs 
>   	  (trees, zs) = parseRestStmtList ys

Parse RestStmtList.  Note that follow(RestStmtList) = {"else", "fi", "od"}.

> parseRestStmtList :: [Sym] -> ([Tree], [Sym])

> parseRestStmtList [] = ([], [])

> parseRestStmtList xx@(x : xs)
>   | elem x ["else", "fi", "od"] = ([], xx)
>   | x == ";" = parseStmtList xs
>   | otherwise = ([Error ((showSym x) ++ " found, expecting ;")], xs)

Parse a statement - skip, assignment, if or do.

> parseStmt :: [Sym] -> (Tree, [Sym])

> parseStmt [] = (Error "unexpected end of input", [])

> parseStmt xx@(x:xs)
>   | x == "skip"  = parseSkip xx
>   | x == "if"    = parseIf xx
>   | x == "while" = parseDo xx
>   | isId x 	   = parseAsgn xx
>   | otherwise    = (Error ("statement can't start with " ++ (showSym x)), xx)

Note that order is important here, so we check for skip, if and while before
trying to parse an assignment.  Otherwise we would treat skip, if or while as
being the start of an assignment!

Parse a skip statement.

> parseSkip :: [Sym] -> (Tree, [Sym])

> parseSkip ("skip" : xs) = (Skip, xs)

Parse an if statement.

> parseIf :: [Sym] -> (Tree, [Sym])

> parseIf ("if" : us) = 
>    	if noError trees then (If condTree (Seq thenPtTree) (Seq elsePtTree), rs)
> 	else ((firstError trees), rs)
>   where (condTree, vs) = parseCond us
>         (thenTree, ws) = parseSym "then" vs
> 	  (thenPtTree, xs) = parseStmtList ws
> 	  (elseTree, ys) = parseSym "else" xs
> 	  (elsePtTree, zs) = parseStmtList ys
> 	  (fiTree, rs) = parseSym "fi" zs
>   	  trees = [condTree, thenTree, Seq thenPtTree, elseTree, Seq elsePtTree, fiTree]

Parse a while/do statement.

> parseDo :: [Sym] -> (Tree, [Sym])

> parseDo ("while" : us) = 
> 	if noError trees then (Do condTree (Seq bodyTree), ys)
> 	else ((firstError trees), ys)
>   where (condTree, vs) = parseCond us
>         (doTree, ws) = parseSym "do" vs
> 	  (bodyTree, xs) = parseStmtList ws
> 	  (odTree, ys) = parseSym "od" xs
>	  trees = [condTree, doTree, Seq bodyTree, odTree]
   	  
Parse an assignment statement.  We know x is an id.

> parseAsgn :: [Sym] -> (Tree, [Sym])

> parseAsgn (x : xs) =
> 	if noError trees then (Asgn x expTree, zs)
> 	else ((firstError trees), zs)
>   where (asgnTree, ys) = parseSym ":=" xs
>   	  ( expTree, zs) = parseExp ys
>	  trees = [asgnTree, expTree]
   	  
Parse an expression.

> parseExp :: [Sym] -> (Tree, [Sym])

> parseExp xx =
> 	if noError trees then (mkExpTree trees, zs)
>       else ((firstError trees), zs)
>   where (termTree, ys) = parseTerm xx
> 	  (restTree, zs) = parseRestExp ys
>   	  trees = (termTree : restTree)

Parse RestExp.

Note that follow(Exp) = {")", "then", "else", "do", ";", "fi", "od"} U RelOps
where RelOps is the set of relational operators.

> parseRestExp :: [Sym] -> ([Tree], [Sym])

> parseRestExp [] = ([], [])

> parseRestExp xx@(x : xs)
>   | elem x [")", "then", "else", "do", ";", "fi", "od"] = ([], xx)
>   | elem x ["=", "!=", "<", "<=", ">", ">="] = ([], xx)
>   | otherwise = ([op, expTree], zs)
>   where (op, ys) = parseOp xx
>   	  (expTree, zs) = parseExp ys

Parse a term - number, variable or bracketted expression.

> parseTerm :: [Sym] -> (Tree, [Sym])

> parseTerm xx@(x : xs)
>   | isNum x = ((Const (toNum x)), xs)
>   | isId x = ((Var x), xs)
>   | x == "(" = parseBrackExp xx
>   | otherwise = (Error ("expression can't start with " ++ (showSym x)), xx)

Parse a bracketted expression.

> parseBrackExp :: [Sym] -> (Tree, [Sym])

> parseBrackExp ("(":xs) =
>     if noError trees then (expTree, zs)
>     else ((firstError trees), zs)
>   where (expTree, ys) = parseExp xs
> 	  (rbTree, zs) = parseSym ")" ys
>    	  trees = [expTree, rbTree]

Make an expression tree - basically turns a list into a tree.

> mkExpTree :: [Tree] -> Tree

> mkExpTree [t] = t

> mkExpTree [t, Sym op, t'] = Bin op t t'

Parse a condition.

> parseCond :: [Sym] -> (Tree, [Sym])

> parseCond xs =
> 	if noError trees then (Cond op leftArgTree rightArgTree, ws)
> 	else ((firstError trees), ys)
>   where (leftArgTree, ys) = parseExp xs
>         (opTree, zs) = parseRelOp ys
> 	  (rightArgTree, ws) = parseExp zs
>	  trees = [leftArgTree, opTree, rightArgTree]
>	  Sym op = opTree

Parse a terminal symbol.

> parseSym :: Sym -> [Sym] -> (Tree, [Sym])

> parseSym sym (x : xs)
> 	 | x == sym = (Sym sym, xs)
> 	 | x /= sym = (Error ((showSym x) ++ " found, expecting " ++ (showSym sym)), (x:xs))

Parse an arithmetic operator

> parseOp :: [Sym] -> (Tree, [Sym])

> parseOp xx@(x : xs)
>     | elem x ["+", "-", "*", "/", "^"] = (Sym x, xs)
>     | otherwise = (Error ("operator expected, parsing " ++ (showSym x)), xx)

Parse a relational operator

> parseRelOp :: [Sym] -> (Tree, [Sym])

> parseRelOp xx@(x : xs)
>     | elem x ["=", "!=", "<", ">", "<=", ">-"] = (Sym x, xs)
>     | otherwise = (Error ("operator expected, parsing " ++ (showSym x)), xx)

> isError :: Tree -> Bool

> isError (Error _) = True
> isError _ = False

> toNum :: String -> Int

> toNum [d] = toNum' d
> toNum (d:ds) = (toNum' d) * 10^(length ds) + (toNum ds)

> toNum' x = case x of
>   '0' -> 0; '1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4
>   '5' -> 5; '6' -> 6; '7' -> 7; '8' -> 8; '9' -> 9

> noError :: [Tree] -> Bool

> noError [] = True

> noError (t : ts) = not (isError t) && noError ts

> firstError :: [Tree] -> Tree

> firstError (t : ts) 
>    | isError t = t
>    | otherwise = firstError ts

> isId (x : xs) = isLetter x && isAlphaNum xs

> isAlphaNum [] = True

> isAlphaNum (x : xs) = (isLetter x || isDigit x) && isAlphaNum xs

> isNum (x : xs) = isDigit x && (xs == [] || isNum xs)

Some sample programs

> p0 = ""
> p1 = "skip"
> p2 = "x := 1"
> p3 = "x := y"
> p4 = "x := (1 - y)"
> p5 = "x := 1+(1 - y)"
> p6 = "x := 1+(1 - y)*2"
> p7 = "x := (1 - y) + (y - 10)"
> p8 = "x := 1; y := x+y*2-1"
> p9 = "while x > 0 do skip od"
> p10 = "while x > 0 do x:=x-1 od"
> p11 = "while x > 0 do x:=x-1 od; y := x"
> p12 = "x := 5; while x > 0 do x:=x-1 od; y := x"
> p13 = "while x > 0 do while y > 0 do x:=x-1 od od"
> p14 = "while x > 0 do x:=x-1; y := y+x-1 od"
> p15 = "x := 5; while x > 0 do x:=x-1; y := y+x-1 od; y := x"
> p16 = "if x = 0 then x:=x-1 else x := x + 1 fi"

Some sample expresions

> e1 = "1"
> e2 = "x"
> e3 = "x+1"
> e4 = "x+y*2-1"
> e5 = "(1 - y)"
> e6 = "1+(1 - y)"
> e7 = "1+(1 - y)*2"
> e8 = "(1 - y) + (y - 10)"

Some sample conditions

> c1 = "x=0"
> c2 = "x+y > 2*z-1"

Some incorrect programs (error handling is very primitive!!)

> q1 = "x = 1"
> q2 = "+ := 1"
> q3 = "x := 1; y := x+y*2-1)"
> q4 = "x := 1 od"
