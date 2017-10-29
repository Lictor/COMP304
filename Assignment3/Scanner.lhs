A scanner for while programs

Author: Lindsay Groves, VUW, 2017.

Symbols used are: identifiers, numbers, +, -, *, /, ^,
=, !=, <, >, <=, >=, ;, (, ).

This scanner skips white space, and just returns these as strings, rather
than as tokens.

> module Scanner where

> import StringSym

Read a string and turn it into a list of symbols

> scan :: String -> [Sym]

> scan [] = []

> scan ('<' : '=' : xs) = "<=" : scan xs
> scan ('>' : '=' : xs) = ">=" : scan xs
> scan ('!' : '=' : xs) = "!=" : scan xs
> scan (':' : '=' : xs) = ":=" : scan xs

> scan (x : xs)
>    | isLetter x = scanIdFirst (x : xs)
>    | isDigit x = scanNumFirst (x : xs)
>    | elem x ['+', '-', '*', '/', '^', '=', '<', '>', ';', '(', ')'] = 
>           [x] : scan xs
>    | elem x [' ', '\t', '\n'] = scan xs

Read a string and turn it into a list of symbols, given that the first
character is a letter (or digit on recursive calls), so the first symol
will be an id.

> scanIdFirst :: String -> [Sym]

> scanIdFirst [x] = [[x]]

> scanIdFirst (x:y:z)
>    | isLetter y || isDigit y = (x:s):t 
>    | otherwise = [x] : scan (y:z)
>    where (s:t) = scanIdFirst (y:z)

Read a string and turn it into a list of symbols, given that the first
character is a digit, so the first symol will be a number.

> scanNumFirst :: String -> [Sym]

> scanNumFirst [x] = [[x]]

> scanNumFirst (x:y:z)
>    | isDigit y = (x:s):t 
>    | otherwise = [x] : scan (y:z)
>    where (s:t) = scanNumFirst (y:z)

> isLetter :: Char -> Bool

> isLetter x = 'a' <= x && x <= 'z' ||'A' <= x && x <= 'Z'

> isDigit :: Char -> Bool

> isDigit x = '0' <= x && x <= '9'
