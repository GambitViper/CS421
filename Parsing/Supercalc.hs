module Supercalc where

import Data.Char
import MonadicParser
           
--
-- Write a parser for expressions according to the grammar
-- 
-- Exprbool ::= Term ( OR Term )*
-- Term ::= Factorbool ( AND Factorbool )*
-- Factorbool ::= Bool | NOT Factorbool | '(' Exprbool ')'
-- Bool ::= false | true | Relation
-- Relation ::= Expr relation Expr
--
-- Expr ::= Factor ( ( '+' | '-' ) Factor )*
-- Factor ::= Atomic ( ( '*' | '/' ) Atomic )*
-- Atomic ::= Digit+ | '(' Expr ')'
--
           
number :: Parser Char Int
number = some (satisfy isDigit) `using` read

boolt :: Parser Char Bool
boolt = literal 't' `thn` \_ ->
        literal 'r' `thn` \_ ->
        literal 'u' `thn` \_ ->
        literal 'e' `thn` \res ->
        accept True

boolf :: Parser Char Bool
boolf = literal 'f' `thn` \_ ->
        literal 'a' `thn` \_ ->
        literal 'l' `thn` \_ ->
        literal 's' `thn` \_ ->
        literal 'e' `thn` \res ->
        accept False

boolean :: Parser Char Bool
boolean = boolt `alt` boolf

booleval :: Parser Char Bool
booleval = boolean `alt` relation

-- Logical operators
andop = literal 'a' `thn`   \_ ->
        literal 'n' `thn`   \_ ->
        literal 'd' `using` \_ -> (&&)
orop  = literal 'o' `thn`   \_ ->
        literal 'r' `using` \_ -> (||)
notop = literal 'n' `thn`   \_ ->
        literal 'o' `thn`   \_ ->
        literal 't' `using` \_ -> (not)

notSuffix :: Parser Char Bool
notSuffix = notop `thn` \op ->
            factorbool `thn` \f ->
            accept(op f)

factorbool :: Parser Char Bool
factorbool = booleval `alt` notSuffix `alt` parenedbool

-- Relational operators
-- Binary operators <, >, <=, >=, == and !=, which receive integers and return a boolean value
lt  = literal '<' `using` \_ -> (<)
gt  = literal '>' `using` \_ -> (>)
lte = literal '<' `thn`   \_ ->
      literal '=' `using` \_ -> (<=)
gte = literal '>' `thn`   \_ ->
      literal '=' `using` \_ -> (>=)
eq  = literal '=' `thn`   \_ ->
      literal '=' `using` \_ -> (==)
neq = literal '!' `thn`   \_ ->
      literal '=' `using` \_ -> (/=)
           
parened :: Parser Char Int
parened = literal '(' `thn` \_ ->
          expr        `thn` \res ->
          literal ')' `thn` \_ ->
          accept res

parenedbool :: Parser Char Bool
parenedbool = literal '(' `thn` \_ ->
              exprBool    `thn` \res ->
              literal ')' `thn` \_ ->
              accept res
           
atomic :: Parser Char Int
atomic = number `alt` parened
           
addition       = literal '+' `using` \_ -> (+)
subtraction    = literal '-' `using` \_ -> (-)
multiplication = literal '*' `using` \_ -> (*)
division       = literal '/' `using` \_ -> (div)
           
factorSuffix :: Parser Char (Int -> Int)
factorSuffix = (multiplication `alt` division) `thn` \op ->
               atomic                          `thn` \val ->
               accept (\z -> op z val)
          
factor :: Parser Char Int
factor = atomic `thn` \num1 ->
         many factorSuffix `thn` \suffixes ->
         accept (foldl (\n f -> f n) num1 suffixes)

termSuffix :: Parser Char (Bool -> Bool)
termSuffix = andop `thn`  \op ->
             factorbool `thn` \val ->
             accept (\z -> op z val)

term :: Parser Char Bool
term = factorbool `thn` \num1 ->
       many termSuffix `thn` \suffixes ->
       accept (foldl (\n f -> f n) num1 suffixes)

exprBoolSuffix :: Parser Char (Bool -> Bool)
exprBoolSuffix = orop `thn` \op ->
                 term `thn` \val ->
                 accept (\z -> op z val)

exprBool :: Parser Char Bool
exprBool = term `thn` \num1 ->
           many exprBoolSuffix `thn` \suffixes ->
           accept (foldl (\n f -> f n) num1 suffixes)
           
exprSuffix :: Parser Char (Int -> Int)
exprSuffix = (addition `alt` subtraction) `thn` \op ->
             factor                       `thn` \val ->
             accept (\z -> op z val)
           
expr :: Parser Char Int
expr = factor `thn` \num1 ->
       many exprSuffix `thn` \suffixes ->
       accept (foldl (\n f -> f n) num1 suffixes)

relation :: Parser Char Bool
relation = expr `thn` \val1 ->
           (lt `alt` gt `alt` lte `alt` gte `alt` eq `alt` neq) `thn` \op ->
           expr `thn` \val2 ->
           accept (op val1 val2)