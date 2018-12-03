module Supercalc where

import MonadicParser
import Data.Char

digit :: Parser Char Char
digit = satisfy isDigit

number :: Parser Char Int
number = some digit `using` read

atomic = number `alt` (literal '(' `thn` \_ ->
                       expr        `thn` \val ->
                       literal ')' `thn` \_ -> accept val)

multiplier = literal '*' `using` \_ -> (*)
divider    = literal '/' `using` \_ -> div
adder      = literal '+' `using` \_ -> (+)
subtracter = literal '-' `using` \_ -> (-)

factorSuffix :: Parser Char (Int -> Int -> Int, Int)
factorSuffix = (multiplier `alt` divider) `thn` \op ->
                atomic                    `thn` \num ->
                accept (op, num)

expr :: Parser Char Int
expr = factor          `thn` \num1 ->
       many exprSuffix `thn` \opvals ->
       accept (let foldingFn :: Int -> (Int -> Int -> Int, Int) -> Int
                   foldingFn x yz = case yz of
                                    (op, x2) -> op x x2
               in foldl foldingFn num1 opvals