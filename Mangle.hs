-- Mangle module
-- Zachary Baklund, September 2018
--
-- This is a simple example of a module definition

module Mangle
where

-- which removes the first letter of a word and attaches it at the end. If the string is empty, 
-- mangle should simply return an empty string:
--
-- mangle "Hello" ==> "elloH"
-- mangle "I" ==> "I"
-- mangle "" ==> ""
--
mangle :: String -> String
mangle [] = []
mangle (x:xs) = xs ++ [x]