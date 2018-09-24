-- Matches module
-- Zachary Baklund, September 2018
--
-- This is a simple example of a module definition

module Matches
where

-- returns all occurrences of the first argument in its second argument.
--  So for example, matches 10 [1,10,2,10,3,10,4] returns [10,10,10], and matches 10 [11,14,17,21] returns [].
--
matches :: Int -> [Int] -> [Int]
matches val [] = []
matches val (x : xs)
  | val == x = x:(matches val xs)
  | otherwise = matches val xs
--matches val = filter (val ==)