-- NeighborDups module
-- Zachary Baklund, September 2018
--

module NeighborDups
where

-- exporting a function noNeighborDups, which returns a list
-- with consecutive duplicates removed
--
matches :: Int -> [Int] -> [Int]
matches val [] = []
matches val (x : xs)
  | val == x = x:(matches val xs)
  | otherwise = matches val xs
--matches val = filter (val ==)