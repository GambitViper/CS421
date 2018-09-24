-- NeighborDups module
-- Zachary Baklund, September 2018
--

module NeighborDups
where

-- exporting a function noNeighborDups, which returns a list
-- with consecutive duplicates removed
--
noNeighborDups :: [Int] -> [Int]
noNeighborDups [] = []
noNeighborDups (x : xs)
  | x == head xs = drop 1 xs
  | otherwise = x : noNeighborDups xs