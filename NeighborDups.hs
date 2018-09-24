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
noNeighborDups [x] = [x]
noNeighborDups (x : y : xs)
  | x == y = x : noNeighborDups xs 
  | otherwise = x : y : noNeighborDups xs