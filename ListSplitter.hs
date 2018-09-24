-- ListSplitter module
-- Zachary Baklund, September 2018
--

module ListSplitter
where

-- exporting the function (splitListAt n xs), which splits
-- a list into two lists, the first one with n elements.
--
matches :: Int -> [Int] -> [Int]
matches val [] = []
matches val (x : xs)
  | val == x = x:(matches val xs)
  | otherwise = matches val xs
--matches val = filter (val ==)