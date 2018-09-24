-- ListSplitter module
-- Zachary Baklund, September 2018
--

module ListSplitter
where

-- exporting the function (splitListAt n xs), which splits
-- a list into two lists, the first one with n elements.
--
splitListAt :: Int -> [Int] -> ([Int], [Int])
splitListAt 0 xs = ([], xs)
splitListAt _ [] = ([], [])
splitListAt n (x:xs)
  | n < 0 = ([], (x:xs))
  | otherwise = ((x:y), z)
  where (y, z) = splitListAt (n - 1) xs