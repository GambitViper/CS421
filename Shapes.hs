-- Shapes module
-- Zachary Baklund, 2-10-18
--

data Shape = Circle Float 
        | Rectangle Float Float
        | Triangle Float Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle l w) = l * w
area (Triangle a b c) = let p = (perimeter (Triangle a b c)) / 2
                        in sqrt(p * (p - a) * (p - b) * (p - c) )
                          
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle l w) = 2 * (l + w)
perimeter (Triangle a b c) = a + b + c