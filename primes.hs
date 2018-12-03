module Primes
where

sieve :: [Int] -> [Int]
sieve (x:xs) = x:sieve(filter(\z->z`mod`x>0)xs)

primes :: [Int]
primes = sieve[2..]