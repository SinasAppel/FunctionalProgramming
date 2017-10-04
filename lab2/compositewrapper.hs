primes :: [Integer]
primes  =  sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  p : sieve [x | x <- xs, x `mod` p /= 0]


-- Insert your own code here.
composites :: [(Integer,[Integer])]

-- Do not change the following wrapper code
wrapper :: String -> [(Integer,[Integer])]
wrapper input = take (read input::Int) composites

main =  print . wrapper =<< getLine
