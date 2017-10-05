primes :: [Integer]
-- Insert your own code here.
primes = 2 : (inc 1 (mult 2 [ i | i <- [1..] , not (i == head(dropWhile (<i) sieve) )]))

sieve :: [Integer]
sieve = merge sieve2

sieve2 :: [[Integer]]
sieve2 = [ [i + j + 2*i*j | i <- [1..j] ] | j <- [1..]  ]

mult x = map (x *)

inc x = map (x +)

merge :: [[Integer]] -> [Integer]
merge [] = []
merge ([]:xss) = merge xss
--merge ((x:xs):xss) = x : mergeLists xs (merge xss)
merge xs = foldr mergeLists [] xs

mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists xs [] = xs
mergeLists [] xs = xs
mergeLists (x:xs) (y:ys)
  | x == y = x : mergeLists xs ys
  | x < y = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys


-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine
