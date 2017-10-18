numberOfPalindromicComposites :: Integer -> Integer
-- Insert your own code here.
numberOfPalindromicComposites n = fromIntegral (length(takeWhile (<=n) palindromicComposites))

isPalindrome :: Integer -> Bool
isPalindrome x = (reverse (show x)) == (show x)

palindromes :: [Integer]
palindromes = [ i | i <- [1..], isPalindrome i]

palindromicComposites :: [Integer]
palindromicComposites = [ i | i <- palindromes, hasTwoPrimeFactors i]

hasTwoPrimeFactors :: Integer -> Bool
hasTwoPrimeFactors x = hasPrimeElement ( [ x `div` i | i <- takeWhile (<=(round(sqrt (fromIntegral x)))) primes, (x `mod` i) == 0])

hasPrimeElement :: [Integer] -> Bool
hasPrimeElement [] = False
hasPrimeElement (x:xs) = x `elem` (takeWhile (<=x) primes)

primes :: [Integer]
primes  =  sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  p : sieve [x | x <- xs, x `mod` p /= 0]

-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = numberOfPalindromicComposites (read input::Integer)

main =  print . wrapper =<< getLine

