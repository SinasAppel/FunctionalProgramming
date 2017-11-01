import Control.Monad

numberOfPalindromicComposites :: Integer -> Integer
-- Insert your own code here.
numberOfPalindromicComposites n = fromIntegral (length(takeWhile (<=n) palindromicComposites))


palindromicComposites :: [Integer]
palindromicComposites = [ i | i <- palindromes, isComposite i]

isComposite :: Integer -> Bool
isComposite x = primeCheck (x `div` lowestPrimeFactor x)

primeCheck :: Integer -> Bool
primeCheck x 
  | x == 1 = False
  | otherwise = x == (lowestPrimeFactor x)

lowestPrimeFactor :: Integer -> Integer
lowestPrimeFactor n = head (primeFactors n)

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n = factors n == [1, n]

primeFactors :: Integer -> [Integer]
primeFactors n = filter isPrime (factors n)

primes :: [Integer]
primes  =  sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  p : sieve [x | x <- xs, x `mod` p /= 0]
    
cutOf :: String -> String
cutOf (x:xs) = xs
    
reverse2 :: String -> Bool -> String
reverse2 x b
  | b = reverse x
  | otherwise = cutOf (reverse x)
  
div2 :: Int -> Int
div2 x 
  | x `mod` 2 == 0 = (x `div` 2)
  | otherwise = ((x `div` 2) + 1)
  
makePalindrome :: Int -> String -> String
makePalindrome x ys = ys ++ (reverse2 ys (x `mod` 2 == 0))

palindromeStr2 :: Int -> [String]
palindromeStr2 i = map (makePalindrome i) (replicateM (div2 i) "0123456789")

palindromeStr :: [String]
palindromeStr = foldr (++) [] [palindromeStr2 x | x <- [1..]]

toInteger2 :: String -> Integer
toInteger2 s = read s :: Integer

filterZero :: [String] -> [String]
filterZero ((x:xs) : ys)
  | x == '0' = filterZero ys
  | otherwise = (x:xs) : filterZero ys

palindromes :: [Integer]
palindromes = dropWhile (<=1) $ map toInteger2 (filterZero palindromeStr)

-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = numberOfPalindromicComposites (read input::Integer)

main =  print . wrapper =<< getLine

