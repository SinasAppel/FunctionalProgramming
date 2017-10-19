sumsg :: Integer -> Integer
-- Insert your own code here.
sumsg x = sum [ i | i <- (map sg [1..x])]

fac :: Integer -> Integer
fac x 
  | x == 0 = 1
  | otherwise = x * fac (x-1)

f :: Integer -> Integer
f x
  | x < 10 = fac x
  | otherwise = (fac (x `mod` 10)) + (f (x `div` 10))
  
sf :: Integer -> Integer
sf x = sumDigits (f x)

sumDigits :: Integer -> Integer
sumDigits x 
  | x < 10 = x
  | otherwise = (x `mod` 10) + (sumDigits (x `div` 10))
  
g :: Integer -> Integer
g x = head [ i | i <- [1..], (sf i) == x]

sg :: Integer -> Integer
sg x = sumDigits (g x)

-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = sumsg (read input::Integer)

main =  print . wrapper =<< getLine
