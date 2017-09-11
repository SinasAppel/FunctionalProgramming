countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b
 | a > b = 0
 | True  = if isHappyNumber a then 1 + countHappyNumbers (a+1) b else countHappyNumbers (a+1) b

{- 
Returns true when a is a happy number
-}
isHappyNumber :: Integer -> Bool
isHappyNumber 1 = True
isHappyNumber 4 = False
isHappyNumber a = isHappyNumber (sumOfSquaredDigits a 0)

{- 
Returns the sum of the squared digits of an int
Default value of b is 0, it is used as a store for the sum
-}
sumOfSquaredDigits :: Integer -> Integer -> Integer
sumOfSquaredDigits a b 
 | (a `div` 10 > 0) = sumOfSquaredDigits (a `div` 10) (b + (a `mod` 10)^2)
 | True             = (a*a + b)


wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Integer) (read b::Integer)

main =  print . wrapper . words =<< getLine
