{- Insert here your own code. The type of lastDigits must be:

lastDigits  :: Integer -> Int -> [Integer]

-}

lastDigits :: Integer -> Int -> [Integer]
lastDigits x n = getLastDigits (getNumber x) n

getNumber :: Integer -> Integer
getNumber x = sum( [ x^y | y <- [ 0 .. x ] ] )

getLastDigit :: Integer -> Integer
getLastDigit x = x `mod` 10

removeLastDigit :: Integer -> Integer
removeLastDigit x = x `quot` 10

getLastDigits :: Integer -> Int -> [Integer]
getLastDigits x n
  | n == 0 || x == 0 = []
  | otherwise = (getLastDigits (removeLastDigit x) (n-1)) ++ [getLastDigit x]


wrapper :: [String] -> [Integer]
wrapper (a:b:_) = lastDigits (read a::Integer) (read b::Int)

main =  print . wrapper . words =<< getLine
