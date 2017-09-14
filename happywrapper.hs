{- Insert here your own code. The type of countHappyNumbers must be:

countHappyNumbers :: Integer -> Integer -> Int

-}

sumSquare :: Integer -> Integer
sumSquare x
  | x<10        = (x^2)
  | otherwise   = (sumSquare (x `quot` 10) + (x `mod` 10)^2)

isHappyNumber :: Integer -> [Integer] -> Bool
isHappyNumber x y
  | x==1          = True
  | elem x y      = False
  | otherwise     = isHappyNumber (sumSquare x) (y ++ [x])

countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b = length ([ x | x <- [a .. b], (isHappyNumber x [])])

wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Integer) (read b::Integer)

main =  print . wrapper . words =<< getLine
