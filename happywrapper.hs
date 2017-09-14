{- Insert here your own code. The type of countHappyNumbers must be:

countHappyNumbers :: Integer -> Integer -> Int

-}


{-
  Recursively "shaves of" the last digit of the number, squares it
  and adds it to the result of a recursive call to the same function,
  with the shortened number as its argument.
-}
sumSquare :: Integer -> Integer
sumSquare x
  | x<10        = (x^2)
  | otherwise   = (sumSquare (x `quot` 10) + (x `mod` 10)^2)
  
  
{-
  Checks if a number is a happy number, using recursive calls for the
  operations. It keeps a list of previously found numbers, because when 
  come to the same number twice, it's a cycle, therefore the result 
  should be false.
  The other base case is obviously when we reach 1.
-}
isHappyNumber :: Integer -> [Integer] -> Bool
isHappyNumber x y
  | x==1          = True
  | elem x y      = False
  | otherwise     = isHappyNumber (sumSquare x) (y ++ [x])
  
{-
  Uses set comprehension to find the amount of numbers in the given
  range that satisfy isHappyNumber.
-}
countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b = length ([ x | x <- [a .. b], (isHappyNumber x [])])

{- End of our code. -}

wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Integer) (read b::Integer)

main =  print . wrapper . words =<< getLine
