smallestMultiple :: Integer -> Integer
-- Insert your own code here.
smallestMultiple n = foldr lcm 1 [1..n]


-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = smallestMultiple (read input::Integer)

main =  print . wrapper =<< getLine
