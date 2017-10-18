multsum :: Integer -> [Integer] -> Integer
-- Insert your own code here.
multsum n xs = sum (takeWhile (<n) (multiples xs) )

mults :: Integer -> [Integer]
mults n = [ n * i | i <- [1..] ]

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys) 
  | x < y = x : (mergeLists xs (y:ys))
  | y < x = y : (mergeLists (x:xs) ys)
  | otherwise = x : (mergeLists xs ys)
  
multiples :: [Integer] -> [Integer]
multiples xs = foldr mergeLists [] (map mults xs) 

-- Do not change the following wrapper code

stringToList :: String -> [Integer]
stringToList xs = map read (words input)::[Integer]
  where input = [if elem x ",[]" then ' ' else x | x <- xs]
  
wrapper :: String -> Integer
wrapper input = multsum (head xs) (tail xs)
  where xs = stringToList input

main =  print . wrapper =<< getLine
