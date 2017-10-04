import Data.List

fibcats :: [Integer]
-- Insert your own code here.
--fibcats = [i | i <- [0..] , i `elem` (takeWhile (<=i) fib) || i `elem` (takeWhile (<=i) cat)]
fibcats = mergeLists fib cat

mergeLists :: [Integer] -> [Integer] -> [Integer]
{-mergeLists (x:xs) (y:ys)
  | x<y = [x] ++ mergeLists (dropWhile (<=x) (x:xs)) (dropWhile (<=x) (y:ys))
  | y>x = [y] ++ mergeLists (dropWhile (<=y) (x:xs)) (dropWhile (<=y) (y:ys))
  | otherwise = [x] ++ mergeLists (dropWhile (<=x) (x:xs)) (dropWhile (<=x) (y:ys))-}
  
mergeLists xs [] = xs
mergeLists [] xs = xs
mergeLists (x:xs) (y:ys)
 | x == y = x : mergeLists xs ys
 | x < y = x : mergeLists xs (y:ys)
 | otherwise = y : mergeLists (x:xs) ys

fib :: [Integer]
fib = (0:1: zipWith (+) fib (tail fib))

cat :: [Integer]
cat = cat2 0

cat2 :: Int -> [Integer]
cat2 x
  | x == 0 = [1] ++ cat2 (x+1)
  | otherwise = [(getCatNum (take x (cat2 (0)))) ] ++ cat2 (x+1)

getCatNum :: [Integer] -> Integer
getCatNum xs = sum ( zipWith (*) (xs) (reverse xs))

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) fibcats

main =  print . wrapper =<< getLine
