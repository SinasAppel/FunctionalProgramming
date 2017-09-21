import Data.List

takuzuStrings :: Integer-> [String]
takuzuStrings a = sort (filterStrings (getStrings a []))

getStrings :: Integer -> [String] -> [String]
getStrings 0 xs = xs
getStrings a [] = getStrings (a-1) ["0"] ++ getStrings (a-1) ["1"]
getStrings a (x@(b:c:ys):xs)
 | b == '1' && c == '1' = getStrings (a-1) ([['0'] ++ x] ++ xs)
 | b == '0' && c == '0' = getStrings (a-1) ([['1'] ++ x] ++ xs)
 | otherwise = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)
getStrings a (x:xs) = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)

filterStrings :: [String] -> [String]
filterStrings [] = []
filterStrings (x:xs) = if isAllowedTakuzuString x then filterStrings xs ++ [x] else filterStrings xs

isAllowedTakuzuString :: String -> Bool
isAllowedTakuzuString xs = if x == y then True else False where
 x = elemNum '0' xs
 y = elemNum '1' xs

elemNum :: Char -> String -> Integer
elemNum y [] = 0
elemNum y (x:xs) = if x == y then 1 + elemNum y xs else elemNum y xs


wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
