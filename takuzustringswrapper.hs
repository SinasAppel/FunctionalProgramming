import Data.List

takuzuStrings :: Integer-> [String]
takuzuStrings a = sort (filterStrings (getStrings a []))

{-- Generate the takuzu strings.
	Strings with more than 2 concecutive 0's or 1's are automatically filtered out
--}
getStrings :: Integer -> [String] -> [String]
getStrings 0 xs = xs
getStrings a [] = getStrings (a-1) ["0"] ++ getStrings (a-1) ["1"]
getStrings a (x@(b:c:ys):xs)
 | b == '1' && c == '1' = getStrings (a-1) ([['0'] ++ x] ++ xs)
 | b == '0' && c == '0' = getStrings (a-1) ([['1'] ++ x] ++ xs)
 | otherwise = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)
getStrings a (x:xs) = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)

{-- Filters the generated strings to make sure that all the strings left are legal --}
filterStrings :: [String] -> [String]
filterStrings xs = filter (\x -> isAllowedTakuzuString x) xs

{-- Bool function that checks if a takuzu string is legal or not --}
isAllowedTakuzuString :: String -> Bool
isAllowedTakuzuString xs = if x == y then True else False where
 x = elemNum '0' xs
 y = elemNum '1' xs

{-- returns the number of times a char is in the string -}
elemNum :: Char -> String -> Integer
elemNum y [] = 0
elemNum y (x:xs) = if x == y then 1 + elemNum y xs else elemNum y xs


wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
