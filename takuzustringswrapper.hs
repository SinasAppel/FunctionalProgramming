takuzuStrings :: Integer-> [String]
takuzuStrings a = getStrings a []

getStrings :: Integer -> [String] -> [String]
getStrings 0 xs = xs
getStrings a [] = getStrings (a-1) ["0"] ++ getStrings (a-1) ["1"]
getStrings a (x@(b:c:ys):xs)
 | b == '1' && c == '1' = getStrings (a-1) ([['0'] ++ x] ++ xs)
 | b == '0' && c == '0' = getStrings (a-1) ([['1'] ++ x] ++ xs)
 | otherwise = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)
getStrings a (x:xs) = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)



wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
