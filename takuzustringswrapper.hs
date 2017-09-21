takuzuStrings :: Integer-> [String]
takuzuStrings a = getStrings a []

getOne :: Integer -> [String] -> [String]
getOne 0 xs = xs
getOne a [] = getOne (a-1) ["1"] ++ getZero (a-1) ["0"]
getOne a (x:xs) = getOne (a-1) ([['1'] ++ x] ++ xs) ++ getZero (a-1) ([['0'] ++ x] ++ xs)

getStrings :: Integer -> [String] -> [String]
getStrings 0 xs = xs
getStrings a [] = getStrings (a-1) ["0"] ++ getStrings (a-1) ["1"]
getStrings a (x:xs) = getStrings (a-1) ([['0'] ++ x] ++ xs) ++ getStrings (a-1) ([['1'] ++ x] ++ xs)


wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
