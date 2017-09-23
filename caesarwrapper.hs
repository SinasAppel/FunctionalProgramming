import Data.Char

{-- cipherEncude is cipherDecode but shifted the other way --}
cipherEncode :: Int -> String -> String
cipherEncode _ [] = []
cipherEncode a xs = cipherDecode (-a) xs

{-- Shifts a whole string --}
shift :: Int -> String -> String
shift _ [] = []
shift a (x:xs) 
 | x == ' '   = x:(shift a xs)
 | otherwise = (shiftChar a x):(shift a xs)

{-- Shifts a single char to another char in the alphabet --}
shiftChar :: Int -> Char -> Char
{-- Check if you don't go out of bounds, 
if you do, adjust the values accordingly --}
shiftChar a b
 | b == ' ' = b 
 | ord b + a > 64 && ord b + a < 91 = chr (ord b + a)
 | ord b + a > 90 = chr (ord b + a - 26)
 | ord b + a < 65 = chr (ord b + a + 26)

 
cipherDecode :: Int -> String -> String
cipherDecode _ [] = []
cipherDecode a (x:xs) 
 | x == ' '  = [x] ++ cipherDecode a xs 
 | otherwise = [shiftChar a x] ++ cipherDecode a (shift a xs)

wrapper :: String -> String
wrapper line
  | cmd == "ENCODE"  = cipherEncode key txt
  | cmd == "DECODE"  = cipherDecode key txt
  where
    str  = dropWhile (not.isAlpha) line
    cmd  = takeWhile isAlpha str
    tail = dropWhile (not.isDigit) str
    key = read (takeWhile isDigit tail)::Int
    txt = dropWhile (not.isAlpha) (dropWhile isDigit tail)

main =  print . wrapper =<< getLine
