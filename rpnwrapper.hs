{- Insert here your own code. The type of rpnEval must be:

rpnEval :: String -> Integer

-}

import Data.Char

evalExp :: Integer -> Integer -> Char -> Integer
evalExp y z x 
  | x == '+' = z + y
  | x == '-' = z - y
  | x == '*' = z * y
  | x == '/' = z `div` y

rpnEval2 :: String -> [Integer] -> Integer
rpnEval2 (x:xs) (y:z:ys)
  | isAlphaNum x = rpnEval2 xs ( (toInteger (digitToInt x) ) : y : z : ys)
  | x /= ' ' = rpnEval2 xs ( (evalExp y z x) : ys)
  | otherwise = rpnEval2 xs (y : z : ys)
rpnEval2 [] (y:ys) = y
rpnEval2 (x:xs) (y:ys) 
  | isAlphaNum x = rpnEval2 xs ( (toInteger (digitToInt x) ) : y : ys)
  | otherwise = rpnEval2 xs ( y : ys )
rpnEval2 (x:xs) []
  | isAlphaNum x = rpnEval2 xs [ toInteger (digitToInt x) ]
  | otherwise = rpnEval2 xs []

rpnEval :: String -> Integer
rpnEval s = rpnEval2 s []

main =  print . rpnEval =<< getLine
