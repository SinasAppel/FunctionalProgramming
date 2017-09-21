{- Insert here your own code. The type of rpnEval must be:

rpnEval :: String -> Integer

-}

evalExp :: Integer -> Integer -> Char -> Integer
evalExp y z x 
  | x == '+' = y + z
  | x == '-' = y - z
  | x == '*' = y * z
  | x == '/' = y / z

rpnEval2 :: String -> [Integer] -> Integer
rpnEval2 (x:xs) (y:z:ys)
  | isAlphaNum x = rpnEval2 xs ( (toInteger (digitToInt x) ) : y : z : ys)
  | x /= ' ' = rpnEval2 xs ( (evalExp y z x) : ys)
  | otherwise = rpnEval2 xs (y : z : ys)

rpnEval :: String -> Integer
rpnEval s = rpnEval2 s []

main =  print . rpnEval =<< getLine
