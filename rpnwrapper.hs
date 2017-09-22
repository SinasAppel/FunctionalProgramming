{- Insert here your own code. The type of rpnEval must be:

rpnEval :: String -> Integer

-}

import Data.Char

{- We have two numbers and an operator, so we apply the operator to
the numbers. We reversed the order of the numbers, because of the order 
they occur in the list. -}
evalExp :: Integer -> Integer -> Char -> Integer
evalExp y z x 
  | x == '+' = z + y
  | x == '-' = z - y
  | x == '*' = z * y
  | x == '/' = z `div` y
  
{- converts the input string into a list of elements, split by 
the spaces in the input. In practice, these will be either strings of
digits or individual operators -}
  
stringToList :: String -> String -> [String]
stringToList (' ' : xs) [] = stringToList xs [] 
{- When we find a space, we add the buffer String to the list of elements -}
stringToList (' ' : xs) (ys) = [ys] ++ stringToList xs [] 
{- When we find a non-space, we add it to the buffer String -}
stringToList (x : xs) (ys) = stringToList xs (ys ++ [x]) 
{- When we find nothing at all, we return the buffer String -}
stringToList [] [] = []
stringToList [] (ys) = [ys]

{- This function resolves the expression -}
resolveExpList :: [String] -> [Integer] -> Integer
{- When we have some input leftover and we also have at least two elements
in the buffer list, we arrive in this case -}
resolveExpList ( (x:xs) : ys) (a : b : as) 
  {- if the first Char of the first String is alphanumeric, it is an Integer, so we
  convert it to one. Then we add it to the front of the buffer list. -}
  | isAlphaNum x = resolveExpList ys ( (toInteger (read (x:xs) :: Int)) : a : b: as)
  {- otherwise the element is an operator, so we pass it to the appropriate function -}
  | otherwise = resolveExpList ys ( (evalExp a b x) : as)
-- No input left, so return the first element in the list.
resolveExpList [] (a : as) = a
-- No two elements have been found yet, so we have to read at least one more.
resolveExpList ( (x:xs) : ys) (a : as) =  resolveExpList ys ( (toInteger (read (x:xs) :: Int)) : a : as)
-- No elements have been found yet, so we have to read in more.
resolveExpList ( (x:xs) : ys) [] = resolveExpList ys [ (toInteger (read (x:xs) :: Int) ) ]

rpnEval :: String -> Integer
rpnEval s = resolveExpList (stringToList s []) []

main =  print . rpnEval =<< getLine
